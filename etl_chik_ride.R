# ==============================================================================
# Chik RIDE | etl_chik_ride.R
# Extrai do PostgreSQL (public.sinan_chik, microdado SINAN de chikungunya, um
# arquivo CHIKBRaa.dbc por ano) as notificacoes de residentes nos 34
# municipios da RIDE-DF e grava dois Parquets JA AGREGADOS, unica fonte de
# dados lida pelo app:
#
#   data/chik_ride_df.parquet     semana x municipio x classificacao x faixa
#                                 etaria -> notificacoes, obitos,
#                                 hospitalizados, forma cronica, gestantes...
#   data/chik_ride_atraso.parquet semana de sintomas x atraso de digitacao
#                                 (semanas) -> notificacoes. Base do nowcast.
#   data/chik_ride_rt.parquet     semana x municipio -> Rt e casos do
#                                 InfoDengue ("Info_Dengue".chikungunya)
#
# Nenhuma linha de microdado sai do banco: toda a agregacao roda no Postgres.
#
# Uso (working directory = raiz do projeto):  Rscript etl_chik_ride.R
#
# Por que public.sinan_chik e nao silver.chikungunya: e o mesmo microdado do
# SINAN, com os nomes de coluna originais (classi_fin, id_mn_resi, ...), e o
# usuario do painel tem SELECT nela (no silver nao). Traz dt_digita, o que
# permite estimar o atraso de digitacao (nowcast) - o InfoDengue nao oferece.
#
# Particularidades que o SQL abaixo trata:
#   - Todas as colunas sao texto -> split_part(x, '.', 1) por seguranca (no
#     SIVEP o mesmo codigo vem como "1" ou "1.0"; aqui vem limpo, mas o custo
#     e zero).
#   - Municipio com 6 digitos. No DF quase tudo vem como 530010; os poucos
#     539xxx (codigos de RA/ignorado) tambem viram o municipio 5300108.
#   - Idade no formato SINAN: 1o digito = unidade (1 hora, 2 dia, 3 mes,
#     4 ano), demais = valor. 4035 = 35 anos.
#   - dt_digita tem lixo (anos 2061, 2067...): o atraso e limitado a 0-52
#     semanas.
# ==============================================================================

library(DBI)
library(RPostgres)
library(arrow)
library(dplyr)

if (file.exists(".Renviron")) readRenviron(".Renviron")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS"),  # credenciais no .Renviron (ver .Renviron.example)
  bigint   = "numeric"
)

# Data minima de inicio de sintomas. A chikungunya chega ao Brasil em 2014; a
# RIDE tem 15 notificacoes naquele ano - a serie comeca em 2015.
DATA_MIN <- Sys.getenv("CHIK_DATA_MIN", "2015-01-04")

# Codigos IBGE oficiais da RIDE-DF (LC 163/2018) - 34 municipios. Mesma lista
# do etl_ride.R do painel de dengue.
ride_codes_oficiais <- c(
  5300108,                                   # Distrito Federal (1)
  3104502, 3109303, 3109451, 3170404,        # Minas Gerais (4)
  5200100, 5200175, 5200258, 5200308, 5200605, 5200803, 5203203, 5204003,
  5205307, 5205497, 5205513, 5205802, 5206206, 5207907, 5208004, 5208608,
  5212501, 5213053, 5214606, 5215231, 5215603, 5217302, 5217609, 5219753,
  5220009, 5220686, 5221858, 5222203, 5222302  # Goias (29)
)
mun6 <- substr(ride_codes_oficiais, 1, 6)
filtro_mun <- sprintf(
  "(id_mn_resi IN (%s) OR id_mn_resi LIKE '53%%')",
  paste0("'", mun6, "'", collapse = ", ")
)

# ------------------------------------------------------------- base comum --
# Classificacao final (classi_fin) da ficha de dengue/chikungunya do SINAN:
#   13 = chikungunya  -> confirmado_lab (criterio 1) ou confirmado_clin
#                        (criterio 2 clinico-epidemiologico, ou vazio/3)
#    5 = descartado
#    8 = inconclusivo
#    vazio = em investigacao (caso ainda nao encerrado)
#    1/2 = codigos antigos de dengue (2014-2016, ~700 fichas): a ficha nao
#          foi encerrada como chikungunya nem descartada -> inconclusivo, o
#          que mantem a contagem de provaveis igual a do InfoDengue.
# Caso PROVAVEL (definicao do Ministerio da Saude) = notificado - descartado.
sql_base <- sprintf("
WITH base AS (
  -- COALESCE: campo vazio vira '', senao (NULL = '1')::int da NULL e a
  -- soma de um grupo so com campos vazios sai NULL em vez de 0.
  SELECT
    substr(dt_sin_pri, 1, 10)::date                       AS dt_sin,
    CASE WHEN dt_digita ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}'
         THEN substr(dt_digita, 1, 10)::date END          AS dt_dig,
    id_mn_resi,
    COALESCE(split_part(classi_fin, '.', 1), '')          AS cf,
    COALESCE(split_part(criterio, '.', 1), '')            AS crit,
    COALESCE(split_part(evolucao, '.', 1), '')            AS evol,
    COALESCE(split_part(hospitaliz, '.', 1), '')          AS hosp,
    COALESCE(split_part(clinc_chik, '.', 1), '')          AS fase,
    COALESCE(split_part(tpautocto, '.', 1), '')           AS autoc,
    COALESCE(split_part(cs_gestant, '.', 1), '')          AS gest,
    COALESCE(cs_sexo, '')                                 AS sexo,
    NULLIF(split_part(nu_idade_n, '.', 1), '')            AS idade_txt
  FROM public.sinan_chik
  WHERE %s
    AND dt_sin_pri ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}'
    AND substr(dt_sin_pri, 1, 10) >= '%s'
),
classif AS (
  SELECT
    dt_sin - EXTRACT(DOW FROM dt_sin)::int                AS data_ini_se,
    dt_dig,
    CASE WHEN id_mn_resi LIKE '53%%' THEN '530010' ELSE id_mn_resi END AS mun6,
    CASE
      WHEN cf = '13' AND crit = '1' THEN 'confirmado_lab'
      WHEN cf = '13'                THEN 'confirmado_clin'
      WHEN cf = '5'                 THEN 'descartado'
      WHEN cf IN ('8', '1', '2')    THEN 'inconclusivo'
      ELSE 'em_investigacao'
    END                                                   AS classe,
    CASE
      WHEN idade_txt IS NULL OR idade_txt !~ '^[1-4][0-9]{3}$' THEN 'ign'
      WHEN left(idade_txt, 1) <> '4'           THEN '0-4'
      WHEN substr(idade_txt, 2)::int < 5       THEN '0-4'
      WHEN substr(idade_txt, 2)::int < 15      THEN '5-14'
      WHEN substr(idade_txt, 2)::int < 30      THEN '15-29'
      WHEN substr(idade_txt, 2)::int < 45      THEN '30-44'
      WHEN substr(idade_txt, 2)::int < 60      THEN '45-59'
      ELSE '60+'
    END                                                   AS faixa_etaria,
    (evol = '2')::int                                     AS obito,
    (hosp = '1')::int                                     AS hospitalizado,
    (fase = '2')::int                                     AS cronico,
    (gest IN ('1', '2', '3', '4'))::int                   AS gestante,
    (sexo = 'F')::int                                     AS feminino,
    (autoc = '1')::int                                    AS autoctone,
    (autoc = '2')::int                                    AS importado
  FROM base
)
", filtro_mun, DATA_MIN)

# ------------------------------------------------------ 1. serie agregada --
message("[ETL Chik] Agregando notificacoes da RIDE-DF no Postgres...")
t0 <- Sys.time()
agg <- DBI::dbGetQuery(con, paste0(sql_base, "
SELECT data_ini_se, mun6, classe, faixa_etaria,
       count(*)            AS casos,
       sum(obito)          AS obitos,
       sum(hospitalizado)  AS hospitalizados,
       sum(cronico)        AS cronicos,
       sum(gestante)       AS gestantes,
       sum(feminino)       AS feminino,
       sum(autoctone)      AS autoctones,
       sum(importado)      AS importados
FROM classif
GROUP BY data_ini_se, mun6, classe, faixa_etaria
"))
message(sprintf("[ETL Chik] %d linhas agregadas (%s notificacoes) em %.1fs.",
                nrow(agg), format(sum(agg$casos), big.mark = ".", decimal.mark = ","),
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# ------------------------------------------------- 2. triangulo de atraso --
# Notificacoes por semana de sintomas x atraso ate a digitacao no SINAN
# (semanas). So o total da RIDE-DF e todas as classificacoes: o atraso e do
# sistema de notificacao, e a ficha e digitada antes de ser classificada.
message("[ETL Chik] Montando triangulo de atraso...")
atraso <- DBI::dbGetQuery(con, paste0(sql_base, "
SELECT data_ini_se,
       LEAST(GREATEST(floor((dt_dig - data_ini_se) / 7.0)::int, 0), 52) AS atraso_sem,
       count(*) AS casos
FROM classif
WHERE dt_dig IS NOT NULL AND dt_dig <= current_date
GROUP BY 1, 2
"))

# -------------------------------------------- 3. populacao (InfoDengue) --
# O SINAN nao traz populacao. A mesma estimativa anual do IBGE que o painel de
# dengue usa ja esta no banco, em "Info_Dengue".dengue - um valor por
# municipio e ano. Anos sem estimativa repetem o ultimo ano disponivel.
message("[ETL Chik] Lendo populacao de Info_Dengue...")
pop <- DBI::dbGetQuery(con, sprintf("
SELECT municipio_geocodigo, municipio_nome, uf_ibge,
       EXTRACT(YEAR FROM data_ini_se)::int AS ano, max(pop) AS pop
FROM \"Info_Dengue\".dengue
WHERE municipio_geocodigo IN (%s)
GROUP BY 1, 2, 3, 4", paste(ride_codes_oficiais, collapse = ", ")))

# ------------------------------------------------------- Rt (InfoDengue) --
# Numero de reproducao efetivo estimado pelo InfoDengue, por municipio e
# semana - o mesmo `rt` que o painel de dengue mostra. Os casos vao junto
# porque sao o peso da media nos recortes acima do municipio.
message("[ETL Chik] Lendo Rt de Info_Dengue.chikungunya...")
rt <- DBI::dbGetQuery(con, sprintf("
SELECT data_ini_se, municipio_geocodigo::int AS municipio_geocodigo,
       casos, rt, p_rt1
FROM \"Info_Dengue\".chikungunya
WHERE municipio_geocodigo IN (%s) AND data_ini_se >= '%s'
ORDER BY 1, 2", paste(ride_codes_oficiais, collapse = ", "), DATA_MIN)) |>
  mutate(data_ini_se = as.Date(data_ini_se), casos = as.integer(casos),
         rt = as.numeric(rt), p_rt1 = as.numeric(p_rt1))

# ----------------------------------------------- semana epidemiologica (MS) --
#' Semana epidemiologica a partir do domingo que a inicia.
#' A SE 1 de um ano e a semana (dom-sab) que contem o dia 4 de janeiro, ou
#' seja, o ano epidemiologico de uma semana e o ano da sua quarta-feira.
se_de_domingo <- function(domingo) {
  ano <- as.integer(format(domingo + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  inicio <- jan4 - as.integer(format(jan4, "%w"))
  semana <- as.integer((domingo - inicio) / 7) + 1L
  ano * 100L + semana
}

medidas <- c("casos", "obitos", "hospitalizados", "cronicos", "gestantes",
             "feminino", "autoctones", "importados")

agg <- agg |>
  mutate(
    data_ini_se = as.Date(data_ini_se),
    se          = se_de_domingo(data_ini_se),
    across(all_of(medidas), as.integer)
  )
agg$municipio_geocodigo <- ride_codes_oficiais[match(agg$mun6, mun6)]

pop_full <- pop |>
  mutate(municipio_geocodigo = as.integer(municipio_geocodigo)) |>
  group_by(municipio_geocodigo) |>
  tidyr::complete(ano = seq(min(ano), max(as.integer(format(max(agg$data_ini_se), "%Y")), max(ano)))) |>
  tidyr::fill(pop, municipio_nome, uf_ibge, .direction = "downup") |>
  ungroup()

agg <- agg |>
  mutate(ano_pop = as.integer(format(data_ini_se + 3L, "%Y"))) |>
  left_join(pop_full, by = c("municipio_geocodigo", "ano_pop" = "ano")) |>
  transmute(
    data_ini_se, se,
    municipio_geocodigo = as.integer(municipio_geocodigo),
    municipio_nome, uf_ibge = as.integer(uf_ibge),
    classe, faixa_etaria,
    across(all_of(medidas)),
    pop = as.numeric(pop)
  ) |>
  arrange(data_ini_se, municipio_geocodigo, classe, faixa_etaria)

sem_pop <- unique(agg$municipio_geocodigo[is.na(agg$pop)])
if (length(sem_pop)) warning("[ETL Chik] Municipios sem populacao: ", paste(sem_pop, collapse = ", "))

atraso <- atraso |>
  mutate(data_ini_se = as.Date(data_ini_se),
         se = se_de_domingo(data_ini_se),
         atraso_sem = as.integer(atraso_sem),
         casos = as.integer(casos)) |>
  arrange(data_ini_se, atraso_sem)

DBI::dbDisconnect(con)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(agg, "data/chik_ride_df.parquet")
arrow::write_parquet(atraso, "data/chik_ride_atraso.parquet")
arrow::write_parquet(rt, "data/chik_ride_rt.parquet")

message(sprintf(
  "[ETL Chik] OK: %s notificacoes (%s provaveis), %s a %s, %d municipios. Parquets em data/.",
  format(sum(agg$casos), big.mark = ".", decimal.mark = ","),
  format(sum(agg$casos[agg$classe != "descartado"]), big.mark = ".", decimal.mark = ","),
  format(min(agg$data_ini_se), "%d/%m/%Y"), format(max(agg$data_ini_se), "%d/%m/%Y"),
  dplyr::n_distinct(agg$municipio_geocodigo)
))
