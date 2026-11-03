# ==============================================================================
# Chik RIDE | global.R
# Ambiente, dados (Parquets agregados do SINAN), dicionario geografico,
# classificacao final, ano sanitario, nowcast e helpers. Executado UMA vez,
# antes de ui.R e server.R.
#
# Mesma arquitetura do painel de gripe: o ETL entrega a base agregada (semana
# x municipio x classificacao x faixa etaria, ~7 mil linhas) e ela fica
# inteira em memoria - todo filtro e um dplyr sobre um data.frame.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(echarts4r)
library(DT)

if (dir.exists("app_cache") && file.access("app_cache", 2) == 0) {
  options(sass.cache = sass::sass_file_cache(file.path("app_cache", "sass")))
}

# ------------------------------------------------------------ fonte de dados --
PARQUET_CHIK   <- Sys.getenv("CHIK_PARQUET", file.path("data", "chik_ride_df.parquet"))
PARQUET_ATRASO <- Sys.getenv("CHIK_PARQUET_ATRASO", file.path("data", "chik_ride_atraso.parquet"))

for (.p in c(PARQUET_CHIK, PARQUET_ATRASO)) {
  if (!file.exists(.p)) {
    stop("Parquet nao encontrado em '", .p, "'. Rode `Rscript etl_chik_ride.R` ",
         "(precisa do .Renviron com PG_*).", call. = FALSE)
  }
}
rm(.p)

MEDIDAS <- c("casos", "obitos", "hospitalizados", "cronicos", "gestantes",
             "feminino", "autoctones", "importados")

DADOS <- arrow::read_parquet(PARQUET_CHIK) |>
  as.data.frame() |>
  dplyr::mutate(
    data_ini_se = as.Date(data_ini_se),
    se          = as.integer(se),
    ano         = se %/% 100L,
    semana      = se %%  100L,
    municipio_geocodigo = as.integer(municipio_geocodigo),
    uf_ibge     = as.integer(uf_ibge),
    dplyr::across(dplyr::all_of(MEDIDAS), ~ dplyr::coalesce(as.numeric(.x), 0))
  )

ATRASO <- arrow::read_parquet(PARQUET_ATRASO) |>
  as.data.frame() |>
  dplyr::mutate(data_ini_se = as.Date(data_ini_se))

ROTULO_BASE <- Sys.getenv("CHIK_ROTULO_BASE", "RIDE-DF")

message(sprintf("[Chik RIDE] Fonte: %s (%s notificações).",
                PARQUET_CHIK, format(sum(DADOS$casos), big.mark = ".", decimal.mark = ",")))

# ------------------------------------------------- dicionario geografico -----
UF_LOOKUP <- data.frame(
  uf_ibge  = c(31L, 52L, 53L),
  uf_sigla = c("MG", "GO", "DF"),
  uf_nome  = c("Minas Gerais", "Goiás", "Distrito Federal"),
  stringsAsFactors = FALSE
)

# Populacao por municipio e ano. A pop so chega nas linhas com notificacao, e
# municipio pequeno passa anos sem nenhuma: a grade e completada com o ano
# vizinho mais proximo (senao a incidencia do recorte sai NA nesses anos).
POP_MUN_ANO <- DADOS |>
  dplyr::distinct(municipio_geocodigo, ano, pop) |>
  dplyr::group_by(municipio_geocodigo, ano) |>
  dplyr::summarise(pop = max(pop, na.rm = TRUE), .groups = "drop") |>
  tidyr::complete(municipio_geocodigo, ano = seq(min(DADOS$ano), max(DADOS$ano))) |>
  dplyr::group_by(municipio_geocodigo) |>
  tidyr::fill(pop, .direction = "downup") |>
  dplyr::ungroup()

GEO_DICT <- DADOS |>
  dplyr::distinct(uf_ibge, municipio_geocodigo, municipio_nome) |>
  dplyr::left_join(UF_LOOKUP, by = "uf_ibge") |>
  dplyr::arrange(uf_nome, municipio_nome)

UF_DISPONIVEIS <- GEO_DICT |> dplyr::distinct(uf_ibge, uf_sigla, uf_nome) |> dplyr::arrange(uf_nome)

# ------------------------------------------------- cobertura temporal --------
DATA_MIN <- min(DADOS$data_ini_se)
DATA_MAX <- max(DADOS$data_ini_se)
ANO_MIN  <- min(DADOS$ano)
ANO_MAX  <- max(DADOS$ano)

# ------------------------------------------------- classificacao final -------
# Ordem = ordem de empilhamento nos graficos (de baixo para cima). Confirmados
# na cor do painel (verde-agua), inconclusivo em ocre, em investigacao e
# descartado em neutros - o que NAO e caso confirmado nunca compete
# visualmente com o que e.
CLASSES <- data.frame(
  classe = c("confirmado_lab", "confirmado_clin", "inconclusivo", "em_investigacao", "descartado"),
  rotulo = c("Confirmado (laboratório)", "Confirmado (clínico-epid.)", "Inconclusivo",
             "Em investigação", "Descartado"),
  cor    = c("#13665D", "#5FB3A1", "#C9A043", "#B8C4CC", "#E3E8EC"),
  stringsAsFactors = FALSE
)
COR_CLASSE    <- stats::setNames(CLASSES$cor, CLASSES$classe)
ROTULO_CLASSE <- stats::setNames(CLASSES$rotulo, CLASSES$classe)

CONFIRMADOS <- c("confirmado_lab", "confirmado_clin")
PROVAVEIS   <- setdiff(CLASSES$classe, "descartado")

# Opcoes do seletor "Casos" (rotulo -> chave). Padrao: casos PROVAVEIS,
# a definicao do Ministerio da Saude (notificados menos descartados) - e o
# numero dos boletins e do InfoDengue.
FILTROS_CLASSE <- c(
  "Casos prováveis"                  = "provaveis",
  "Confirmados"                      = "confirmados",
  "Confirmados por laboratório"      = "confirmado_lab",
  "Em investigação"                  = "em_investigacao",
  "Todas as notificações"            = "todas",
  "Descartados"                      = "descartado"
)

classes_do_filtro <- function(chave) {
  switch(
    chave,
    provaveis   = PROVAVEIS,
    confirmados = CONFIRMADOS,
    todas       = CLASSES$classe,
    chave
  )
}

rotulo_filtro_classe <- function(chave) names(FILTROS_CLASSE)[FILTROS_CLASSE == chave]

FAIXAS <- c("0-4", "5-14", "15-29", "30-44", "45-59", "60+")
ROTULO_FAIXA <- c(`0-4` = "0–4", `5-14` = "5–14", `15-29` = "15–29", `30-44` = "30–44",
                  `45-59` = "45–59", `60+` = "60+", ign = "Ignorada")

# ------------------------------------------------- ano sanitario (jul-jun) ---
# Mesmo recorte do painel de dengue: a temporada comeca na SE 27 e termina na
# SE 26 do ano seguinte. A chikungunya da RIDE-DF tem pico entre fevereiro e
# abril e a subida comeca em novembro/dezembro - ancorar o canal no ano civil
# partiria a curva de subida ao meio.
SEMANA_CORTE_SANITARIA <- 27L

ano_sanitario <- function(ano, semana) ano - as.integer(semana < SEMANA_CORTE_SANITARIA)

rotulo_ano_sanitario <- function(ano_san) sprintf("%d/%d", ano_san, ano_san + 1L)

NIVEIS_SEMANA_SANITARIA <- c(seq(SEMANA_CORTE_SANITARIA, 52L), seq(1L, SEMANA_CORTE_SANITARIA - 1L))

TEMPORADAS <- sort(unique(ano_sanitario(DADOS$ano, DADOS$semana)))
TEMPORADA_ATUAL <- max(TEMPORADAS)

# Temporada que o canal abre por padrao: a atual, exceto nas primeiras 20
# semanas dela (jul-nov, entressafra), quando a linha seria quase so zeros -
# ai abre a temporada anterior, que acabou de fechar.
TEMPORADA_PADRAO <- local({
  sem <- DADOS$semana[DADOS$data_ini_se == DATA_MAX][1]
  if (sem >= SEMANA_CORTE_SANITARIA && sem < SEMANA_CORTE_SANITARIA + 20L) TEMPORADA_ATUAL - 1L
  else TEMPORADA_ATUAL
})

# ------------------------------------------------------------------ paleta --
# Mesma interface dos paineis de dengue e gripe (azul Cenarios, Poppins/DM
# Sans). A rampa de magnitude e verde-agua, cor desta doenca no painel -
# ColorBrewer BuGn, sequencial e segura para daltonismo.
TONS_AZUL <- c(
  `50`  = "#ECF7FE", `100` = "#CFE9F9", `200` = "#A8D5F1", `300` = "#7ABEE6",
  `400` = "#4FA4D3", `500` = "#3891C0", `600` = "#0E7BA8", `700` = "#00638C",
  `800` = "#004565", `900` = "#00324C"
)

TONS_DOENCA <- c(
  `50`  = "#EEF8F5", `100` = "#D3EEE6", `200` = "#A9DCCD", `300` = "#7CC7B2",
  `400` = "#4FAE96", `500` = "#2E937D", `600` = "#13665D", `700` = "#0E554D",
  `800` = "#0A433D", `900` = "#06302C"
)

PALETA <- c(
  fundo       = "#F2F6F9",
  superficie  = "#FFFFFF",
  linha       = "#D8E3EA",
  tinta       = "#12222B",
  texto       = "#4A4A4A",
  sub         = "#7A8B95",
  neutro      = "#B8C4CC",
  azul        = TONS_AZUL[["500"]],
  azul_acao   = TONS_AZUL[["600"]],
  azul_titulo = TONS_AZUL[["700"]],
  doenca      = TONS_DOENCA[["600"]],
  doenca_forte = TONS_DOENCA[["800"]],
  nowcast     = "#E03131",
  ok          = "#17895B",
  atencao     = "#DCA744",
  grave       = "#C65D26",
  critico     = "#8F2639"
)

ESCALA_MAPA <- c("#EDF8FB", "#CCECE6", "#99D8C9", "#66C2A4", "#2CA25F", "#006D2C")

FONTE_GRAFICO <- "'DM Sans', system-ui, sans-serif"

cor_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# ------------------------------------------------------------------- malhas --
# GeoJSON do IBGE (mesmos arquivos do painel de dengue) recortados aos
# municipios da base. Parse uma vez no start.
GEO_DIR <- file.path("data", "geo")

MALHA_RIDE <- local({
  codigos <- as.character(GEO_DICT$municipio_geocodigo)
  feicoes <- list()
  for (uf in sort(unique(GEO_DICT$uf_ibge))) {
    arq <- file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf))
    if (!file.exists(arq)) next
    js <- jsonlite::fromJSON(arq, simplifyVector = FALSE)
    feicoes <- c(feicoes, Filter(function(f) as.character(f$properties$codarea) %in% codigos,
                                 js$features))
  }
  list(type = "FeatureCollection", features = feicoes)
})

message(sprintf("[Chik RIDE] Malha: %d de %d municípios.",
                length(MALHA_RIDE$features), nrow(GEO_DICT)))

# Contorno das UFs e da RIDE (gerado por prep/gerar_contorno.R), juntado ao
# FIM da malha do mapa: as feicoes extras sao desenhadas por ultimo, por cima
# dos municipios, e por serem parte do mesmo mapa acompanham zoom e arraste.
# Opcional: sem o arquivo, o mapa sai so com as bordas cinza dos municipios.
CONTORNO_RIDE <- local({
  arq <- file.path(GEO_DIR, "contorno_ride.geojson")
  if (!file.exists(arq)) return(list())
  Filter(function(f) is.character(f$properties$codarea),
         jsonlite::fromJSON(arq, simplifyVector = FALSE)$features)
})
CODIGOS_CONTORNO <- vapply(CONTORNO_RIDE, function(f) f$properties$codarea, "")
MALHA_MAPA <- list(type = "FeatureCollection",
                   features = c(MALHA_RIDE$features, CONTORNO_RIDE))

# ------------------------------------------------------------------ nowcast --
# O SINAN traz a data de digitacao de cada ficha, entao o atraso de
# notificacao e observavel. Metodo: "chain ladder" empirico, igual ao do
# painel de gripe.
#
#   F(k) = fracao das notificacoes de uma semana ja digitadas k semanas depois
#          do inicio dela, medida nas semanas maduras (>= 26 semanas) das
#          ultimas 156 semanas maduras (3 anos: a chikungunya tem poucas
#          notificacoes por semana fora do pico, 2 anos dariam pouca base).
#   estimado(semana) = observado / F(k), k = idade da semana na data do
#          arquivo.
#
# Intervalo: quantis 10% e 90% de F(k) entre as semanas de treino com pelo
# menos 10 notificacoes. O mesmo F(k) vale para qualquer recorte e
# classificacao. Ressalva: o nowcast corrige a DIGITACAO; a classificacao
# final (que transforma "em investigacao" em confirmado ou descartado) leva
# mais tempo e nao e modelada.
NOWCAST_MAX_K <- 8L   # semanas mais recentes que recebem correcao
# A semana corrente (k = 0) fica sem estimativa: so ~16% das fichas dela
# estao digitadas e, com 2-10 notificacoes por semana, dividir por F(0)
# produz um salto de 5-10x que achata o grafico. Ela aparece so como
# observado (e os KPIs ja a ignoram).
NOWCAST_MIN_K <- 1L

DATA_REF_ARQUIVO <- max(ATRASO$data_ini_se + 7L * ATRASO$atraso_sem)

NOWCAST_F <- local({
  maduras <- ATRASO |>
    dplyr::filter(data_ini_se <= DATA_REF_ARQUIVO - 26L * 7L) |>
    dplyr::filter(data_ini_se >  max(data_ini_se) - 156L * 7L)

  por_semana <- maduras |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      total = sum(casos),
      cum   = list(vapply(0:NOWCAST_MAX_K, function(k) sum(casos[atraso_sem <= k]), numeric(1))),
      .groups = "drop"
    ) |>
    dplyr::filter(total >= 10)

  mat <- do.call(rbind, por_semana$cum) / por_semana$total
  data.frame(
    k     = 0:NOWCAST_MAX_K,
    f     = colSums(do.call(rbind, por_semana$cum)) / sum(por_semana$total),
    f_q10 = apply(mat, 2, stats::quantile, 0.10),
    f_q90 = apply(mat, 2, stats::quantile, 0.90)
  )
})

message(sprintf("[Chik RIDE] Nowcast: arquivo ate %s; F(1)=%.0f%%, F(4)=%.0f%%.",
                format(DATA_REF_ARQUIVO, "%d/%m/%Y"),
                100 * NOWCAST_F$f[2], 100 * NOWCAST_F$f[5]))

#' Aplica o nowcast a uma serie semanal (colunas data_ini_se, casos).
#' Acrescenta casos_est, casos_est_min, casos_est_max (NA fora da janela).
aplicar_nowcast <- function(d) {
  k <- as.integer((DATA_REF_ARQUIVO - d$data_ini_se) / 7)
  i <- match(k, NOWCAST_F$k)
  i[k < NOWCAST_MIN_K] <- NA_integer_
  d$casos_est     <- ifelse(is.na(i), NA_real_, d$casos / pmax(NOWCAST_F$f[i], 0.05))
  d$casos_est_min <- ifelse(is.na(i), NA_real_, d$casos / pmax(NOWCAST_F$f_q90[i], 0.05))
  d$casos_est_max <- ifelse(is.na(i), NA_real_, d$casos / pmax(NOWCAST_F$f_q10[i], 0.05))
  d
}

# --------------------------------------------------------- Rt (InfoDengue) --
# Rt por municipio e semana estimado pelo InfoDengue, o mesmo do painel de
# dengue. Opcional: sem o Parquet (ETL antigo) o card de Rt avisa e o resto
# do painel funciona.
PARQUET_RT <- Sys.getenv("CHIK_PARQUET_RT", file.path("data", "chik_ride_rt.parquet"))
RT_FONTE   <- "\"Info_Dengue\".chikungunya"
RT_PERIODO_PADRAO <- "104"

RT_INFODENGUE <- if (file.exists(PARQUET_RT)) {
  arrow::read_parquet(PARQUET_RT) |>
    as.data.frame() |>
    dplyr::mutate(data_ini_se = as.Date(data_ini_se),
                  municipio_geocodigo = as.integer(municipio_geocodigo),
                  casos = as.numeric(casos), rt = as.numeric(rt))
} else NULL

#' Serie semanal de Rt do recorte, no formato do painel de dengue.
#'
#' `rt` e estimado POR MUNICIPIO e nao e aditivo entre areas. No municipio,
#' o valor direto; acima disso, a media ponderada pelos casos de cada
#' municipio (aproximacao) e o numero de municipios com Rt > 1 na semana
#' (bem definido em qualquer nivel - serve de contraprova).
serie_rt <- function(geo) {
  if (is.null(RT_INFODENGUE)) return(NULL)
  mun <- switch(
    geo$nivel,
    base      = GEO_DICT$municipio_geocodigo,
    uf        = GEO_DICT$municipio_geocodigo[GEO_DICT$uf_ibge == geo$uf_ibge],
    municipio = geo$geocodigo
  )
  d <- RT_INFODENGUE[RT_INFODENGUE$municipio_geocodigo %in% mun, , drop = FALSE] |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      # rt_num e rt_peso ANTES de `casos`: o summarise e sequencial e, depois
      # de casos = sum(casos), `casos` passaria a ser o total da semana.
      rt_num    = sum(rt * casos, na.rm = TRUE),
      rt_peso   = sum(casos[!is.na(rt)], na.rm = TRUE),
      casos     = sum(casos, na.rm = TRUE),
      rt_direto = suppressWarnings(max(rt, na.rm = TRUE)),
      n_mun     = dplyr::n_distinct(municipio_geocodigo),
      n_mun_rt1 = sum(rt > 1, na.rm = TRUE),
      .groups   = "drop"
    )
  d$rt <- if (identical(geo$nivel, "municipio")) {
    ifelse(is.finite(d$rt_direto), d$rt_direto, NA_real_)
  } else {
    ifelse(d$rt_peso > 0, d$rt_num / d$rt_peso, NA_real_)
  }
  d
}

# ----------------------------------------------------------------- helpers ---

#' Filtra a base pelo recorte geografico e pelo conjunto de classificacoes.
filtrar <- function(geo, classes = CLASSES$classe) {
  d <- DADOS[DADOS$classe %in% classes, , drop = FALSE]
  switch(
    geo$nivel,
    base      = d,
    uf        = d[d$uf_ibge == geo$uf_ibge, , drop = FALSE],
    municipio = d[d$municipio_geocodigo == geo$geocodigo, , drop = FALSE],
    stop("Nivel geografico desconhecido: ", geo$nivel, call. = FALSE)
  )
}

#' Populacao do recorte por ano (soma dos municipios).
pop_recorte <- function(geo) {
  mun <- switch(
    geo$nivel,
    base      = GEO_DICT$municipio_geocodigo,
    uf        = GEO_DICT$municipio_geocodigo[GEO_DICT$uf_ibge == geo$uf_ibge],
    municipio = geo$geocodigo
  )
  POP_MUN_ANO |>
    dplyr::filter(municipio_geocodigo %in% mun) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
}

#' SE (AAAASS) de um domingo de inicio de semana - calendario do MS.
se_de_domingo <- function(dd) {
  ano <- as.integer(format(dd + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  ano * 100L + as.integer((dd - (jan4 - as.integer(format(jan4, "%w")))) / 7) + 1L
}

#' Grade completa de semanas (sem buraco quando nao ha caso - comum aqui:
#' fora do pico muitas semanas tem zero notificacao).
SEMANAS <- data.frame(data_ini_se = seq(DATA_MIN, DATA_MAX, by = 7))
SEMANAS$se <- se_de_domingo(SEMANAS$data_ini_se)

#' Serie semanal (total do recorte e classificacoes), com incidencia e nowcast.
serie_semanal <- function(geo, classes) {
  d <- filtrar(geo, classes) |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(casos = sum(casos), obitos = sum(obitos),
                     hospitalizados = sum(hospitalizados), .groups = "drop")
  SEMANAS |>
    dplyr::left_join(d, by = "data_ini_se") |>
    dplyr::mutate(dplyr::across(c(casos, obitos, hospitalizados), ~ dplyr::coalesce(.x, 0)),
                  ano = se %/% 100L, semana = se %% 100L) |>
    dplyr::left_join(pop_recorte(geo), by = "ano") |>
    dplyr::mutate(incidencia = casos / pop * 1e5) |>
    aplicar_nowcast()
}

#' Expressao JS que formata "AAAA-MM-DD" como "dd/mm/aaaa" (sem new Date,
#' que interpreta como UTC e volta um dia em UTC-3).
JS_DATA_BR <- "(function(s){var p=String(s).split('-');return p[2]+'/'+p[1]+'/'+p[0];})"

fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x)) || !is.finite(x)) return("-")
  paste0(fmt_num(100 * x, digits), "%")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --------------------------------------------------------- modulos do app ----
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)
