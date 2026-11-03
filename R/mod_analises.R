# ==============================================================================
# Chik RIDE | Aba "Análises"
#
#   - ranking de municipios (tabela, respeita o filtro de classificacao)
#   - classificacao final por ano (quanto se confirma, descarta ou fica
#     pendente - a qualidade do encerramento das fichas)
#   - achados da analise exploratoria, CALCULADOS da base no start (nao ha
#     numero escrito a mao: um novo ETL atualiza o texto)
# ==============================================================================

# ------------------------------------------------------------------ achados --
calcular_achados <- function() {
  d <- DADOS
  prov <- d[d$classe %in% PROVAVEIS, , drop = FALSE]
  conf <- d[d$classe %in% CONFIRMADOS, , drop = FALSE]
  mes <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  mes_ext <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto",
              "setembro", "outubro", "novembro", "dezembro")
  mes_da_se <- function(se) mes[as.integer(format(as.Date("2023-01-01") + (se - 1) * 7 + 3, "%m"))]
  pct <- function(x, dg = 0) fmt_pct(x, dg)
  var <- function(a, b) if (a > 0) sprintf("%s%s", if (b >= a) "+" else "−", pct(abs(b / a - 1))) else "n/a"

  # 1. sazonalidade: SE de pico por temporada (provaveis, temporadas completas)
  prov$temporada <- ano_sanitario(prov$ano, prov$semana)
  pico <- prov |>
    dplyr::filter(temporada > min(temporada), temporada < TEMPORADA_ATUAL) |>
    dplyr::group_by(temporada, semana) |>
    dplyr::summarise(n = sum(casos), .groups = "drop") |>
    dplyr::group_by(temporada) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  pk <- stats::median(pico$semana)
  fev_mai <- sum(prov$casos[prov$semana >= 5 & prov$semana <= 22]) / sum(prov$casos)

  # 2. epidemia de 2022-2023: Goias x DF, municipios lideres em incidencia
  epi <- prov[prov$ano %in% 2022:2023, , drop = FALSE]
  go_epi <- sum(epi$casos[epi$uf_ibge == 52]) / sum(epi$casos)
  pop_epi <- POP_MUN_ANO[POP_MUN_ANO$ano == 2022, ]
  top_epi <- epi |>
    dplyr::group_by(municipio_geocodigo) |>
    dplyr::summarise(n = sum(casos), .groups = "drop") |>
    dplyr::left_join(pop_epi, by = "municipio_geocodigo") |>
    dplyr::filter(n >= 50) |>
    dplyr::mutate(inc = n / pop * 1e5) |>
    dplyr::arrange(dplyr::desc(inc)) |>
    utils::head(3)
  top_epi$nome <- GEO_DICT$municipio_nome[match(top_epi$municipio_geocodigo, GEO_DICT$municipio_geocodigo)]
  df_epi <- sum(epi$casos[epi$municipio_geocodigo == 5300108])
  inc_df_epi <- df_epi / pop_epi$pop[pop_epi$municipio_geocodigo == 5300108] * 1e5

  # 3. confirmacao laboratorial e descarte
  c22 <- conf[conf$ano >= 2022, ]
  lab22 <- sum(c22$casos[c22$classe == "confirmado_lab"]) / sum(c22$casos)
  por_ano <- d |>
    dplyr::group_by(ano) |>
    dplyr::summarise(n = sum(casos), desc = sum(casos[classe == "descartado"]),
                     .groups = "drop")
  n2020 <- por_ano$n[por_ano$ano == 2020]; d2020 <- por_ano$desc[por_ano$ano == 2020]

  # 4. perfil: mulheres e idade entre confirmados
  fem <- sum(conf$feminino) / sum(conf$casos)
  adult <- sum(conf$casos[conf$faixa_etaria %in% c("30-44", "45-59")]) / sum(conf$casos)
  idos <- sum(conf$casos[conf$faixa_etaria == "60+"]) / sum(conf$casos)

  # 5. gravidade: hospitalizacao, fase cronica, obitos (confirmados encerrados)
  ce <- conf[conf$data_ini_se <= DATA_MAX - 8L * 7L, ]
  hosp <- sum(ce$hospitalizados) / sum(ce$casos)
  hosp60 <- sum(ce$hospitalizados[ce$faixa_etaria == "60+"]) / sum(ce$casos[ce$faixa_etaria == "60+"])
  cron <- sum(ce$cronicos) / sum(ce$casos)
  obit <- sum(conf$obitos)

  # 6. autoctonia (confirmados com o campo preenchido)
  aut <- sum(c22$autoctones); imp <- sum(c22$importados)

  # 7. ano corrente x anterior (mesmo periodo)
  sem_ult <- max(d$semana[d$ano == ANO_MAX])
  p_cor <- sum(prov$casos[prov$ano == ANO_MAX])
  p_ant <- sum(prov$casos[prov$ano == ANO_MAX - 1L & prov$semana <= sem_ult])
  c_cor <- sum(conf$casos[conf$ano == ANO_MAX])
  c_ant <- sum(conf$casos[conf$ano == ANO_MAX - 1L & conf$semana <= sem_ult])
  hosp_cor <- sum(prov$hospitalizados[prov$ano == ANO_MAX])
  hosp_ant <- sum(prov$hospitalizados[prov$ano == ANO_MAX - 1L & prov$semana <= sem_ult])

  # 8. pendencia: em investigacao nas ultimas 52 semanas
  u52 <- d[d$data_ini_se > DATA_MAX - 52L * 7L, ]
  inv52 <- sum(u52$casos[u52$classe == "em_investigacao"]) / sum(u52$casos)
  inv_hist <- d[d$data_ini_se <= DATA_MAX - 104L * 7L & d$ano >= 2019, ]
  inv_hist <- sum(inv_hist$casos[inv_hist$classe == "em_investigacao"]) / sum(inv_hist$casos)

  list(
    list(titulo = sprintf("Uma doença de verão-outono, com pico em %s", mes_ext[match(mes_da_se(pk), mes)]),
         texto = sprintf(
           "Nas temporadas completas, o pico de casos prováveis cai em mediana na SE %d (%s), e %s dos casos se concentram entre as SE 5 e 22 (fev–mai) - a estação do Aedes aegypti, o mesmo vetor da dengue. A subida começa em dezembro, por isso o canal endêmico usa o ano sanitário (SE 27 a 26), como o painel de dengue.",
           as.integer(pk), mes_da_se(pk), pct(fev_mai))),
    list(titulo = "A grande epidemia foi no Entorno goiano, não no DF",
         texto = sprintf(
           "Em 2022–2023, %s dos casos prováveis da RIDE foram de residentes em Goiás. As maiores incidências do período (municípios com 50+ casos) foram em %s. O DF teve %s casos prováveis no biênio (%s por 100 mil) - bem abaixo do Entorno.",
           pct(go_epi),
           paste(sprintf("%s (%s/100 mil)", top_epi$nome, fmt_int(top_epi$inc)), collapse = ", "),
           fmt_int(df_epi), fmt_int(inc_df_epi))),
    list(titulo = sprintf("%d frente a %d (até a SE %d)", ANO_MAX, ANO_MAX - 1L, sem_ult),
         texto = sprintf(
           "Casos prováveis %s (%s → %s), confirmados %s (%s → %s), hospitalizações %s (%s → %s). %s",
           var(p_ant, p_cor), fmt_int(p_ant), fmt_int(p_cor),
           var(c_ant, c_cor), fmt_int(c_ant), fmt_int(c_cor),
           var(hosp_ant, hosp_cor), fmt_int(hosp_ant), fmt_int(hosp_cor),
           if (hosp_cor > hosp_ant) "As hospitalizações sobem mais que os casos - vale olhar o perfil etário dos internados." else "")),
    list(titulo = "Quase toda confirmação é laboratorial",
         texto = sprintf(
           "Desde 2022, %s dos casos confirmados foram encerrados por critério laboratorial (sorologia IgM ou RT-PCR). Em 2020, com a covid-19, a RIDE notificou %s suspeitas de chikungunya e descartou %s (%s) - quase todas no DF. Um pico de notificação não é, por si, um pico de doença: o filtro \"Casos\" deixa ver as duas coisas.",
           pct(lab22), fmt_int(n2020), fmt_int(d2020), pct(d2020 / n2020))),
    list(titulo = "Mulheres adultas são a maioria dos casos",
         texto = sprintf(
           "%s dos confirmados são mulheres e %s têm entre 30 e 59 anos (%s têm 60+). O padrão (mais exposição domiciliar ao vetor e mais procura por atendimento) é o mesmo descrito para a dengue e importa para a fase crônica, que é mais comum em mulheres de meia-idade.",
           pct(fem), pct(adult), pct(idos))),
    list(titulo = "Pouco óbito, mas internação e cronificação",
         texto = sprintf(
           "Entre os confirmados com desfecho, %s foram hospitalizados (%s entre os de 60+). %s tiveram a fase crônica registrada na ficha - um piso, porque a ficha costuma ser encerrada antes dos 3 meses que definem a cronificação. Óbitos por chikungunya confirmados na série: %s.",
           pct(hosp, 1), pct(hosp60, 1), pct(cron, 1), fmt_int(obit))),
    list(titulo = "Transmissão local",
         texto = sprintf(
           "Desde 2022, entre os confirmados com o campo preenchido, %s foram autóctones (infecção no próprio município) e %s importados. A chikungunya está estabelecida na RIDE - não depende de casos vindos de fora.",
           pct(aut / (aut + imp)), pct(imp / (aut + imp)))),
    list(titulo = "O fim da curva é incompleto duas vezes",
         texto = sprintf(
           "Só %s das notificações de uma semana estão digitadas ao fim da semana seguinte e %s após 4 semanas - o nowcast corrige isso nas últimas %d semanas. Além da digitação, há o encerramento: %s das notificações das últimas 52 semanas ainda estão em investigação, contra %s nas fichas com mais de 2 anos. Os confirmados recentes vão crescer.",
           pct(NOWCAST_F$f[2]), pct(NOWCAST_F$f[5]), NOWCAST_MAX_K, pct(inv52), pct(inv_hist)))
  )
}

ACHADOS <- tryCatch(calcular_achados(), error = function(e) {
  warning("[Chik RIDE] Achados nao calculados: ", conditionMessage(e), call. = FALSE)
  list()
})

# ----------------------------------------------------------------------- UI --
mod_analises_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "bg-primary text-white fw-semibold",
                           shiny::textOutput(ns("titulo_ranking"), inline = TRUE)),
        bslib::card_body(DT::DTOutput(ns("ranking")))
      ),
      cartao_grafico(
        "Classificação final das notificações, por ano",
        echarts4r::echarts4rOutput(ns("classif"), height = "560px"),
        rodape = "Ano de início dos sintomas, recorte RIDE-DF inteiro. Casos prováveis = tudo menos descartados."
      )
    ),
    bslib::card(
      bslib::card_header(class = "bg-primary text-white fw-semibold",
                         "Achados da análise exploratória"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = "380px",
          !!!lapply(ACHADOS, function(a) shiny::div(
            class = "insight",
            shiny::tags$h6(a$titulo),
            shiny::tags$p(class = "small", a$texto)
          ))
        )
      )
    )
  )
}

mod_analises_server <- function(id, classe) {
  shiny::moduleServer(id, function(input, output, session) {

    output$titulo_ranking <- shiny::renderText(
      sprintf("Municípios — %s", rotulo_filtro_classe(classe())))

    output$ranking <- DT::renderDT({
      d <- filtrar(list(nivel = "base"), classes_do_filtro(classe()))
      pop <- POP_MUN_ANO |> dplyr::filter(ano == max(ano)) |> dplyr::select(-ano)
      t <- d |>
        dplyr::group_by(municipio_geocodigo) |>
        dplyr::summarise(
          c52  = sum(casos[data_ini_se > DATA_MAX - 52L * 7L]),
          h52  = sum(hospitalizados[data_ini_se > DATA_MAX - 52L * 7L]),
          u4   = sum(casos[data_ini_se > DATA_MAX - 4L * 7L]),
          a4   = sum(casos[data_ini_se <= DATA_MAX - 4L * 7L & data_ini_se > DATA_MAX - 8L * 7L]),
          tot  = sum(casos),
          .groups = "drop")
      GEO_DICT |>
        dplyr::select(municipio_geocodigo, municipio_nome, uf_sigla) |>
        dplyr::left_join(t, by = "municipio_geocodigo") |>
        dplyr::left_join(pop, by = "municipio_geocodigo") |>
        dplyr::mutate(dplyr::across(c(c52, h52, u4, a4, tot), ~ dplyr::coalesce(.x, 0))) |>
        dplyr::transmute(
          `Município` = municipio_nome, UF = uf_sigla,
          `Casos (52 sem.)` = c52,
          `Incid. /100 mil` = round(c52 / pop * 1e5, 1),
          `Hospitaliz. (52 sem.)` = h52,
          `Últimas 4 sem.` = u4,
          `4 sem. anteriores` = a4,
          `Desde 2015` = tot
        ) |>
        dplyr::arrange(dplyr::desc(`Incid. /100 mil`), dplyr::desc(`Desde 2015`)) |>
        DT::datatable(
          rownames = FALSE, options = list(pageLength = 12, dom = "ftp",
                                           language = list(search = "Buscar:",
                                                           paginate = list(previous = "Anterior", `next` = "Próxima"))),
          caption = "Últimas 4 semanas sem nowcast: compare com cautela (digitação incompleta)."
        ) |>
        DT::formatRound("Incid. /100 mil", digits = 1, mark = ".", dec.mark = ",") |>
        DT::formatStyle("Incid. /100 mil",
                        background = DT::styleColorBar(c(0, 100), TONS_DOENCA[["200"]]),
                        backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                        backgroundPosition = "center")
    })

    output$classif <- echarts4r::renderEcharts4r({
      larga <- DADOS |>
        dplyr::group_by(ano, classe) |>
        dplyr::summarise(casos = sum(casos), .groups = "drop") |>
        tidyr::pivot_wider(names_from = classe, values_from = casos, values_fill = 0)
      cls <- CLASSES$classe[CLASSES$classe %in% names(larga)]
      larga$x <- as.character(larga$ano)

      e <- larga |> echarts4r::e_charts(x)
      for (a in cls) e <- e |> echarts4r::e_bar_(a, name = ROTULO_CLASSE[[a]], stack = "c")
      e |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_color(unname(COR_CLASSE[cls])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_y_axis(inverse = TRUE) |>
        echarts4r::e_x_axis(name = "Notificações") |>
        echarts4r::e_grid(left = 50, right = 25, top = 10, bottom = 60) |>
        e_padrao()
    })
  })
}
