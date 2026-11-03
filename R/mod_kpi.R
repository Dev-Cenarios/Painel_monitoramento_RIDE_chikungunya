# ==============================================================================
# Chik RIDE | Modulo: KPIs
#
#   1. casos (do filtro) no ano corrente x mesmo periodo do ano anterior
#   2. ultimas 4 semanas ENCERRADAS, ja com nowcast, x as 4 anteriores
#      (a semana corrente fica de fora: a correcao dela e instavel demais)
#   3. confirmados no ano e % por laboratorio
#   4. hospitalizacoes e obitos no ano, entre os casos provaveis
#   5. notificacoes do ano ainda em investigacao (pendencia de encerramento)
#
# 3 a 5 ignoram o filtro de classificacao de proposito: respondem sempre
# sobre o recorte geografico.
# ==============================================================================

mod_kpi_ui <- function(id) shiny::uiOutput(shiny::NS(id)("boxes"))

mod_kpi_server <- function(id, geo, classe, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    tom <- function(bg, claro = FALSE) {
      bslib::value_box_theme(bg = bg, fg = if (claro) PALETA[["tinta"]] else "#FFFFFF")
    }

    output$boxes <- shiny::renderUI({
      s <- serie()
      shiny::req(nrow(s) > 0)

      ult_se   <- max(s$se)
      ano_cor  <- ult_se %/% 100L
      sem_cor  <- ult_se %% 100L
      no_ano   <- s$ano == ano_cor
      ant_per  <- s$ano == ano_cor - 1L & s$semana <= sem_cor
      casos_ano <- sum(s$casos[no_ano])
      casos_ant <- sum(s$casos[ant_per])
      var_ano   <- if (casos_ant > 0) casos_ano / casos_ant - 1 else NA_real_

      est   <- dplyr::coalesce(s$casos_est, s$casos)
      n     <- nrow(s)
      ult4  <- sum(est[(n - 4):(n - 1)])
      ant4  <- sum(est[(n - 8):(n - 5)])
      var4  <- if (ant4 > 0) ult4 / ant4 - 1 else NA_real_

      d_ano <- filtrar(geo())
      d_ano <- d_ano[d_ano$ano == ano_cor, , drop = FALSE]
      tot   <- sum(d_ano$casos)
      conf  <- sum(d_ano$casos[d_ano$classe %in% CONFIRMADOS])
      lab   <- sum(d_ano$casos[d_ano$classe == "confirmado_lab"])
      prov  <- d_ano[d_ano$classe %in% PROVAVEIS, , drop = FALSE]
      hosp  <- sum(prov$hospitalizados)
      obit  <- sum(prov$obitos)
      inv   <- sum(d_ano$casos[d_ano$classe == "em_investigacao"])

      seta <- function(v) {
        if (is.na(v)) return("")
        sprintf("%s %s", if (v >= 0) "▲" else "▼", fmt_pct(abs(v), 0))
      }
      rot_cl <- tolower(rotulo_filtro_classe(classe()))

      bslib::layout_column_wrap(
        width = "190px", fill = FALSE, class = "mb-3",
        bslib::value_box(
          title = sprintf("%s em %d", rotulo_filtro_classe(classe()), ano_cor),
          value = fmt_int(casos_ano),
          shiny::tags$small(sprintf("%s vs. mesmo período de %d (%s)",
                                    seta(var_ano), ano_cor - 1L, fmt_int(casos_ant))),
          theme = tom(TONS_DOENCA[["600"]])
        ),
        bslib::value_box(
          title = "Últimas 4 semanas (nowcast)",
          value = fmt_int(ult4),
          shiny::tags$small(sprintf("%s %s vs. 4 semanas anteriores", rot_cl, seta(var4))),
          theme = if (!is.na(var4) && var4 > 0.15) tom(TONS_DOENCA[["800"]])
                  else tom(TONS_DOENCA[["100"]], claro = TRUE)
        ),
        bslib::value_box(
          title = sprintf("Confirmados em %d", ano_cor),
          value = fmt_int(conf),
          shiny::tags$small(sprintf("%s por laboratório", fmt_pct(if (conf) lab / conf else NA, 0))),
          theme = tom(TONS_AZUL[["700"]])
        ),
        bslib::value_box(
          title = sprintf("Hospitalizados em %d", ano_cor),
          value = fmt_int(hosp),
          shiny::tags$small(sprintf("%s dos prováveis · %s óbito%s",
                                    fmt_pct(if (sum(prov$casos)) hosp / sum(prov$casos) else NA),
                                    fmt_int(obit), if (obit == 1) "" else "s")),
          theme = tom(PALETA[["critico"]])
        ),
        bslib::value_box(
          title = "Ainda em investigação",
          value = fmt_pct(if (tot) inv / tot else NA, 0),
          shiny::tags$small(sprintf("%s das %s notificações de %d sem classificação final",
                                    fmt_int(inv), fmt_int(tot), ano_cor)),
          theme = tom(TONS_AZUL[["100"]], claro = TRUE)
        )
      )
    })
  })
}
