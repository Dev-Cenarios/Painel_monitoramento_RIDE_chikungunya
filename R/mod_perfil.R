# ==============================================================================
# Chik RIDE | Modulo: perfil e gravidade
#
#   "Faixa etária × sexo": casos por faixa, mulheres x homens. A chikungunya
#     da RIDE pesa nas mulheres adultas (2 em cada 3 confirmados) - mais
#     exposicao domiciliar ao vetor e mais procura por atendimento.
#   "Gravidade por faixa": % hospitalizados e % na fase cronica (artralgia
#     persistente > 3 meses) entre os casos do filtro. Obito por chikungunya
#     e raro na RIDE (poucas unidades em 10 anos), por isso nao vira taxa.
#     Casos recentes ainda sem desfecho: o periodo exclui as ultimas 8
#     semanas.
# ==============================================================================

mod_perfil_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("periodo"), "Período",
                      c("Últimas 52 semanas" = "52", "Desde 2022" = "2022",
                        "Série completa" = "tudo"), selected = "2022")
}

mod_perfil_saida <- function(id, qual = c("faixa", "gravidade"), height = "330px") {
  qual <- match.arg(qual)
  echarts4r::echarts4rOutput(shiny::NS(id)(qual), height = height)
}

mod_perfil_server <- function(id, geo, classe) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      ini <- switch(input$periodo %||% "2022",
                    "52" = DATA_MAX - 51L * 7L, "2022" = as.Date("2022-01-02"), DATA_MIN)
      d <- filtrar(geo(), classes_do_filtro(classe()))
      d[d$data_ini_se >= ini & d$faixa_etaria %in% FAIXAS, , drop = FALSE]
    })

    output$faixa <- echarts4r::renderEcharts4r({
      d <- base()
      shiny::validate(shiny::need(sum(d$casos) > 0, "Sem casos no período."))
      g <- d |>
        dplyr::group_by(faixa_etaria) |>
        dplyr::summarise(mulheres = sum(feminino), homens = sum(casos) - sum(feminino),
                         .groups = "drop")
      g <- data.frame(faixa_etaria = FAIXAS) |>
        dplyr::left_join(g, by = "faixa_etaria") |>
        dplyr::mutate(dplyr::across(c(mulheres, homens), ~ dplyr::coalesce(.x, 0)),
                      x = unname(ROTULO_FAIXA[faixa_etaria]))

      g |>
        echarts4r::e_charts(x) |>
        echarts4r::e_bar(mulheres, name = "Mulheres", stack = "s") |>
        echarts4r::e_bar(homens, name = "Homens", stack = "s") |>
        echarts4r::e_color(c(TONS_DOENCA[["600"]], TONS_DOENCA[["300"]])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(name = "Casos") |>
        echarts4r::e_grid(left = 55, right = 15, top = 30, bottom = 50) |>
        e_padrao()
    })

    output$gravidade <- echarts4r::renderEcharts4r({
      d <- base()
      d <- d[d$data_ini_se <= DATA_MAX - 8L * 7L, , drop = FALSE]
      shiny::validate(shiny::need(sum(d$casos) > 0, "Sem casos encerrados no período."))
      g <- d |>
        dplyr::group_by(faixa_etaria) |>
        dplyr::summarise(casos = sum(casos),
                         hosp = round(100 * sum(hospitalizados) / casos, 1),
                         cron = round(100 * sum(cronicos) / casos, 1), .groups = "drop")
      g <- data.frame(faixa_etaria = FAIXAS) |> dplyr::left_join(g, by = "faixa_etaria")
      g$x <- unname(ROTULO_FAIXA[g$faixa_etaria])

      g |>
        echarts4r::e_charts(x) |>
        echarts4r::e_bar(hosp, name = "% hospitalizados") |>
        echarts4r::e_bar(cron, name = "% fase crônica") |>
        echarts4r::e_color(c(PALETA[["critico"]], TONS_AZUL[["400"]])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE,
                             valueFormatter = htmlwidgets::JS("function(v){return v + '%';}")) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(name = "%") |>
        echarts4r::e_grid(left = 45, right = 15, top = 30, bottom = 50) |>
        e_padrao()
    })
  })
}
