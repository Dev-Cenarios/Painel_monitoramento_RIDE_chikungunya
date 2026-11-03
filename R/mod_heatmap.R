# ==============================================================================
# Chik RIDE | Modulo: sazonalidade (ano x semana epidemiologica)
#
# Incidencia semanal do recorte+classificacao. Mostra a faixa de transmissao
# de fevereiro a maio (SE 5-20) e os anos epidemicos (2016 no DF, 2022-2023
# em Goias). Ano calendario aqui: o pico cabe inteiro no ano civil e a
# leitura "uma linha = um ano" e mais direta; o canal endemico e que usa o
# ano sanitario.
# ==============================================================================

mod_heatmap_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("metrica"), "Métrica",
                      c("Incidência /100 mil" = "inc", "Casos" = "casos"), selected = "inc")
}

mod_heatmap_saida <- function(id, height = "330px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}

mod_heatmap_server <- function(id, serie) {
  shiny::moduleServer(id, function(input, output, session) {
    output$grafico <- echarts4r::renderEcharts4r({
      s <- serie()
      met <- input$metrica %||% "inc"
      s$valor <- round(if (met == "inc") s$incidencia else s$casos, 2)
      s$semana <- pmin(s$semana, 52L)
      d <- s |>
        dplyr::group_by(ano, semana) |>
        dplyr::summarise(valor = sum(valor), .groups = "drop") |>
        dplyr::mutate(x = sprintf("%02d", semana), y = as.character(ano), valor = round(valor, 2)) |>
        dplyr::arrange(semana, ano)

      # Teto no percentil 99 das semanas com caso: uma unica semana de pico
      # epidemico apagaria todo o resto em tons claros. Acima do teto a cor
      # satura (o tooltip mostra o valor real).
      pos <- d$valor[d$valor > 0]
      teto <- if (length(pos)) stats::quantile(pos, 0.99, names = FALSE) else 1

      e <- d |>
        echarts4r::e_charts(x) |>
        echarts4r::e_heatmap(y, valor, name = if (met == "inc") "Incidência /100 mil" else "Casos",
                             itemStyle = list(borderColor = "#FFFFFF", borderWidth = 1)) |>
        echarts4r::e_visual_map(valor, inRange = list(color = ESCALA_MAPA),
                                orient = "horizontal", left = "center", bottom = 0,
                                calculable = FALSE, itemHeight = 160,
                                text = list("alto (acima satura)", "0")) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){return 'SE ' + p.value[0] + '/' + p.value[1] + '<br/>' +
             p.seriesName + ': ' + Number(p.value[2]).toLocaleString('pt-BR',{maximumFractionDigits:1});}")) |>
        echarts4r::e_x_axis(name = "SE", nameLocation = "middle", nameGap = 22) |>
        echarts4r::e_grid(left = 50, right = 15, top = 10, bottom = 80) |>
        e_padrao()
      # e_visual_map() recalcula min/max a partir da serie; o teto e imposto
      # depois. No visualMap continuo, valor acima do max recebe a cor maxima.
      e$x$opts$visualMap[[1]]$min <- 0
      e$x$opts$visualMap[[1]]$max <- max(round(teto, 1), 0.1)
      e
    })
  })
}
