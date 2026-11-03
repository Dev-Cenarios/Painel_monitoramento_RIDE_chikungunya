# ==============================================================================
# Modulo: Numero de Reproducao Efetivo (Rt), estimado pelo InfoDengue
#
# Mesmo grafico do painel de dengue: linha de Rt com limiar em 1,0 (acima de
# 1 cada caso gera mais de um caso novo e a transmissao esta em expansao).
#
# Agregacao (ver serie_rt() no global.R): no municipio, o valor da coluna
# `rt`; no estado ou na RIDE, media ponderada pelos casos de cada municipio -
# aproximacao, rotulada como tal - e o tooltip mostra quantos municipios estao
# com Rt > 1 na semana.
#
# Nao segue o filtro "Casos": o Rt do InfoDengue e um so por municipio.
# ==============================================================================

# Rt tem cauda longa em municipio pequeno (poucos casos geram razoes altas).
RT_TETO_VISUAL <- 5

mod_rt_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("periodo"), "Período",
                        c("Últimas 52 semanas" = "52", "Últimas 104 semanas" = "104",
                          "Série completa" = "tudo"),
                        selected = RT_PERIODO_PADRAO),
    shiny::checkboxInput(ns("limitar_escala"),
                         sprintf("Limitar eixo a Rt = %d", RT_TETO_VISUAL), value = TRUE),
    shiny::tags$hr(),
    shiny::uiOutput(ns("nota_metodo"))
  )
}

mod_rt_saida <- function(id, height = "320px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}

mod_rt_server <- function(id, geo) {
  shiny::moduleServer(id, function(input, output, session) {

    agregado <- shiny::reactive(!identical(geo()$nivel, "municipio"))

    dados <- shiny::reactive({
      d <- serie_rt(geo())
      shiny::validate(shiny::need(!is.null(d),
        "Sem dados de Rt: rode o ETL de novo para gerar o Parquet do InfoDengue."))
      fim <- max(RT_INFODENGUE$data_ini_se)
      ini <- switch(input$periodo %||% RT_PERIODO_PADRAO,
                    "52" = fim - 51L * 7L, "104" = fim - 103L * 7L, min(d$data_ini_se))
      d <- d[d$data_ini_se >= ini, , drop = FALSE]
      d$limiar <- 1
      d
    })

    output$nota_metodo <- shiny::renderUI({
      shiny::tags$div(
        class = "small text-muted",
        if (agregado()) {
          shiny::tagList(shiny::tags$strong("Rt agregado."), shiny::tags$br(),
                         "Média ponderada pelos casos de cada município. Aproximação: ",
                         "Rt não é aditivo entre áreas.")
        } else {
          shiny::tagList(shiny::tags$strong("Rt municipal."), shiny::tags$br(),
                         "Valor direto da coluna ", shiny::tags$code("rt"), " de ",
                         shiny::tags$code(RT_FONTE), ".")
        }
      )
    })

    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados()
      shiny::req(nrow(d) > 0)
      shiny::validate(shiny::need(any(!is.na(d$rt)),
                                  "Sem estimativas de Rt para este recorte no período."))
      d$x <- as.character(d$data_ini_se)

      tabela_js <- jsonlite::toJSON(
        data.frame(rt = round(d$rt, 2), n_mun = d$n_mun, n_mun_rt1 = d$n_mun_rt1,
                   casos = d$casos),
        dataframe = "rows", na = "null", auto_unbox = TRUE, digits = 2
      )
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(params){
           if (!params || !params.length) return '';
           var tab = %s, r = tab[params[0].dataIndex];
           if (!r) return '';
           function f(x, d){ return (x === null || x === undefined) ? '—' :
             x.toLocaleString('pt-BR', {maximumFractionDigits: d}); }
           var cor = (r.rt !== null && r.rt > 1) ? '%s' : '%s';
           var txt = '<b>' + %s(params[0].axisValue) + '</b><br/>' +
                     'Rt: <b style=\"color:' + cor + '\">' + f(r.rt, 2) + '</b><br/>' +
                     'Casos na semana (InfoDengue): ' + f(r.casos, 0);
           if (%s) txt += '<br/><span style=\"opacity:.7\">' + f(r.n_mun_rt1, 0) +
                          ' de ' + f(r.n_mun, 0) + ' municípios com Rt > 1</span>';
           return txt;
         }",
        tabela_js, PALETA[["critico"]], PALETA[["azul_titulo"]], JS_DATA_BR,
        if (agregado()) "true" else "false"
      ))

      teto <- if (isTRUE(input$limitar_escala)) RT_TETO_VISUAL else max(c(d$rt, 1.5), na.rm = TRUE)

      d |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(rt, name = "Rt", symbol = "none", connectNulls = FALSE,
                          lineStyle = list(color = PALETA[["doenca"]], width = 2),
                          itemStyle = list(color = PALETA[["doenca"]])) |>
        echarts4r::e_line(limiar, name = "Limiar (Rt = 1)", symbol = "none", silent = TRUE,
                          lineStyle = list(color = PALETA[["tinta"]], width = 1.5, type = "dashed"),
                          itemStyle = list(color = PALETA[["tinta"]]),
                          tooltip = list(show = FALSE)) |>
        echarts4r::e_x_axis(axisLabel = list(formatter = htmlwidgets::JS(
          sprintf("function(v){return %s(v);}", JS_DATA_BR)))) |>
        echarts4r::e_y_axis(name = "Rt", nameLocation = "end", nameGap = 12, min = 0, max = teto) |>
        echarts4r::e_datazoom(x_index = 0, type = "slider", bottom = 4) |>
        echarts4r::e_legend(top = 0, type = "scroll") |>
        echarts4r::e_grid(left = 50, right = 24, top = 40, bottom = 78) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = js_tooltip) |>
        e_padrao()
    })
  })
}
