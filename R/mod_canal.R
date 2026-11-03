# ==============================================================================
# Chik RIDE | Modulo: diagrama de controle (canal endemico)
#
# Igual ao painel de dengue, o canal usa o ANO SANITARIO (SE 27 a SE 26 do
# ano seguinte): a subida da chikungunya comeca em novembro/dezembro e o pico
# fica entre fevereiro e abril, entao o ano civil partiria a temporada.
#
# Canal = quartis da incidencia semanal das temporadas de referencia:
#   abaixo de Q1 (sucesso) | Q1-Q3 (seguranca/alerta, mediana marcada) |
#   acima de Q3 (epidemia).
#
# Referencia padrao: todas as temporadas completas anteriores a analisada,
# exceto 2021/22 e 2022/23 (epidemia em Goias - inflariam o canal e
# esconderiam a proxima). O usuario pode incluir qualquer temporada. Com
# poucos casos por semana o Q1 fica em zero na maior parte do ano - o canal
# serve sobretudo para dizer "acima do esperado" (Q3).
# ==============================================================================

TEMPORADAS_EPIDEMICAS <- c(2021L, 2022L)

mod_canal_controles <- function(id) {
  ns <- shiny::NS(id)
  # A primeira temporada (2014/15) comeca em jan/2015: incompleta, fica fora.
  tps <- TEMPORADAS[TEMPORADAS > min(TEMPORADAS)]
  rot <- stats::setNames(tps, vapply(tps, rotulo_ano_sanitario, character(1)))
  shiny::tagList(
    shiny::selectInput(ns("temporada"), "Temporada analisada", choices = rev(rot),
                       selected = TEMPORADA_PADRAO),
    shiny::checkboxGroupInput(ns("ref"), "Temporadas de referência",
                              choices = rot, inline = TRUE,
                              selected = tps[tps < TEMPORADA_PADRAO & !tps %in% TEMPORADAS_EPIDEMICAS]),
    shiny::helpText(shiny::tags$small(
      "2021/22 e 2022/23 ficam fora por padrão: a epidemia em Goiás inflaria o canal."
    )),
    shiny::checkboxInput(ns("nowcast"), "Mostrar nowcast", TRUE)
  )
}

mod_canal_saida <- function(id, height = "400px") {
  shiny::tagList(
    echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height),
    shiny::div(class = "small text-muted px-3 pb-2", shiny::textOutput(shiny::NS(id)("nota")))
  )
}

mod_canal_server <- function(id, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    dados <- shiny::reactive({
      s <- serie()
      s$semana <- pmin(s$semana, 52L)  # SE 53 entra como 52
      s$temporada <- ano_sanitario(s$ano, s$semana)
      s <- s |>
        dplyr::group_by(temporada, semana) |>
        dplyr::summarise(inc = sum(casos) / max(pop) * 1e5,
                         inc_est = if (all(is.na(casos_est))) NA_real_
                                   else sum(dplyr::coalesce(casos_est, casos)) / max(pop) * 1e5,
                         .groups = "drop")

      tp  <- as.integer(input$temporada %||% TEMPORADA_PADRAO)
      ref <- setdiff(as.integer(input$ref), tp)

      canal <- s |>
        dplyr::filter(temporada %in% ref) |>
        dplyr::group_by(semana) |>
        dplyr::summarise(q1 = stats::quantile(inc, 0.25), q2 = stats::median(inc),
                         q3 = stats::quantile(inc, 0.75), .groups = "drop")

      atual <- s |> dplyr::filter(temporada == tp) |> dplyr::select(semana, inc, inc_est)

      out <- data.frame(semana = NIVEIS_SEMANA_SANITARIA) |>
        dplyr::left_join(canal, by = "semana") |>
        dplyr::left_join(atual, by = "semana")
      list(d = out, tp = tp, ref = ref)
    })

    output$nota <- shiny::renderText({
      r <- dados()
      sprintf("Canal: quartis da incidência semanal de %d temporada%s (%s). Linha: %s, por semana de início dos sintomas.",
              length(r$ref), if (length(r$ref) == 1) "" else "s",
              if (length(r$ref)) paste(vapply(sort(r$ref), rotulo_ano_sanitario, character(1)), collapse = ", ")
              else "nenhuma",
              rotulo_ano_sanitario(r$tp))
    })

    output$grafico <- echarts4r::renderEcharts4r({
      r <- dados()
      d <- r$d
      shiny::validate(shiny::need(length(r$ref) > 0, "Escolha ao menos uma temporada de referência."))

      d <- d |>
        dplyr::mutate(
          x      = sprintf("SE %02d", semana),
          base   = round(q1, 2),
          faixa  = round(q3 - q1, 2),
          q2     = round(q2, 2),
          inc    = round(inc, 2),
          inc_est = round(inc_est, 2)
        )
      com_nc <- isTRUE(input$nowcast) && any(!is.na(d$inc_est))
      if (com_nc) {
        # A linha do nowcast parte do ultimo ponto observado antes dela.
        k0 <- min(which(!is.na(d$inc_est))) - 1L
        if (k0 >= 1L) d$inc_est[k0] <- d$inc[k0]
      }

      e <- d |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(base, name = "Abaixo de Q1", stack = "canal", symbol = "none",
                          lineStyle = list(width = 0),
                          areaStyle = list(color = TONS_AZUL[["50"]])) |>
        echarts4r::e_line(faixa, name = "Q1 – Q3", stack = "canal", symbol = "none",
                          lineStyle = list(width = 0),
                          areaStyle = list(color = TONS_AZUL[["200"]])) |>
        echarts4r::e_line(q2, name = "Mediana", symbol = "none",
                          lineStyle = list(width = 2, color = TONS_AZUL[["800"]])) |>
        echarts4r::e_line(inc, name = rotulo_ano_sanitario(r$tp), symbol = "circle", symbolSize = 5,
                          lineStyle = list(width = 3, color = TONS_DOENCA[["700"]]),
                          itemStyle = list(color = TONS_DOENCA[["700"]]))
      cores <- c(TONS_AZUL[["50"]], TONS_AZUL[["200"]], TONS_AZUL[["800"]], TONS_DOENCA[["700"]])
      if (com_nc) {
        e <- e |> echarts4r::e_line(inc_est, name = "Nowcast", symbol = "none",
                                    lineStyle = list(type = "dashed", width = 2,
                                                     color = PALETA[["nowcast"]]))
        cores <- c(cores, PALETA[["nowcast"]])
      }

      # Tooltip: a serie "Q1 – Q3" guarda a LARGURA da faixa (empilhada sobre
      # Q1); o tooltip mostra Q3 = Q1 + largura.
      tt <- htmlwidgets::JS("function(ps){
        var out = ps[0].axisValue, q1 = null;
        ps.forEach(function(p){
          var v = Array.isArray(p.value) ? p.value[1] : p.value;
          if (v === null || v === undefined || v === '-' || isNaN(v)) return;
          if (p.seriesName === 'Abaixo de Q1') { q1 = +v; out += '<br/>Q1: ' + (+v).toFixed(2); return; }
          if (p.seriesName === 'Q1 – Q3') { out += '<br/>Q3: ' + (q1 + +v).toFixed(2); return; }
          out += '<br/>' + p.marker + p.seriesName + ': ' + (+v).toFixed(2);
        });
        return out;
      }")

      e |>
        echarts4r::e_color(unname(cores)) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = tt, confine = TRUE) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(name = "Incidência /100 mil") |>
        echarts4r::e_grid(left = 55, right = 20, top = 30, bottom = 60) |>
        e_padrao()
    })
  })
}
