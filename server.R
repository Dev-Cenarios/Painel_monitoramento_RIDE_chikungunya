# ==============================================================================
# Chik RIDE | server.R
# Orquestra os modulos. Os dados ja estao em memoria (global.R): cada modulo
# filtra DADOS com filtrar(), sem consulta a banco.
# ==============================================================================

function(input, output, session) {

  # Clique no mapa (ou "Visão geral") -> filtros. O mapa so pede o foco; o
  # estado do recorte mora nos inputs do modulo de filtros.
  foco_mapa <- shiny::reactiveVal(NULL)

  filtros <- mod_filtros_server("filtros", foco_externo = foco_mapa)
  geo    <- filtros$geo
  classe <- filtros$classe

  # Serie semanal do recorte + classificacao: compartilhada por KPIs, serie,
  # canal e heatmap.
  serie <- shiny::reactive(serie_semanal(geo(), classes_do_filtro(classe()))) |>
    shiny::bindCache(geo(), classe())

  mod_kpi_server("kpi", geo, classe, serie)

  pedido <- mod_mapa_server("mapa", geo, classe)
  shiny::observeEvent(pedido(), foco_mapa(pedido()), ignoreInit = TRUE)

  mod_serie_server("serie", geo, classe, serie)
  mod_rt_server("rt", geo)
  # Quadrantes: todos os municipios (o recorte so destaca o selecionado).
  pedido_tendencia <- mod_mapa_rt_server("tendencia", geo, shiny::reactive(classes_do_filtro(classe())))
  # Clique num ponto da tendencia: mesmo efeito do clique no mapa.
  shiny::observeEvent(pedido_tendencia(), foco_mapa(pedido_tendencia()), ignoreInit = TRUE)
  mod_canal_server("canal", serie)
  mod_heatmap_server("heatmap", serie)
  mod_perfil_server("perfil", geo, classe)
  mod_analises_server("analises", classe)
}
