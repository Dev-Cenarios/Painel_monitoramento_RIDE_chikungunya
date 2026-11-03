# ==============================================================================
# Chik RIDE | ui.R
#
# Aba "Painel":
#   0. cabecalho
#   1. filtros (recorte geografico + classificacao dos casos)
#   2. KPIs
#   3. mapa (5) | evolucao semanal por classificacao (7)
#   4. Rt do InfoDengue (largura total)
#   5. canal endemico (7) | sazonalidade (5)
#   6. perfil (sexo x idade) e gravidade
# Aba "Análises": ranking de municipios, classificacao por ano, achados.
# Aba "Sobre os dados": fonte, definicoes e limitacoes.
# ==============================================================================

bslib::page_navbar(
  title        = "Chik RIDE",
  theme        = TEMA_PAINEL,
  window_title = "Chik RIDE — Monitoramento de chikungunya",
  padding      = "1rem",
  fillable     = FALSE,

  bslib::nav_panel(
    "Painel",
    painel_ui_assets(),

    shiny::tags$div(
      class = "d-flex justify-content-between align-items-end mb-3",
      shiny::tags$div(
        shiny::tags$h4(class = "mb-0 fw-semibold", "Chik RIDE"),
        shiny::tags$span(class = "text-muted small",
                         "Painel de monitoramento de chikungunya · fonte SINAN")
      ),
      shiny::tags$span(
        class = "text-muted small text-end",
        sprintf("%d municípios · %d–%d · dados até a semana de %s",
                nrow(GEO_DICT), ANO_MIN, ANO_MAX, format(DATA_MAX, "%d/%m/%Y"))
      )
    ),

    bslib::card(
      class = "mb-3",
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        shiny::tags$span(class = "fw-semibold", "Recorte"),
        shiny::tags$span(class = "small", mod_filtros_resumo("filtros"))
      ),
      bslib::card_body(mod_filtros_ui("filtros"))
    ),

    mod_kpi_ui("kpi"),

    bslib::layout_columns(
      col_widths = c(5, 7),
      cartao_grafico("Distribuição espacial", mod_mapa_saida("mapa", "460px"),
                     mod_mapa_controles("mapa")),
      cartao_grafico("Casos semanais por classificação final", mod_serie_saida("serie", "460px"),
                     mod_serie_controles("serie"),
                     rodape = "Semana de início dos sintomas. Linha tracejada: estimativa corrigida pelo atraso de digitação (nowcast) e faixa de incerteza.")
    ),

    cartao_grafico("Rt (InfoDengue) — número de reprodução efetivo", mod_rt_saida("rt", "320px"),
                   mod_rt_controles("rt"),
                   rodape = "Acima de 1 (linha tracejada), cada caso gera mais de um caso novo: transmissão em expansão. No estado e na RIDE, média dos municípios ponderada pelos casos."),

    bslib::layout_columns(
      col_widths = c(7, 5),
      cartao_grafico("Diagrama de controle (canal endêmico)", mod_canal_saida("canal", "380px"),
                     mod_canal_controles("canal")),
      cartao_grafico("Sazonalidade — ano × semana epidemiológica",
                     mod_heatmap_saida("heatmap", "420px"), mod_heatmap_controles("heatmap"))
    ),

    bslib::navset_card_tab(
      full_screen = TRUE,
      title = shiny::tags$span(class = "fw-semibold", "Perfil e gravidade"),
      bslib::nav_panel("Faixa etária × sexo",
                       bslib::card_body(class = "p-0", mod_perfil_saida("perfil", "faixa"))),
      bslib::nav_panel("Gravidade por faixa",
                       bslib::card_body(class = "p-0", mod_perfil_saida("perfil", "gravidade"),
                                        shiny::tags$p(class = "small text-muted px-3",
                                                      "Exclui as 8 semanas mais recentes (desfecho ainda em aberto). Fase crônica = campo da ficha; costuma ser subregistrada."))),
      bslib::nav_spacer(),
      bslib::nav_item(bslib::popover(gatilho_opcoes(), mod_perfil_controles("perfil"),
                                     title = "Opções", placement = "left"))
    )
  ),

  bslib::nav_panel("Análises", mod_analises_ui("analises")),

  bslib::nav_panel(
    "Sobre os dados",
    bslib::card(
      bslib::card_body(
        class = "small",
        shiny::tags$h5("Fonte"),
        shiny::tags$p(
          "Microdados de chikungunya do SINAN (arquivos CHIKBR do DATASUS), tabela ", shiny::tags$code("public.sinan_chik"),
          sprintf(" no banco do projeto. Notificações de residentes nos 34 municípios da RIDE-DF (LC 163/2018), de %s a %s. ",
                  format(DATA_MIN, "%d/%m/%Y"), format(DATA_MAX, "%d/%m/%Y")),
          "O ETL (", shiny::tags$code("etl_chik_ride.R"), ") agrega tudo no Postgres; o painel não lê microdado."
        ),
        shiny::tags$h5("Definições"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$b("Semana:"), " semana epidemiológica (domingo a sábado) do início dos sintomas."),
          shiny::tags$li(shiny::tags$b("Caso provável:"), " toda notificação, exceto as descartadas (definição do Ministério da Saúde; é o número dos boletins e do InfoDengue)."),
          shiny::tags$li(shiny::tags$b("Classificação:"), " CLASSI_FIN = 13 é chikungunya confirmada - por laboratório (CRITERIO = 1) ou clínico-epidemiológico; 5 descartado; 8 inconclusivo; vazio = em investigação. Nas fichas de 2014–2016, códigos antigos de dengue (1/2) entram como inconclusivo."),
          shiny::tags$li(shiny::tags$b("Incidência:"), " casos por 100 mil habitantes, com a estimativa populacional anual do IBGE usada pelo InfoDengue."),
          shiny::tags$li(shiny::tags$b("Gravidade:"), " hospitalizado = HOSPITALIZ 1; fase crônica = CLINC_CHIK 2; óbito = EVOLUCAO 2 (óbito pelo agravo)."),
          shiny::tags$li(shiny::tags$b("DF:"), " residência 53xxxx somada no município Brasília (5300108); no SINAN quase tudo já vem como 530010.")
        ),
        shiny::tags$h5("Canal endêmico"),
        shiny::tags$p("Ano sanitário (SE 27 a SE 26), como no painel de dengue. Faixas = quartis da incidência semanal das temporadas de referência; 2021/22 e 2022/23 (epidemia em Goiás) ficam fora por padrão."),
        shiny::tags$h5("Nowcast"),
        shiny::tags$p(sprintf(
          "Proporção acumulada de notificações digitadas em até k semanas, F(k), medida nas semanas já maduras (≥ 26 semanas) dos últimos três anos; o valor das últimas %d semanas é dividido por F(k), exceto a semana corrente, ainda incompleta demais para estimar. F(0) = %s, F(1) = %s, F(4) = %s. Faixa: quantis 10%%–90%% de F(k) entre as semanas de treino. O mesmo atraso é aplicado a todo recorte e classificação.",
          NOWCAST_MAX_K, fmt_pct(NOWCAST_F$f[1]), fmt_pct(NOWCAST_F$f[2]), fmt_pct(NOWCAST_F$f[5]))),
        shiny::tags$h5("Rt"),
        shiny::tags$p("Número de reprodução efetivo estimado pelo InfoDengue para cada município (tabela ", shiny::tags$code("\"Info_Dengue\".chikungunya"),
                      "), o mesmo usado no painel de dengue. No município é o valor direto; no estado e na RIDE, média ponderada pelos casos de cada município - aproximação, porque Rt não se soma entre áreas. O tooltip mostra quantos municípios estão com Rt > 1. Não segue o filtro \"Casos\"."),
        shiny::tags$h5("Limitações"),
        shiny::tags$ul(
          shiny::tags$li("O nowcast corrige a digitação, não o encerramento: notificações recentes ainda \"em investigação\" vão virar confirmadas ou descartadas."),
          shiny::tags$li("Poucos casos por semana fora do pico: canal endêmico e nowcast ficam instáveis em município pequeno."),
          shiny::tags$li("Fase crônica e autoctonia têm muito campo em branco; os percentuais são um piso."),
          shiny::tags$li("A ficha de dengue/chikungunya é a mesma: na cocirculação, parte das suspeitas é reclassificada como dengue e sai da contagem de chikungunya.")
        )
      )
    )
  )
)
