# Chik RIDE — Painel de monitoramento de chikungunya

Painel Shiny (bslib + echarts4r) irmão do **dengue_ride** e do **gripe_ride**,
com as notificações de chikungunya de residentes nos 34 municípios da
RIDE-DF. Fonte: microdado do SINAN (arquivos CHIKBR do DATASUS), tabela
`public.sinan_chik` do banco do projeto.

## 1. Estrutura

```
chik_ride/
├── etl_chik_ride.R      # Postgres -> data/*.parquet (tudo agregado no banco)
├── global.R             # dados em memória, classificação, ano sanitário, nowcast
├── ui.R / server.R
├── R/
│   ├── mod_filtros.R    # recorte (RIDE-DF / Estado / Município) + "Casos"
│   ├── mod_kpi.R        # 5 indicadores do topo
│   ├── mod_mapa.R       # incidência por município (clique = foco)
│   ├── mod_serie.R      # casos semanais empilhados por classificação + nowcast
│   ├── mod_rt.R         # Rt do InfoDengue (como no dengue_ride)
│   ├── mod_mapa_rt.R    # Rt — Tendência por município (quadrantes), como no dengue_ride
│   ├── mod_canal.R      # canal endêmico (ano sanitário, como a dengue)
│   ├── mod_heatmap.R    # sazonalidade ano x SE
│   ├── mod_perfil.R     # faixa etária x sexo; hospitalização/fase crônica
│   ├── mod_analises.R   # ranking, classificação por ano, achados calculados
│   └── utils_ui.R       # tema e cards
└── data/
    ├── chik_ride_df.parquet      # semana x município x classificação x faixa
    ├── chik_ride_atraso.parquet  # semana x atraso de digitação (nowcast)
    ├── chik_ride_rt.parquet      # semana x município -> Rt do InfoDengue
    └── geo/MUN_{31,52,53}.geojson
```

## 2. Atualizar os dados

Precisa do `.Renviron` com `PG_HOST`, `PG_PORT`, `PG_DB`, `PG_USER`, `PG_PASS`
(o mesmo dos outros painéis; ver `.Renviron.example`).

```bash
cd /srv/shiny-server/chik_ride && Rscript etl_chik_ride.R && touch restart.txt
```

O ETL leva poucos segundos. Os Parquets **não são versionados**
(`.gitignore`): no servidor, rode o ETL depois do primeiro `git pull`.

Rodar local (RStudio): abrir a pasta, `shiny::runApp()`. Pacotes: shiny,
bslib, echarts4r, DT, dplyr, tidyr, arrow, jsonlite, sass, htmlwidgets (+ DBI e
RPostgres para o ETL).

## 3. Decisões de tratamento

| Tema | Decisão |
|---|---|
| Fonte | `public.sinan_chik`, não `silver.chikungunya`: é o mesmo microdado com os nomes originais do SINAN, e o usuário do painel tem SELECT nela (no `silver`, não). Traz `dt_digita`, que permite o nowcast. |
| Caso | **Provável** = notificado − descartado (definição do MS, a dos boletins e do InfoDengue). O seletor "Casos" troca para confirmados, confirmados por laboratório, em investigação, todas as notificações ou descartados. |
| Classificação | `CLASSI_FIN` 13 = confirmado (laboratório se `CRITERIO` = 1, senão clínico-epidemiológico); 5 descartado; 8 inconclusivo; vazio = em investigação. Códigos antigos de dengue (1/2, só em 2014–2016) entram como inconclusivo, o que mantém os prováveis iguais aos do InfoDengue. |
| Residência no DF | Quase tudo vem como 530010; os poucos 539xxx também vão para 5300108. (Ao contrário do SIVEP, o SINAN não traz RA.) |
| Semana | Semana epidemiológica (dom–sáb) do início dos sintomas. |
| Canal endêmico | **Ano sanitário** (SE 27→26), como o dengue_ride. Referência padrão: temporadas completas, exceto 2021/22 e 2022/23 (epidemia em Goiás). Abre na temporada anterior durante as primeiras 20 semanas da nova (entressafra). |
| Nowcast | Chain ladder empírico sobre `DT_DIGITA` (3 anos de semanas maduras). Corrige as semanas 1 a 8 anteriores ao arquivo; a semana corrente (F(0) ≈ 16%) fica sem estimativa, porque com poucas notificações a divisão explode. |
| Rt | `rt` do InfoDengue (`"Info_Dengue".chikungunya`), por município, como no dengue_ride. No estado e na RIDE, média ponderada pelos casos (aproximação) + nº de municípios com Rt > 1. |
| Rt — Tendência (quadrantes) | Não é o Rt: X = casos da janela atual ÷ mediana da mesma janela nos anos anteriores (deslocada 52, 104… semanas); Y = janela atual ÷ janela anterior. Cor = quadrante, forma = incidência acumulada no ano. Todos os municípios sempre; o recorte só destaca o selecionado. Segue o filtro "Casos"; janela padrão 8 sem. (até 26). Semana corrente fora e recentes corrigidas pelo nowcast. |
| Gravidade | Hospitalização (`HOSPITALIZ`), fase crônica (`CLINC_CHIK` = 2) e óbito (`EVOLUCAO` = 2). Óbito é raro demais para virar taxa. |
| População | Estimativa anual do IBGE em `"Info_Dengue".dengue`; anos sem notificação no município herdam o ano vizinho. |

## 4. Achados da exploratória (out/2026)

Calculados pelo app a partir da base (aba **Análises**):

- **Sazonalidade**: pico mediano na SE 16 (abril); 66% dos casos entre as
  SE 5 e 22.
- **Epidemia de 2022–2023 no Entorno goiano**: 67% dos prováveis do biênio
  em Goiás. Luziânia (754/100 mil) e Santo Antônio do Descoberto (667/100
  mil) contra 41/100 mil no DF.
- **2026 vs 2025** (até a SE 41): prováveis +32%, confirmados +18%,
  hospitalizações de 10 para 43. O canal mostra 2025/26 acima do Q3 entre
  abril e junho. Novo Gama lidera a incidência das últimas 52 semanas.
- **Confirmação**: 94% laboratorial desde 2022. Em 2020, 91% das 1,6 mil
  notificações foram descartadas (quase todas no DF) — pico de notificação
  ≠ pico de doença.
- **Perfil**: 66% dos confirmados são mulheres; 54% têm 30–59 anos.
- **Gravidade**: 3,4% hospitalizados, 1,2% com fase crônica registrada
  (piso), 5 óbitos confirmados na série.
- **Autoctonia**: 96% dos confirmados com o campo preenchido são autóctones.
- **Atraso**: 52% das fichas digitadas ao fim da semana seguinte, 87% em 4
  semanas; 14% das notificações das últimas 52 semanas ainda estão em
  investigação.

## 5. Próximos passos possíveis

- Cruzar com dengue (mesma ficha, mesmo vetor): sobreposição de picos e
  reclassificação entre os dois agravos.
- Alerta do InfoDengue (`"Info_Dengue".chikungunya` tem `nivel` e `p_rt1`).
- Nowcast do encerramento (em investigação → confirmado/descartado).
