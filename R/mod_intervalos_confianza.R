# ============================================================
# mod_intervalos_confianza.R — Intervalos de confianza
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Simulación interactiva (cobertura: % de IC que sí
#      contienen el verdadero parámetro)
#   3. Los datos
#   4. Practica con datos reales
#   5. Código R
#
# Nota pedagógica: construye directamente sobre Error Estándar
# (IC = estimado ± margen, margen = t × EE). El punto más
# importante a ense\u00f1ar aqu\u00ed es la interpretaci\u00f3n correcta del
# nivel de confianza (sobre el PROCEDIMIENTO, no sobre un IC
# particular) — es la confusi\u00f3n m\u00e1s com\u00fan del tema.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_intervalos_confianza_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("arrows-angle-contract", class = "me-2"),
        "Intervalos de confianza",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "El error est\u00e1ndar te dice cu\u00e1nta incertidumbre hay en tu ",
        "estimado \u2014 pero un solo n\u00famero es dif\u00edcil de comunicar. El ",
        "intervalo de confianza (IC) traduce esa incertidumbre en algo ",
        "m\u00e1s \u00fatil: ", strong("un rango de valores plausibles"), " para ",
        "el verdadero par\u00e1metro."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(

          paso_infografia(
            1, "rulers", colores$primario,
            "\u00bfQu\u00e9 es un intervalo de confianza?",
            p(class = "mb-2",
              "Un IC es un rango de valores construido as\u00ed:"
            ),
            div(style = paste0("background:", colores$fondo,
                              "; border-radius:8px; padding:14px;",
                              "text-align:center; font-size:1.15rem;",
                              "font-weight:600; color:", colores$primario, ";"),
                "Estimado \u00b1 (valor cr\u00edtico \u00d7 error est\u00e1ndar)"
            ),
            p(class = "mt-2 mb-0",
              "Por ejemplo, con la media de vampiros (", strong("175 kg"),
              ") y su error est\u00e1ndar (", strong("5.53 kg"), "), un IC del ",
              "95% ser\u00eda aproximadamente ", strong("175 \u00b1 2 \u00d7 5.53"),
              " \u2248 ", strong("[164, 186] kg"), " \u2014 el rango donde ",
              "esperar\u00edamos que caiga el verdadero peso promedio ",
              "poblacional."
            ),
            div(class = "alert alert-secondary small mt-2 mb-0",
                bs_icon("info-circle", class = "me-1",
                        style = paste0("color:", colores$primario)),
                "\u00bfDe d\u00f3nde sale ese ", strong("2"), "? Es un redondeo de ",
                strong("z = 1.96"), " \u2014 el valor exacto que, en una ",
                em("distribuci\u00f3n normal"), ", deja el 95% de los datos ",
                "entre \u22121.96 y +1.96 desviaciones est\u00e1ndar de la media ",
                "(la \"regla emp\u00edrica\"). Para otros niveles de confianza ",
                "ese n\u00famero cambia: ~1.64 para 90%, ~2.58 para 99%.",
                tags$br(), tags$br(),
                strong("Pero ojo: "), "este m\u00f3dulo NO usa ese 2/1.96 \u2014 usa ",
                "la ", strong("t de Student"), ", que es mayor cuando n es ",
                "peque\u00f1o (porque adem\u00e1s de la variabilidad de los datos, ",
                "hay incertidumbre extra por no conocer \u03c3 real). Con n ",
                "grande, t converge a z; con n chico, son muy distintos:",
                tags$table(
                  class = "table table-sm table-bordered small mt-2 mb-1",
                  tags$thead(
                    tags$tr(
                      tags$th("Grados de libertad (n\u22121)",
                             style = paste0("background:", colores$primario,
                                           "; color:#fff;")),
                      tags$th("Valor cr\u00edtico exacto (95%)",
                             style = paste0("background:", colores$primario,
                                           "; color:#fff;"))
                    )
                  ),
                  tags$tbody(
                    tags$tr(tags$td("df = 1 (n=2, ej. 2 dantas)"), tags$td(strong("12.71"))),
                    tags$tr(tags$td("df = 9 (n=10)"), tags$td("2.26")),
                    tags$tr(tags$td("df = 30 (n=31)"), tags$td("2.04")),
                    tags$tr(tags$td("df \u2192 \u221e"), tags$td("1.96"))
                  )
                ),
                "Con solo 2 dantas, el valor real (12.71) no se parece en ",
                "nada al \"2\" de la aproximaci\u00f3n \u2014 por eso, con muestras ",
                "muy peque\u00f1as, ", strong("siempre"), " conviene usar t, ",
                "nunca la aproximaci\u00f3n z \u2248 2."
            )
          ),

          conector_infografia("¿Cómo se ve por dentro ese intervalo?"),

          paso_infografia(
            2, "arrows-angle-contract", colores$acento,
            "Anatom\u00eda de un intervalo de confianza",
            p(class = "mb-2",
              "Todo IC tiene tres partes:"
            ),
            fluidRow(
              column(4,
                div(style = paste0("text-align:center; background:",
                                  colores$fondo, "; border-radius:8px;",
                                  "padding:14px;"),
                    div(class = "small text-muted mb-1", "L\u00edmite inferior"),
                    div(style = paste0("font-size:1.6rem; font-weight:700;",
                                      "color:", colores$acento, ";"),
                        "164 kg")
                )
              ),
              column(4,
                div(style = paste0("text-align:center; background:",
                                  colores$fondo, "; border-radius:8px;",
                                  "padding:14px;"),
                    div(class = "small text-muted mb-1", "Estimado puntual"),
                    div(style = paste0("font-size:1.6rem; font-weight:700;",
                                      "color:", colores$primario, ";"),
                        "175 kg")
                )
              ),
              column(4,
                div(style = paste0("text-align:center; background:",
                                  colores$fondo, "; border-radius:8px;",
                                  "padding:14px;"),
                    div(class = "small text-muted mb-1", "L\u00edmite superior"),
                    div(style = paste0("font-size:1.6rem; font-weight:700;",
                                      "color:", colores$acento, ";"),
                        "186 kg")
                )
              )
            ),
            p(class = "mt-2 mb-0 text-muted small",
              "El ", strong("nivel de confianza"), " (ej. 95%) no describe ",
              "el ancho en s\u00ed \u2014 describe qu\u00e9 tan seguido este ",
              "procedimiento acierta (ver Paso 3)."
            )
          ),

          conector_infografia("La parte más importante: ¿qué significa realmente el 95%?"),

          fluidRow(
            column(6,
              tarjeta_concepto(
                "check-circle-fill", colores$secundario,
                "Interpretaci\u00f3n CORRECTA",
                p(class = "mb-0",
                  "Si repites el muestreo muchas veces y calculas un IC en ",
                  "cada caso, ", strong("el 95% de esos intervalos"),
                  " contendr\u00edan el verdadero valor de \u03bc. Es una ",
                  "propiedad del ", em("procedimiento"), ", no de un ",
                  "intervalo en particular."
                )
              )
            ),
            column(6,
              tarjeta_concepto(
                "x-circle-fill", colores$peligro,
                "Interpretaci\u00f3n INCORRECTA",
                p(class = "mb-0",
                  "\"Hay 95% de probabilidad de que ", em("este"),
                  " intervalo contenga \u03bc\" \u2014 esto no tiene sentido: una ",
                  "vez calculado, el intervalo ", strong("ya contiene \u03bc o no ",
                  "lo contiene"), " (no hay azar de por medio). El 95% se ",
                  "refiere al m\u00e9todo, no a este resultado espec\u00edfico."
                )
              )
            )
          ),

          tarjeta_concepto(
            "hand-index-thumb", colores$primario,
            "En la pr\u00e1ctica: t\u00fa solo vives con UN intervalo",
            p(class = "mb-0",
              "Es cierto que nunca repites el muestreo miles de veces \u2014 en ",
              "la vida real calculas un solo IC. Por eso la frase que s\u00ed se ",
              "usa (y es aceptable) es: ", strong("\"Puedo decir, con 95% de ",
              "confianza, que el verdadero valor de \u03bc est\u00e1 en este ",
              "rango.\""), " Su\u00e9na parecida a la interpretaci\u00f3n incorrecta, ",
              "pero la palabra clave es ", em("confianza"), ", no ", em(
              "probabilidad"), ": es una forma abreviada de decir \"este ",
              "intervalo sali\u00f3 de un m\u00e9todo que acierta el 95% de las ",
              "veces\" \u2014 no una afirmaci\u00f3n de que hay 95% de azar en ",
              "que \u03bc caiga aqu\u00ed. Es la forma pr\u00e1ctica y correcta de ",
              "reportar un IC en un art\u00edculo o informe."
            )
          ),

          conector_infografia("¿Y en el enfoque bayesiano? Ahí sí puedes decir \"probabilidad\""),

          fluidRow(
            column(6,
              tarjeta_concepto(
                "bar-chart-line", colores$primario,
                "IC frecuentista (este m\u00f3dulo)",
                p(class = "mb-0",
                  "\u03bc es un n\u00famero fijo (desconocido, pero no aleatorio). ",
                  "El intervalo es lo que var\u00eda de muestra en muestra. Por ",
                  "eso el 95% describe el ", em("procedimiento"), " repetido, ",
                  "nunca ", em("este"), " intervalo en particular."
                )
              )
            ),
            column(6,
              tarjeta_concepto(
                "graph-up", colores$acento,
                "Intervalo de credibilidad (StatBayes)",
                p(class = "mb-0",
                  "Aqu\u00ed \u03bc s\u00ed se trata como una variable aleatoria con su ",
                  "propia distribuci\u00f3n (la posterior). Por eso ", strong(
                  "s\u00ed"), " es correcto decir: \"hay 95% de probabilidad de ",
                  "que \u03bc est\u00e9 en este intervalo\" \u2014 la interpretaci\u00f3n ",
                  "intuitiva que much\u00edsima gente quiere darle al IC ",
                  "frecuentista, pero que solo es v\u00e1lida en el enfoque ",
                  "bayesiano."
                )
              )
            )
          ),

          div(
            class = "alert alert-secondary small mt-3 mb-0",
            bs_icon("signpost-split", class = "me-2",
                    style = paste0("color:", colores$primario)),
            strong("Otras formas de calcular un IC: "), "este m\u00f3dulo usa ",
            "la f\u00f3rmula cl\u00e1sica basada en la t de Student (asume que la ",
            "media muestral se distribuye aproximadamente normal). Pero ",
            "existen otros m\u00e9todos, \u00fatiles en distintas situaciones: ",
            tags$ul(class = "mb-0 mt-1",
              tags$li(strong("Log-normal: "), "para par\u00e1metros que no ",
                     "pueden ser negativos y suelen ser asim\u00e9tricos (ej. ",
                     "el tama\u00f1o poblacional N en StatAbundance)."),
              tags$li(strong("Perfil de verosimilitud: "), "m\u00e1s confiable ",
                     "que la f\u00f3rmula normal cuando la muestra es peque\u00f1a."),
              tags$li(strong("Bootstrap: "), "remuestrea tus propios datos ",
                     "miles de veces (como hiciste en Error Est\u00e1ndar), sin ",
                     "asumir ninguna distribuci\u00f3n de antemano.")
            )
          ),

          p(class = "text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en ", strong("Simulaci\u00f3n interactiva"),
            " \u2014 genera muchos IC de golpe y observa qu\u00e9 porcentaje ",
            "realmente atrapa el verdadero valor."
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Simulación interactiva
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"),
                        "Simulaci\u00f3n interactiva"),
        card_body(
          p(class = "small text-muted mb-3",
            "Se genera una poblaci\u00f3n con \u03bc conocido. Cada muestra ",
            "simulada produce su propio IC \u2014 los verdes SI contienen \u03bc, ",
            "los rojos NO."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("nivel_conf_sim_ic"), "Nivel de confianza:",
                            min = 0.80, max = 0.99, value = 0.95, step = 0.01,
                            post = "%", ticks = FALSE),
                sliderInput(ns("n_muestra_sim_ic"), "Tama\u00f1o de cada muestra (n):",
                            min = 5, max = 100, value = 20, step = 5),
                sliderInput(ns("reps_sim_ic"), "N\u00famero de IC simulados:",
                            min = 20, max = 100, value = 50, step = 10),
                actionButton(ns("regenerar_sim_ic"), "Nueva simulaci\u00f3n",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle")),
                tags$hr(),
                uiOutput(ns("cards_sim_ic"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_ic"), height = "420px"),
              uiOutput(ns("insight_sim_ic"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Los datos
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("table", class = "me-1"), "Los datos"),
        card_body(
          navset_pill(

            nav_panel(
              title = tagList(bs_icon("collection", class = "me-1"),
                              "Datos de ejemplo"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                div(
                  radioButtons(
                    ns("fuente_datos_ic"),
                    label   = tagList(bs_icon("database", class = "me-1"),
                                      "Elige un dataset:"),
                    choices = c(
                      "Ping\u00fcinos (palmerpenguins)"    = "penguins",
                      "Peso al nacer (MASS::birthwt)"      = "birthwt",
                      "Salarios (carData::Salaries)"        = "salaries",
                      "Actitud (datasets::attitude)"        = "attitude"
                    ),
                    selected = "penguins"
                  ),
                  uiOutput(ns("info_dataset_ic"))
                ),
                div(
                  uiOutput(ns("cards_datos_ic")),
                  DTOutput(ns("tabla_preview_ic"))
                )
              )
            ),

            nav_panel(
              title = tagList(bs_icon("upload", class = "me-1"), "Mis datos"),
              br(),
              fileInput(ns("archivo_ic"), "Sube un archivo CSV:",
                        accept = c(".csv")),
              tags$hr(),
              DTOutput(ns("tabla_preview_propio_ic"))
            ),

            nav_panel(
              title = tagList(bs_icon("list-columns-reverse", class = "me-1"),
                              "Tipos de variables"),
              br(),
              uiOutput(ns("tabla_tipos_ic")),
              div(class = "mt-3",
                actionButton(ns("aplicar_tipos_ic"), "Aplicar tipos",
                             class = "btn-primary btn-sm me-2"),
                actionButton(ns("resetear_tipos_ic"), "Restaurar originales",
                             class = "btn-outline-secondary btn-sm")
              ),
              uiOutput(ns("tipos_aplicados_msg_ic"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Practica con datos reales
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("clipboard-data", class = "me-1"),
                        "Practica con datos reales"),
        card_body(
          p(class = "small text-muted mb-3",
            "Elige una variable num\u00e9rica y un nivel de confianza para ",
            "construir su intervalo."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                uiOutput(ns("sel_var_ic")),
                sliderInput(ns("nivel_conf_ic"), "Nivel de confianza:",
                            min = 0.80, max = 0.99, value = 0.95, step = 0.01,
                            post = "%", ticks = FALSE),
                actionButton(ns("calcular_ic"), "Calcular IC",
                             class = "btn-primary w-100 btn-sm",
                             icon  = icon("play")),
                tags$hr(),
                uiOutput(ns("cards_practica_ic"))
              )
            ),
            div(
              plotOutput(ns("plot_practica_ic"), height = "260px"),
              uiOutput(ns("insight_practica_ic"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 5: Código R
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "C\u00f3digo R"),
        card_body(
          p(class = "small text-muted mb-2",
            "El c\u00f3digo R equivalente a lo que hiciste en ",
            strong("Practica con datos reales"), "."
          ),
          verbatimTextOutput(ns("codigo_r_ic")),
          downloadButton(ns("descargar_script_ic"), "Descargar script .R",
                         class = "btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_intervalos_confianza_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Simulación interactiva (cobertura) ────────────
    poblacion_sim_ic <- reactive({
      input$regenerar_sim_ic
      stats::rnorm(100000, mean = 175, sd = 15)
    })

    simulacion_ic <- reactive({
      pob   <- poblacion_sim_ic()
      n     <- input$n_muestra_sim_ic
      reps  <- input$reps_sim_ic
      conf  <- input$nivel_conf_sim_ic
      req(pob, n, reps, conf)
      mu <- mean(pob)

      resultados <- lapply(seq_len(reps), function(i) {
        muestra <- sample(pob, n, replace = TRUE)
        ic <- ic_media_t(muestra, conf)
        data.frame(rep = i, li = ic$li, ls = ic$ls, media = ic$media,
                  contiene = (ic$li <= mu && mu <= ic$ls))
      })
      df <- do.call(rbind, resultados)
      list(df = df, mu = mu)
    })

    output$cards_sim_ic <- renderUI({
      res <- simulacion_ic()
      cobertura <- mean(res$df$contiene) * 100
      tagList(
        tarjeta_metrica("Media poblacional (\u03bc)", round(res$mu, 1), "media"),
        tarjeta_metrica("Cobertura observada",
                        paste0(round(cobertura, 1), "%"), "error_estandar",
                        ultima = TRUE)
      )
    })

    output$plot_sim_ic <- renderPlot({
      res  <- simulacion_ic()
      df   <- res$df
      conf <- input$nivel_conf_sim_ic
      ggplot(df, aes(x = li, xend = ls, y = rep, yend = rep,
                    color = contiene)) +
        geom_vline(xintercept = res$mu, linetype = "dashed",
                  color = colores$peligro, linewidth = 0.8) +
        geom_segment(linewidth = 1) +
        geom_point(aes(x = media, y = rep), size = 0.8) +
        scale_color_manual(values = c(`TRUE` = colores$secundario,
                                     `FALSE` = colores$peligro),
                          labels = c(`TRUE` = "Contiene \u03bc",
                                    `FALSE` = "No contiene \u03bc"),
                          name = NULL) +
        labs(x = "Valor", y = "Muestra simulada",
             title = paste0("IC al ", round(conf * 100), "% \u2014 ",
                           input$reps_sim_ic, " muestras simuladas")) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top",
              plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 13))
    })

    output$insight_sim_ic <- renderUI({
      res  <- simulacion_ic()
      conf <- input$nivel_conf_sim_ic
      cobertura <- mean(res$df$contiene) * 100
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("Pediste un nivel de confianza del ", round(conf * 100),
                "% \u2014 y de los ", nrow(res$df), " IC simulados, el ",
                round(cobertura, 1), "% efectivamente contuvo a \u03bc. Con ",
                "pocas repeticiones puede no coincidir exacto (es ",
                "variabilidad de Monte Carlo); sube el n\u00famero de IC ",
                "simulados para que se acerque m\u00e1s al ", round(conf * 100),
                "% te\u00f3rico.")
      )
    })

    # ── Datos reactivos (ejemplo) ─────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_ic
      req(!is.null(fuente) && nchar(fuente) > 0)
      df <- switch(fuente,
        penguins = as.data.frame(palmerpenguins::penguins),
        birthwt  = MASS::birthwt,
        salaries = carData::Salaries,
        attitude = datasets::attitude
      )
      df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
    })

    datos_mod <- reactiveVal(NULL)
    observeEvent(datos(), { datos_mod(datos()) })

    output$info_dataset_ic <- renderUI({
      fuente <- input$fuente_datos_ic
      if (is.null(fuente)) return(NULL)
      switch(fuente,
        penguins = div(class = "alert alert-info small py-2 px-3 mb-2",
          strong("Ping\u00fcinos: "), "medidas corporales de 3 especies."),
        birthwt = div(class = "alert alert-info small py-2 px-3 mb-2",
          strong("Peso al nacer: "), "189 nacimientos y factores de riesgo."),
        salaries = div(class = "alert alert-warning small py-2 px-3 mb-2",
          strong("Salarios: "), "sesgado a la derecha."),
        attitude = div(class = "alert alert-info small py-2 px-3 mb-2",
          strong("Actitud: "), "encuesta de clima laboral.")
      )
    })

    output$cards_datos_ic <- renderUI({
      req(datos_mod())
      d    <- datos_mod()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(col) is.factor(col) || is.character(col)))
      layout_columns(
        col_widths = c(4, 4, 4),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$primario, "; font-weight:700;"),
                  nrow(d)),
               p(class = "small text-muted mb-0", "Observaciones"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$acento, "; font-weight:700;"),
                  nnum),
               p(class = "small text-muted mb-0", "Num\u00e9ricas"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                  ncat),
               p(class = "small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_ic <- renderDT({
      req(datos_mod())
      datatable(datos_mod(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    datos_propio_ic <- reactive({
      req(input$archivo_ic)
      df <- readr::read_csv(input$archivo_ic$datapath, show_col_types = FALSE)
      df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
    })

    observeEvent(input$archivo_ic, {
      req(datos_propio_ic())
      datos_mod(as.data.frame(datos_propio_ic()))
    })

    output$tabla_preview_propio_ic <- renderDT({
      req(datos_propio_ic())
      datatable(datos_propio_ic(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    tipos_usuario_ic <- reactiveVal(NULL)

    output$tabla_tipos_ic <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_ic()
      filas <- lapply(names(d), function(nm) {
        col    <- d[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        sel    <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual
        tags$tr(
          tags$td(strong(nm)),
          tags$td(class = "text-muted small", actual),
          tags$td(
            selectInput(ns(paste0("tipo_", nm)), NULL,
                        choices  = c("Num\u00e9rica" = "numeric", "Factor" = "factor"),
                        selected = sel, width = "160px")
          )
        )
      })
      tags$table(
        class = "table table-sm table-bordered small",
        tags$thead(tags$tr(tags$th("Variable"), tags$th("Tipo actual"),
                           tags$th("Nuevo tipo"))),
        tags$tbody(filas)
      )
    })

    observeEvent(input$aplicar_tipos_ic, {
      req(datos_mod())
      d  <- datos_mod()
      tu <- list()
      for (nm in names(d)) {
        nuevo <- input[[paste0("tipo_", nm)]]
        req(!is.null(nuevo))
        tu[[nm]] <- nuevo
        d[[nm]]  <- if (nuevo == "factor") as.factor(d[[nm]])
                    else as.numeric(as.character(d[[nm]]))
      }
      tipos_usuario_ic(tu)
      datos_mod(d)
      output$tipos_aplicados_msg_ic <- renderUI({
        div(class = "alert alert-success small mt-3",
            bs_icon("check-circle-fill", class = "me-1"),
            "Tipos de variable aplicados.")
      })
    })

    observeEvent(input$resetear_tipos_ic, {
      tipos_usuario_ic(NULL)
      datos_mod(datos())
      output$tipos_aplicados_msg_ic <- renderUI({
        div(class = "alert alert-info small mt-3",
            bs_icon("arrow-counterclockwise", class = "me-1"),
            "Tipos restaurados a los originales.")
      })
    })

    # ── Practica con datos reales ──────────────────────
    vars_numericas_ic <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    output$sel_var_ic <- renderUI({
      req(vars_numericas_ic())
      selectInput(ns("var_ic"), "Variable num\u00e9rica:",
                  choices = vars_numericas_ic())
    })

    valores_ic <- reactive({
      req(datos_mod(), input$var_ic)
      x <- datos_mod()[[input$var_ic]]
      x[!is.na(x)]
    })

    resultado_practica_ic <- eventReactive(input$calcular_ic, {
      x    <- valores_ic()
      conf <- input$nivel_conf_ic
      req(x, conf)
      ic_media_t(x, conf)
    })

    output$cards_practica_ic <- renderUI({
      req(resultado_practica_ic())
      ic <- resultado_practica_ic()
      tagList(
        tarjeta_metrica("L\u00edmite inferior", round(ic$li, 2), "media"),
        tarjeta_metrica("Estimado (media)", round(ic$media, 2), "media"),
        tarjeta_metrica("L\u00edmite superior", round(ic$ls, 2), "media",
                        ultima = TRUE)
      )
    })

    output$plot_practica_ic <- renderPlot({
      req(resultado_practica_ic())
      ic <- resultado_practica_ic()
      df <- data.frame(x = 1, media = ic$media, li = ic$li, ls = ic$ls)
      ggplot(df, aes(x = x, y = media)) +
        geom_errorbar(aes(ymin = li, ymax = ls), width = 0.15,
                     color = colores$primario, linewidth = 1) +
        geom_point(size = 4, color = colores$acento) +
        scale_x_continuous(limits = c(0.5, 1.5), breaks = NULL) +
        labs(x = NULL, y = input$var_ic,
             title = paste0("Estimado e IC al ",
                           round(input$nivel_conf_ic * 100), "%")) +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 13))
    })

    output$insight_practica_ic <- renderUI({
      req(resultado_practica_ic())
      ic <- resultado_practica_ic()
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("Con ", round(input$nivel_conf_ic * 100), "% de confianza, ",
                "el verdadero valor poblacional de ", input$var_ic,
                " est\u00e1 entre ", round(ic$li, 2), " y ", round(ic$ls, 2),
                ". El margen de error fue \u00b1", round(ic$margen, 2),
                " (t = ", round(ic$tval, 2), " \u00d7 EE = ", round(ic$se, 2),
                ", con ", ic$gl, " grados de libertad).")
      )
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_ic <- renderText({
      req(input$var_ic, input$nivel_conf_ic)
      fuente <- input$fuente_datos_ic
      es_propio <- !is.null(datos_propio_ic()) && !is.null(input$archivo_ic)
      nm_datos <- if (es_propio) "mis_datos" else fuente
      linea_carga <- if (es_propio) {
        "# datos <- readr::read_csv('mis_datos.csv')"
      } else {
        switch(fuente,
          penguins = "penguins <- as.data.frame(palmerpenguins::penguins)",
          birthwt  = "birthwt  <- MASS::birthwt",
          salaries = "salaries <- carData::Salaries",
          attitude = "attitude <- datasets::attitude"
        )
      }
      paste0(
        "# \u2500\u2500 Intervalo de confianza para una media \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "# Dataset: ", nm_datos, "\n",
        "# Variable: ", input$var_ic, "\n\n",
        linea_carga, "\n\n",
        "x    <- ", nm_datos, "$", input$var_ic, "\n",
        "x    <- x[!is.na(x)]\n",
        "n    <- length(x)\n",
        "conf <- ", input$nivel_conf_ic, "\n\n",
        "media  <- mean(x)\n",
        "se     <- sd(x) / sqrt(n)\n",
        "gl     <- n - 1\n",
        "tval   <- qt(1 - (1 - conf) / 2, df = gl)\n",
        "margen <- tval * se\n\n",
        "c(li = media - margen, media = media, ls = media + margen)\n"
      )
    })

    output$descargar_script_ic <- downloadHandler(
      filename = function() paste0("StatBasics_intervalos_confianza_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_ic(), file)
    )

  })
}
