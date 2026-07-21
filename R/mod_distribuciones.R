# ============================================================
# mod_distribuciones.R — Distribuciones de probabilidad
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Simulación interactiva (teórica, con sliders)
#   3. Los datos (Datos de ejemplo + Mis datos + Tipos de variables)
#   4. Identificar la distribución (con fitdistrplus)
#   5. Código R
#
# Nota pedagógica: la pestaña "Identificar la distribución" usa
# el gráfico de Cullen y Frey (asimetría² vs. curtosis) en vez de
# un clasificador de caja negra (como performance::check_distribution()),
# para que el estudiante vea el razonamiento — y reutiliza
# directamente los conceptos de asimetría ya vistos en
# mod_tendencia_central.R.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_distribuciones_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("distribute-vertical", class = "me-2"),
        "Distribuciones de probabilidad",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Una distribuci\u00f3n de probabilidad describe qu\u00e9 valores puede ",
        "tomar una variable y ", strong("qu\u00e9 tan probable"), " es cada ",
        "uno. Elegir la distribuci\u00f3n correcta es la base de casi todo ",
        "modelo estad\u00edstico."
      )
    ),

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Distribuciones de probabilidad comunes"),
          p(class = "small text-muted mb-3",
            "Cada una modela un tipo distinto de proceso aleatorio."
          ),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-4",
              style = "background: #ffffff;",
              tags$thead(
                tags$tr(
                  style = paste0("background:", colores$primario,
                                "; color: #ffffff;"),
                  tags$th("Distribuci\u00f3n",
                         style = paste0("background:", colores$primario,
                                       "; color: #ffffff;")),
                  tags$th("Par\u00e1metros",
                         style = paste0("background:", colores$primario,
                                       "; color: #ffffff;")),
                  tags$th("Cu\u00e1ndo se usa",
                         style = paste0("background:", colores$primario,
                                       "; color: #ffffff;"))
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(colspan = "3",
                         style = paste0("background:", colores$secundario,
                                       "; color: #ffffff; font-weight:700;"),
                         "Continuas")
                ),
                tags$tr(
                  tags$td(strong("Normal")),
                  tags$td("Media (\u03bc), desv. est\u00e1ndar (\u03c3)"),
                  tags$td("Muchos fen\u00f3menos naturales; base de la ",
                          "inferencia cl\u00e1sica")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("t de Student")),
                  tags$td("Grados de libertad (df)"),
                  tags$td("Como la normal, pero con colas m\u00e1s pesadas ",
                          "\u2014 muestras peque\u00f1as")
                ),
                tags$tr(
                  tags$td(strong("Chi-cuadrada")),
                  tags$td("Grados de libertad (df)"),
                  tags$td("Suma de normales al cuadrado; pruebas de ",
                          "varianza y bondad de ajuste")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Exponencial")),
                  tags$td("Tasa (\u03bb)"),
                  tags$td("Tiempo entre eventos independientes (ej.: ",
                          "tiempo de espera, tiempo hasta una falla)")
                ),
                tags$tr(
                  tags$td(strong("Gamma")),
                  tags$td("Forma (shape), tasa (rate)"),
                  tags$td("Variables positivas y asim\u00e9tricas a la ",
                          "derecha (ej.: tiempos de espera acumulados, ",
                          "costos)")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Lognormal")),
                  tags$td("Media y sd de log(x)"),
                  tags$td("Variables positivas cuyo logaritmo es normal ",
                          "(ej.: ingresos, concentraciones)")
                ),
                tags$tr(
                  tags$td(strong("Beta")),
                  tags$td("Forma 1 y forma 2 (shape1, shape2)"),
                  tags$td("Proporciones o tasas \u2014 valores estrictamente ",
                          "entre 0 y 1 (ej.: porcentajes, probabilidades)")
                ),
                tags$tr(
                  tags$td(colspan = "3",
                         style = paste0("background:", colores$secundario,
                                       "; color: #ffffff; font-weight:700;"),
                         "Discretas")
                ),
                tags$tr(
                  tags$td(strong("Binomial")),
                  tags$td("n ensayos, p probabilidad de \u00e9xito"),
                  tags$td("Conteo de \u00e9xitos en n ensayos independientes ",
                          "(s\u00ed/no)")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Poisson")),
                  tags$td("\u03bb (tasa promedio de ocurrencia)"),
                  tags$td("Conteos raros en un intervalo de tiempo/espacio")
                )
              )
            )
          ),

          div(
            class = "alert alert-secondary small py-2 px-3 mb-3",
            bs_icon("info-circle", class = "me-1",
                    style = paste0("color:", colores$primario)),
            strong("\u00bfQu\u00e9 es una \"cola pesada\"? "),
            "Los valores extremos (muy alejados de la media) son ",
            strong("m\u00e1s probables"), " de lo que predice la normal. La ",
            "curva de densidad no cae tan r\u00e1pido hacia cero en los ",
            "extremos \u2014 se queda \"m\u00e1s alta\" m\u00e1s lejos del ",
            "centro. Por eso, con colas pesadas es m\u00e1s com\u00fan ",
            "observar un dato sorprendentemente grande o peque\u00f1o de lo ",
            "que esperar\u00edas con una normal."
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfDiscreta o continua?"),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("bar-chart-steps", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Discreta"), br(),
              "Solo toma valores puntuales (0, 1, 2, ...). Ej.: n\u00famero ",
              "de \u00e9xitos, conteos."
            ),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("graph-up", class = "me-1",
                      style = paste0("color:", colores$acento)),
              strong("Continua"), br(),
              "Puede tomar cualquier valor en un rango. Ej.: peso, ",
              "altura, tiempo."
            )
          ),

          p(class = "small text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en la pesta\u00f1a ",
            strong("Simulaci\u00f3n interactiva"), " o descubre qu\u00e9 ",
            "distribuci\u00f3n sigue una variable real en ",
            strong("Identificar la distribuci\u00f3n"), "."
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 2: Simulación interactiva (teórica)
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("sliders", class = "me-1"),
                        "Simulaci\u00f3n interactiva"),
        card_body(
          p(class = "small text-muted mb-3",
            "Elige una distribuci\u00f3n y mueve sus par\u00e1metros para ver ",
            "en vivo c\u00f3mo cambia su forma."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                selectInput(
                  ns("familia_sim_dist"), "Distribuci\u00f3n:",
                  choices = list(
                    "Continuas" = c("Normal", "t de Student", "Chi-cuadrada",
                                   "Exponencial", "Gamma", "Lognormal", "Beta"),
                    "Discretas" = c("Binomial", "Poisson")
                  ),
                  selected = "Normal"
                ),
                uiOutput(ns("parametros_sim_dist")),
                sliderInput(ns("n_sim_dist"), "Repeticiones de la simulaci\u00f3n (n):",
                            min = 20, max = 2000, value = 500, step = 20),
                actionButton(ns("regenerar_sim_dist"),
                             "Nueva muestra aleatoria",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_dist"), height = "380px"),
              uiOutput(ns("insight_sim_dist"))
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

            # ── Sub 1: Datos de ejemplo ─────────────
            nav_panel(
              title = tagList(bs_icon("collection", class = "me-1"),
                              "Datos de ejemplo"),
              br(),
              layout_columns(
                col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
                div(
                  radioButtons(
                    ns("fuente_datos_dist"),
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
                  uiOutput(ns("info_dataset_dist"))
                ),
                div(
                  uiOutput(ns("cards_datos_dist")),
                  DTOutput(ns("tabla_preview_dist"))
                )
              )
            ),

            # ── Sub 2: Mis datos ─────────────────────
            nav_panel(
              title = tagList(bs_icon("folder2-open", class = "me-1"), "Mis datos"),
              br(),
              layout_columns(
                col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
                div(
                  p(class = "small text-muted mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    "Sube un archivo CSV o Excel. ",
                    "La primera fila debe contener los nombres de las columnas."),
                  fileInput(
                    ns("archivo_dist"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar\u2026",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador_dist"),
                    label   = "Separador (CSV):",
                    choices = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    )
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio_dist"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio_dist")),
                    br(),
                    DTOutput(ns("tabla_preview_propio_dist"))
                  )
                )
              )
            ),

            # ── Sub 3: Tipos de variables ────────────
            nav_panel(
              title = tagList(bs_icon("list-columns-reverse", class = "me-1"),
                              "Tipos de variables"),
              br(),
              uiOutput(ns("tabla_tipos_dist")),
              div(class = "mt-3",
                actionButton(ns("aplicar_tipos_dist"), "Aplicar tipos",
                             class = "btn-primary btn-sm me-2"),
                actionButton(ns("resetear_tipos_dist"), "Restaurar originales",
                             class = "btn-outline-secondary btn-sm")
              ),
              uiOutput(ns("tipos_aplicados_msg_dist"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Identificar la distribución
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("search", class = "me-1"),
                        "Identificar la distribuci\u00f3n"),
        card_body(
          p(class = "small text-muted mb-3",
            "Elige una variable num\u00e9rica del dataset activo. Primero se ",
            "ubica en el ", strong("gr\u00e1fico de Cullen y Frey"),
            " (asimetr\u00eda\u00b2 vs. curtosis) para ver qu\u00e9 ",
            "distribuciones son candidatas plausibles \u2014 luego se ",
            "ajustan por m\u00e1xima verosimilitud y se comparan."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(3, 9)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_dist")),
                uiOutput(ns("tipo_var_dist_ui")),
                uiOutput(ns("sel_candidatas_dist")),
                actionButton(ns("ajustar_dist"), "Ajustar y comparar",
                             class = "btn-primary w-100 btn-sm",
                             icon  = icon("play"))
              )
            ),
            div(
              plotOutput(ns("plot_cullen_frey_dist"), height = "480px"),
              uiOutput(ns("insight_cullen_frey_dist"))
            )
          ),
          uiOutput(ns("resumen_conteo_dist")),
          tags$hr(),
          uiOutput(ns("resultado_ajuste_dist"))
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
            strong("Identificar la distribuci\u00f3n"), "."
          ),
          verbatimTextOutput(ns("codigo_r_dist")),
          downloadButton(ns("descargar_script_dist"), "Descargar script .R",
                         class = "btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_distribuciones_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Simulación interactiva (teórica) ──────────────
    output$parametros_sim_dist <- renderUI({
      switch(input$familia_sim_dist,
        "Normal" = tagList(
          sliderInput(ns("media_sim_dist"), "Media (\u03bc):",
                      min = -20, max = 20, value = 0, step = 1),
          sliderInput(ns("sd_sim_dist"), "Desv. est\u00e1ndar (\u03c3):",
                      min = 1, max = 15, value = 3, step = 0.5)
        ),
        "Binomial" = tagList(
          sliderInput(ns("size_sim_dist"), "Ensayos por experimento (size):",
                      min = 1, max = 100, value = 20, step = 1),
          sliderInput(ns("prob_sim_dist"), "Probabilidad de \u00e9xito (p):",
                      min = 0.01, max = 0.99, value = 0.5, step = 0.01)
        ),
        "Poisson" = sliderInput(ns("lambda_sim_dist"), "Tasa (\u03bb):",
                                min = 0.5, max = 30, value = 4, step = 0.5),
        "t de Student" = sliderInput(ns("df_t_sim_dist"),
                                     "Grados de libertad (df):",
                                     min = 1, max = 60, value = 5, step = 1),
        "Chi-cuadrada" = sliderInput(ns("df_chi_sim_dist"),
                                     "Grados de libertad (df):",
                                     min = 1, max = 30, value = 3, step = 1),
        "Exponencial" = sliderInput(ns("rate_exp_sim_dist"), "Tasa (\u03bb):",
                                    min = 0.1, max = 5, value = 1, step = 0.1),
        "Gamma" = tagList(
          sliderInput(ns("shape_gamma_sim_dist"), "Forma (shape):",
                      min = 0.5, max = 10, value = 2, step = 0.5),
          sliderInput(ns("rate_gamma_sim_dist"), "Tasa (rate):",
                      min = 0.1, max = 5, value = 1, step = 0.1)
        ),
        "Lognormal" = tagList(
          sliderInput(ns("meanlog_sim_dist"), "Media de log(x):",
                      min = -2, max = 3, value = 0, step = 0.1),
          sliderInput(ns("sdlog_sim_dist"), "SD de log(x):",
                      min = 0.1, max = 2, value = 0.5, step = 0.1)
        ),
        "Beta" = tagList(
          sliderInput(ns("shape1_beta_sim_dist"), "Forma 1 (shape1):",
                      min = 0.5, max = 10, value = 2, step = 0.5),
          sliderInput(ns("shape2_beta_sim_dist"), "Forma 2 (shape2):",
                      min = 0.5, max = 10, value = 2, step = 0.5)
        )
      )
    })

    datos_sim_dist <- reactive({
      input$regenerar_sim_dist
      fam <- input$familia_sim_dist
      n   <- input$n_sim_dist
      req(fam, n)

      switch(fam,
        "Normal" = {
          req(input$media_sim_dist, input$sd_sim_dist)
          list(x = stats::rnorm(n, input$media_sim_dist, input$sd_sim_dist),
               discreta = FALSE)
        },
        "Binomial" = {
          req(input$size_sim_dist, input$prob_sim_dist)
          list(x = stats::rbinom(n, input$size_sim_dist, input$prob_sim_dist),
               discreta = TRUE)
        },
        "Poisson" = {
          req(input$lambda_sim_dist)
          list(x = stats::rpois(n, input$lambda_sim_dist), discreta = TRUE)
        },
        "t de Student" = {
          req(input$df_t_sim_dist)
          list(x = stats::rt(n, input$df_t_sim_dist), discreta = FALSE)
        },
        "Chi-cuadrada" = {
          req(input$df_chi_sim_dist)
          list(x = stats::rchisq(n, input$df_chi_sim_dist), discreta = FALSE)
        },
        "Exponencial" = {
          req(input$rate_exp_sim_dist)
          list(x = stats::rexp(n, input$rate_exp_sim_dist), discreta = FALSE)
        },
        "Gamma" = {
          req(input$shape_gamma_sim_dist, input$rate_gamma_sim_dist)
          list(x = stats::rgamma(n, shape = input$shape_gamma_sim_dist,
                                rate = input$rate_gamma_sim_dist),
               discreta = FALSE)
        },
        "Lognormal" = {
          req(input$meanlog_sim_dist, input$sdlog_sim_dist)
          list(x = stats::rlnorm(n, input$meanlog_sim_dist, input$sdlog_sim_dist),
               discreta = FALSE)
        },
        "Beta" = {
          req(input$shape1_beta_sim_dist, input$shape2_beta_sim_dist)
          list(x = stats::rbeta(n, input$shape1_beta_sim_dist,
                                input$shape2_beta_sim_dist),
               discreta = FALSE)
        }
      )
    })

    output$plot_sim_dist <- renderPlot({
      d <- datos_sim_dist()
      req(length(d$x) > 1)
      fam <- input$familia_sim_dist

      if (!d$discreta) {
        df <- data.frame(x = d$x)
        p <- ggplot(df, aes(x = x)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30,
                         fill = colores$secundario, color = "white",
                         alpha = 0.85) +
          geom_density(color = colores$primario, linewidth = 0.9)
      } else {
        tab <- as.data.frame(table(d$x))
        names(tab) <- c("x", "freq")
        tab$x <- as.numeric(as.character(tab$x))
        tab$dens <- tab$freq / sum(tab$freq)
        p <- ggplot(tab, aes(x = x, y = dens)) +
          geom_col(fill = colores$secundario, color = "white", alpha = 0.85,
                   width = 0.7)
      }

      p + labs(x = "Valor", y = if (d$discreta) "Proporci\u00f3n" else "Densidad",
               title = paste0("Distribuci\u00f3n ", fam)) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 17))
    })

    output$insight_sim_dist <- renderUI({
      d <- datos_sim_dist()
      req(length(d$x) > 1)
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          if (d$discreta) {
            paste0("Es una distribuci\u00f3n discreta: solo toma valores ",
                   "enteros. Media observada: ", round(mean(d$x), 2),
                   " | Varianza observada: ", round(stats::var(d$x), 2))
          } else {
            paste0("Es una distribuci\u00f3n continua: puede tomar ",
                   "cualquier valor en su rango. Media observada: ",
                   round(mean(d$x), 2), " | SD observada: ",
                   round(stats::sd(d$x), 2))
          }
      )
    })

    # ── Datos reactivos (ejemplo) ─────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_dist
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

    # ── Info dataset ──────────────────────────────────
    output$info_dataset_dist <- renderUI({
      fuente <- input$fuente_datos_dist
      if (is.null(fuente)) return(NULL)
      switch(fuente,
        penguins = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          strong("Ping\u00fcinos: "), "medidas corporales de 3 especies ",
          "de ping\u00fcinos en el archipi\u00e9lago Palmer, Ant\u00e1rtida."
        ),
        birthwt = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          strong("Peso al nacer: "), "datos de 189 nacimientos y factores ",
          "de riesgo de bajo peso al nacer."
        ),
        salaries = div(
          class = "alert alert-warning small py-2 px-3 mb-2",
          strong("Salarios: "), "sesgado a la derecha \u2014 la mayor\u00eda ",
          "de salarios profesorales son moderados, con algunos muy altos."
        ),
        attitude = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          strong("Actitud: "), "puntajes de encuesta de clima laboral en ",
          "una oficina de seguros."
        )
      )
    })

    output$cards_datos_dist <- renderUI({
      req(datos_mod())
      d    <- datos_mod()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(col) is.factor(col) || is.character(col)))
      layout_columns(
        col_widths = breakpoints(sm = c(12, 12, 12), md = c(4, 4, 4)),
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

    output$tabla_preview_dist <- renderDT({
      req(datos_mod())
      datatable(datos_mod(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Mis datos ──────────────────────────────────────
    datos_propio_dist <- reactive({
      req(input$archivo_dist)
      ext <- tools::file_ext(input$archivo_dist$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_dist$datapath)
        else
          readr::read_delim(input$archivo_dist$datapath,
                            delim = input$separador_dist %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error al leer archivo:", e$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio_dist(), {
      req(datos_propio_dist())
      datos_mod(as.data.frame(datos_propio_dist()))
    })

    output$resumen_datos_propio_dist <- renderUI({
      req(datos_propio_dist())
      d <- datos_propio_dist()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_dist <- renderUI({
      req(datos_propio_dist())
      d    <- datos_propio_dist()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
      layout_columns(
        col_widths = breakpoints(sm = c(12, 12, 12), md = c(4, 4, 4)),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$primario, "; font-weight:700;"), nrow(d)),
               p(class = "small text-muted mb-0", "Observaciones"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$acento, "; font-weight:700;"), nnum),
               p(class = "small text-muted mb-0", "Num\u00e9ricas"))),
        card(class = "text-center",
             card_body(class = "p-2",
               h3(style = paste0("color:", colores$secundario, "; font-weight:700;"), ncat),
               p(class = "small text-muted mb-0", "Categ\u00f3ricas")))
      )
    })

    output$tabla_preview_propio_dist <- renderDT({
      req(datos_propio_dist())
      datatable(datos_propio_dist(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_dist <- reactiveVal(NULL)

    output$tabla_tipos_dist <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_dist()
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

    observeEvent(input$aplicar_tipos_dist, {
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
      tipos_usuario_dist(tu)
      datos_mod(d)
      output$tipos_aplicados_msg_dist <- renderUI({
        div(class = "alert alert-success small mt-3",
            bs_icon("check-circle-fill", class = "me-1"),
            "Tipos de variable aplicados.")
      })
    })

    observeEvent(input$resetear_tipos_dist, {
      tipos_usuario_dist(NULL)
      datos_mod(datos())
      output$tipos_aplicados_msg_dist <- renderUI({
        div(class = "alert alert-info small mt-3",
            bs_icon("arrow-counterclockwise", class = "me-1"),
            "Tipos restaurados a los originales.")
      })
    })

    # ── Identificar la distribución ────────────────────
    vars_numericas_dist <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    output$sel_var_dist <- renderUI({
      req(vars_numericas_dist())
      selectInput(ns("var_dist"), "Variable num\u00e9rica:",
                  choices = vars_numericas_dist())
    })

    valores_dist <- reactive({
      req(datos_mod(), input$var_dist)
      x <- datos_mod()[[input$var_dist]]
      x[!is.na(x)]
    })

    # Sugerencia autom\u00e1tica al cambiar de variable, pero el usuario
    # siempre puede corregirla manualmente (ej. peso en gramos es
    # continua aunque sean valores enteros).
    output$tipo_var_dist_ui <- renderUI({
      req(valores_dist())
      radioButtons(ns("tipo_var_dist"), "Tipo de variable:",
                   choices  = c("Continua" = "continua",
                               "Conteo (discreta)" = "conteo"),
                   selected = sugerir_tipo_variable(valores_dist()),
                   inline   = TRUE)
    })

    es_conteo_dist <- reactive({
      req(input$tipo_var_dist)
      input$tipo_var_dist == "conteo"
    })

    output$sel_candidatas_dist <- renderUI({
      req(valores_dist(), input$tipo_var_dist)
      opciones <- candidatas_disponibles(valores_dist(), input$tipo_var_dist)
      # names = etiqueta legible (se muestra); values = c\u00f3digo interno
      # (se devuelve en input$candidatas_dist)
      opciones_choices <- stats::setNames(opciones,
                                          etiquetas_distribuciones[opciones])
      checkboxGroupInput(ns("candidatas_dist"), "Distribuciones a comparar:",
                          choices  = opciones_choices,
                          selected = opciones_choices)
    })

    output$plot_cullen_frey_dist <- renderPlot({
      ancho <- session$clientData[[paste0("output_", ns("plot_cullen_frey_dist"),
                                          "_width")]]
      req(ancho, ancho > 0)
      x <- valores_dist()
      req(length(x) > 4)
      graphics::par(mar = c(4, 4, 3, 8))
      fitdistrplus::descdist(x, boot = 500, graph = TRUE,
                             discrete = es_conteo_dist())
    })

    output$resumen_conteo_dist <- renderUI({
      req(es_conteo_dist())
      x <- valores_dist()
      r <- resumen_dispersion_conteo(x)
      sobredispersa <- r$indice > 1.5
      tagList(
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12, 12), md = c(4, 4, 4)),
          tarjeta_metrica("Media", round(r$media, 2), "media"),
          tarjeta_metrica("Varianza", round(r$varianza, 2), "varianza"),
          div(
            class = paste0("alert small py-2 px-3 mb-0 ",
                          if (sobredispersa) "alert-warning" else "alert-info"),
            strong("\u00cdndice de dispersi\u00f3n (var/media): "),
            round(r$indice, 2),
            tags$br(),
            tags$span(class = "text-muted", style = "font-size: 0.78rem;",
                      "Poisson asume que var = media (\u00edndice \u2248 1).")
          )
        ),
        div(class = paste0("alert small mt-2 ",
                          if (sobredispersa) "alert-warning" else "alert-info"),
            bs_icon("lightbulb-fill", class = "me-1"),
            if (sobredispersa) {
              paste0("Hay sobredispersi\u00f3n (varianza mucho mayor que la ",
                    "media) \u2014 la Binomial Negativa suele ajustar mejor ",
                    "que Poisson en este caso. Este es exactamente el ",
                    "razonamiento detr\u00e1s de elegir la familia binomial ",
                    "negativa en un GLM/GLMM (StatModels/StatBayes).")
            } else {
              paste0("La varianza es cercana a la media \u2014 Poisson es ",
                    "un modelo razonable para estos datos.")
            }
        )
      )
    })

    output$insight_cullen_frey_dist <- renderUI({
      x <- valores_dist()
      req(length(x) > 4)
      if (es_conteo_dist()) {
        div(class = "alert alert-info small mt-2",
            bs_icon("lightbulb-fill", class = "me-1"),
            "Como es una variable de conteos (enteros \u2265 0), aqu\u00ed se ",
            "compara contra Poisson y Binomial Negativa \u2014 revisa el ",
            "\u00edndice de dispersi\u00f3n abajo para saber cu\u00e1l es m\u00e1s ",
            "adecuada."
        )
      } else {
        div(class = "alert alert-info small mt-2",
            bs_icon("lightbulb-fill", class = "me-1"),
            strong("C\u00f3mo leer el gr\u00e1fico:"),
            tags$ul(
              class = "mb-1 ps-3",
              tags$li(strong("Punto rojo (\"Observation\"): "), "tu variable. ",
                     "Los puntos naranjas alrededor son remuestreos ",
                     "bootstrap que reflejan la incertidumbre de esa ",
                     "ubicaci\u00f3n."),
              tags$li(strong("S\u00edmbolos fijos "),
                     "(normal *, uniforme \u25b3, exponencial \u2612, ",
                     "log\u00edstica +): cada uno es un \u00fanico valor ",
                     "te\u00f3rico de asimetr\u00eda\u00b2/curtosis \u2014 esas ",
                     "distribuciones no tienen un par\u00e1metro de forma ",
                     "que las cambie."),
              tags$li(strong("L\u00edneas "), "(lognormal punteada, gamma ",
                     "guionada): representan un rango de formas posibles, ",
                     "seg\u00fan su par\u00e1metro de forma."),
              tags$li(strong("\u00c1rea gris: "), "regi\u00f3n alcanzable por ",
                     "la distribuci\u00f3n Beta \u2014 al tener dos ",
                     "par\u00e1metros de forma, puede cubrir un \u00e1rea ",
                     "amplia en vez de un solo punto o l\u00ednea."),
              tags$li(strong("Weibull "), "no se dibuja directamente (sus ",
                     "formas quedan muy cerca de gamma/lognormal); solo ",
                     "aparece como nota en la leyenda.")
            ),
            "Mientras m\u00e1s cerca est\u00e9 el punto rojo de un ",
            "s\u00edmbolo, l\u00ednea o \u00e1rea, m\u00e1s plausible es esa ",
            "distribuci\u00f3n como candidata."
        )
      }
    })

    ajuste_realizado <- eventReactive(input$ajustar_dist, {
      x <- valores_dist()
      codigos <- input$candidatas_dist
      req(length(codigos) > 0)
      ajustar_candidatas(x, codigos)
    })

    output$resultado_ajuste_dist <- renderUI({
      req(ajuste_realizado())
      fits <- ajuste_realizado()
      if (length(fits) == 0) {
        return(div(class = "alert alert-warning small",
                   "Ninguna distribuci\u00f3n candidata pudo ajustarse a ",
                   "estos datos."))
      }
      tagList(
        h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
           "Comparaci\u00f3n por criterios de informaci\u00f3n"),
        p(class = "small text-muted mb-2",
          "Menor ", strong("AIC"), "/", strong("BIC"), " = mejor ajuste ",
          "relativo entre las candidatas evaluadas."
        ),
        DTOutput(ns("tabla_criterios_dist")),
        tags$hr(),
        h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
           "Densidad ajustada y gr\u00e1fico Q-Q"),
        layout_columns(
          col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
          plotOutput(ns("plot_denscomp_dist"), height = "360px"),
          plotOutput(ns("plot_qqcomp_dist"), height = "360px")
        )
      )
    })

    output$tabla_criterios_dist <- renderDT({
      req(ajuste_realizado())
      df <- tabla_criterios_ajuste(ajuste_realizado())
      datatable(df, rownames = FALSE,
                options = list(dom = "t", paging = FALSE),
                class = "table-sm table-striped")
    })

    output$plot_denscomp_dist <- renderPlot({
      fits <- ajuste_realizado()
      req(length(fits) > 0)
      tryCatch({
        fitdistrplus::denscomp(fits, legendtext = names(fits),
                              plotstyle = "ggplot") +
          ggplot2::labs(title = "Densidad observada vs. ajustada") +
          theme_light(base_size = 16) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0, y = 0,
                            label = paste("No se pudo graficar:",
                                         conditionMessage(e))) +
          theme_void()
      })
    })

    output$plot_qqcomp_dist <- renderPlot({
      fits <- ajuste_realizado()
      req(length(fits) > 0)
      tryCatch({
        fitdistrplus::qqcomp(fits, legendtext = names(fits),
                             plotstyle = "ggplot") +
          ggplot2::labs(title = "Gr\u00e1fico Q-Q") +
          theme_light(base_size = 16) +
          theme(plot.background = element_rect(fill = colores$fondo, color = NA))
      }, error = function(e) {
        ggplot() + annotate("text", x = 0, y = 0,
                            label = paste("No se pudo graficar:",
                                         conditionMessage(e))) +
          theme_void()
      })
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_dist <- renderText({
      req(input$var_dist, input$candidatas_dist)
      fuente <- input$fuente_datos_dist
      es_propio <- !is.null(datos_propio_dist()) && !is.null(input$archivo_dist)
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
      codigos <- input$candidatas_dist
      es_conteo <- es_conteo_dist()
      linea_ajustes <- paste0(
        "ajustes <- list(\n",
        paste0("  ", codigos, " = fitdistrplus::fitdist(x, \"", codigos, "\")",
              collapse = ",\n"),
        "\n)\n"
      )
      paste0(
        "# \u2500\u2500 Identificar la distribuci\u00f3n \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "# Dataset: ", nm_datos, "\n",
        "# Variable: ", input$var_dist, "\n\n",
        linea_carga, "\n\n",
        "library(fitdistrplus)\n\n",
        "x <- ", nm_datos, "$", input$var_dist, "\n",
        "x <- x[!is.na(x)]\n\n",
        "descdist(x, boot = 500, discrete = ", es_conteo, ")",
        "  # gr\u00e1fico de Cullen y Frey\n\n",
        if (es_conteo) {
          paste0("# \u00cdndice de dispersi\u00f3n (var/media): Poisson asume \u2248 1\n",
                "var(x) / mean(x)\n\n")
        } else "",
        linea_ajustes, "\n",
        "sapply(ajustes, function(f) c(AIC = f$aic, BIC = f$bic))\n",
        "denscomp(ajustes, legendtext = names(ajustes))\n",
        "qqcomp(ajustes, legendtext = names(ajustes))\n"
      )
    })

    output$descargar_script_dist <- downloadHandler(
      filename = function() paste0("StatBasics_distribuciones_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_dist(), file)
    )

  })
}
