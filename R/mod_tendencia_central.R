# ============================================================
# mod_tendencia_central.R — Medidas de tendencia central
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Simulación interactiva (teórica, con sliders)
#   3. Los datos (Datos de ejemplo + Mis datos + Tipos de variables)
#   4. Practica con datos reales
#   5. Código R
#
# Nota pedagógica: incluye un dataset asimétrico (salarios de
# profesores, carData::Salaries) para mostrar visualmente cuándo
# media, mediana y moda difieren y cuál conviene usar.
# ============================================================

# ── Helper: moda ────────────────────────────────────────────
moda_tc <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ── Helper: moda aproximada para datos continuos (pico de densidad)
moda_continua_tc <- function(x) {
  d <- stats::density(x)
  d$x[which.max(d$y)]
}

# ── Helper: coeficiente de asimetría (Fisher-Pearson, g1)
asimetria_tc <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- stats::sd(x)
  (sum((x - m)^3) / n) / s^3
}

# ── UI ────────────────────────────────────────────────────
mod_tendencia_central_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("bar-chart", class = "me-2"),
        "Tendencia central",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Media, mediana y moda responden a la misma pregunta \u2014 ",
        strong("\u00bfd\u00f3nde se concentran los datos?"),
        " \u2014 pero cada una lo hace de forma distinta. Elegir la ",
        "medida correcta depende de la forma de la distribuci\u00f3n."
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
             "Medidas de tendencia central"),
          p(class = "small text-muted mb-3",
            "Resumen el valor \"t\u00edpico\" o central de un conjunto de datos."
          ),

          div(
            style = "overflow-x: auto;",
            tags$table(
              class = "table table-sm table-bordered small mb-4",
              style = "background: #ffffff;",
              tags$thead(
                style = paste0("background:", colores$primario,
                               "; color: #ffffff;"),
                tags$tr(
                  tags$th("Medida"), tags$th("F\u00f3rmula / definici\u00f3n"),
                  tags$th("Cu\u00e1ndo usarla")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong("Media (\u0078\u0304)")),
                  tags$td("\u03a3x / n"),
                  tags$td("Datos sim\u00e9tricos, sin outliers extremos")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Mediana")),
                  tags$td("Valor central al ordenar los datos"),
                  tags$td("Datos asim\u00e9tricos o con outliers")
                ),
                tags$tr(
                  tags$td(strong("Moda")),
                  tags$td("Valor(es) m\u00e1s frecuente(s)"),
                  tags$td("Variables categ\u00f3ricas o discretas")
                )
              )
            )
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "La forma de la distribuci\u00f3n decide cu\u00e1l medida usar"),
          p(class = "small text-muted mb-2",
            "En una distribuci\u00f3n sim\u00e9trica, media y mediana coinciden. ",
            "En una distribuci\u00f3n asim\u00e9trica (sesgada), se separan \u2014 y la ",
            "mediana suele describir mejor el valor \"t\u00edpico\"."
          ),

          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("distribute-horizontal", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Sim\u00e9trica"), br(),
              "Media \u2248 Mediana \u2248 Moda"
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("arrow-bar-right", class = "me-1",
                      style = paste0("color:", colores$acento)),
              strong("Sesgada a la derecha"), br(),
              "Moda < Mediana < Media (ej.: precios, salarios)"
            ),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("arrow-bar-left", class = "me-1",
                      style = paste0("color:", colores$peligro)),
              strong("Sesgada a la izquierda"), br(),
              "Media < Mediana < Moda"
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "El efecto de un valor at\u00edpico"),
          layout_columns(
            col_widths = c(6, 6),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("exclamation-triangle-fill", class = "me-1",
                      style = paste0("color:", colores$peligro)),
              strong("La media es sensible."), br(),
              "Un solo valor extremo puede desplazarla mucho."
            ),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("shield-check", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("La mediana es robusta."), br(),
              "Apenas se mueve aunque el outlier sea extremo."
            )
          ),
          p(class = "small text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en la pesta\u00f1a ",
            strong("Simulaci\u00f3n interactiva"), " (con sliders) o en ",
            strong("Practica con datos reales"), " (con el dataset de ",
            "salarios, que es asim\u00e9trico)."
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
            "Genera una distribuci\u00f3n simulada y observa en vivo c\u00f3mo se ",
            "relacionan media, mediana y moda seg\u00fan el ", strong("sesgo"),
            ", la ", strong("proporci\u00f3n de valores at\u00edpicos"), " y el ",
            strong("tama\u00f1o de muestra"), "."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("sesgo_sim_tc"), "Sesgo (asimetr\u00eda):",
                            min = -5, max = 5, value = 0, step = 0.5),
                sliderInput(ns("outliers_sim_tc"),
                            "Valores at\u00edpicos (%):",
                            min = 0, max = 20, value = 0, step = 1,
                            post = "%"),
                sliderInput(ns("n_sim_tc"), "Tama\u00f1o de muestra (n):",
                            min = 20, max = 1000, value = 200, step = 10),
                actionButton(ns("regenerar_sim_tc"), "Nueva muestra aleatoria",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle")),
                tags$hr(),
                uiOutput(ns("cards_sim_tc"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_tc"), height = "380px"),
              uiOutput(ns("insight_sim_tc"))
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
                col_widths = c(4, 8),
                div(
                  radioButtons(
                    ns("fuente_datos_tc"),
                    label   = tagList(bs_icon("database", class = "me-1"),
                                      "Seleccionar dataset:"),
                    choices = c(
                      "Ping\u00fcinos Palmer \u2014 medidas corporales (ecolog\u00eda)" = "penguins",
                      "Peso al nacer \u2014 salud perinatal (Hosmer & Lemeshow)" = "birthwt",
                      "Salarios de profesores universitarios (ciencias sociales, sesgado)" = "salaries",
                      "Actitud hacia el trabajo \u2014 encuesta organizacional (psicolog\u00eda)" = "attitude"
                    ),
                    selected = "salaries"
                  ),
                  tags$hr(),
                  uiOutput(ns("info_dataset_tc"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_tc")),
                    br(),
                    DTOutput(ns("tabla_preview_tc"))
                  )
                )
              )
            ),

            # ── Sub 2: Mis datos ─────────────────────
            nav_panel(
              title = tagList(bs_icon("folder2-open", class = "me-1"),
                              "Mis datos"),
              br(),
              layout_columns(
                col_widths = c(4, 8),
                div(
                  p(class = "small text-muted mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    "Sube un archivo CSV o Excel. ",
                    "La primera fila debe contener los nombres de las columnas."),
                  fileInput(
                    ns("archivo_tc"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar\u2026",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador_tc"),
                    label   = "Separador (CSV):",
                    choices = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    )
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio_tc"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio_tc")),
                    br(),
                    DTOutput(ns("tabla_preview_propio_tc"))
                  )
                )
              )
            ),

            # ── Sub 3: Tipos de variables ────────────
            nav_panel(
              title = tagList(bs_icon("sliders2", class = "me-1"),
                              "Tipos de variables"),
              br(),
              p(class = "small text-muted mb-3",
                "Verifica que cada variable tenga el tipo correcto antes de ",
                "calcular medidas de tendencia central o dispersi\u00f3n \u2014 ",
                "estas s\u00f3lo tienen sentido para variables ", strong("num\u00e9ricas"), "."
              ),
              layout_columns(
                col_widths = c(10, 2),
                uiOutput(ns("tabla_tipos_tc")),
                div(
                  class = "pt-2",
                  actionButton(ns("aplicar_tipos_tc"), "Aplicar tipos",
                               class = "btn-primary w-100",
                               icon  = icon("check")),
                  br(), br(),
                  actionButton(ns("resetear_tipos_tc"), "Restaurar",
                               class = "btn-outline-secondary w-100 btn-sm",
                               icon  = icon("rotate-left"))
                )
              ),
              uiOutput(ns("tipos_aplicados_msg_tc"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Practica con datos reales
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("calculator", class = "me-1"),
                        "Practica con datos reales"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_tc")),
                tags$hr(),
                checkboxInput(ns("agregar_outlier_tc"),
                              "Agregar un valor at\u00edpico",
                              value = FALSE),
                conditionalPanel(
                  condition = "input.agregar_outlier_tc == true",
                  ns = ns,
                  numericInput(ns("valor_outlier_tc"), "Valor del outlier:",
                               value = 0)
                ),
                tags$hr(),
                uiOutput(ns("cards_estadisticos_tc"))
              )
            ),
            div(
              plotOutput(ns("plot_histograma_tc"), height = "380px"),
              uiOutput(ns("insight_tc"))
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
            "C\u00f3digo reproducible para el dataset y variable seleccionados ",
            "en la pesta\u00f1a anterior."),
          div(class = "codigo-bloque", verbatimTextOutput(ns("codigo_r_tc"))),
          downloadButton(ns("descargar_script_tc"), "Descargar script .R",
                        class = "btn-primary mt-3")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_tendencia_central_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Simulación interactiva (teórica) ──────────────
    datos_sim_tc <- reactive({
      input$regenerar_sim_tc  # dependencia: botón regenera nueva muestra
      n      <- input$n_sim_tc
      sesgo  <- input$sesgo_sim_tc
      pct_at <- input$outliers_sim_tc
      req(n, !is.null(sesgo), !is.null(pct_at))

      forma <- max(0.3, 5 - abs(sesgo))
      peso  <- abs(sesgo) / 5   # 0 = normal pura, 1 = gamma completa
      z     <- stats::rnorm(n)
      if (peso > 0) {
        g       <- stats::rgamma(n, shape = forma, rate = 1)
        g_std   <- as.numeric(scale(g))
        base_std <- (1 - peso) * z + peso * g_std
      } else {
        base_std <- z
      }
      if (sesgo < 0) base_std <- -base_std
      x <- base_std * 10 + 50   # centrado en 50, escala legible

      n_out <- round(n * pct_at / 100)
      if (n_out > 0) {
        idx <- sample(seq_len(n), n_out)
        signo <- if (sesgo >= 0) 1 else -1
        x[idx] <- 50 + signo * stats::runif(n_out, 40, 70)
      }
      x
    })

    estadisticos_sim_tc <- reactive({
      x <- datos_sim_tc()
      req(length(x) > 1)
      list(
        media    = mean(x),
        mediana  = median(x),
        moda     = moda_continua_tc(x),
        asimetria = asimetria_tc(x)
      )
    })

    output$cards_sim_tc <- renderUI({
      e <- estadisticos_sim_tc()
      tagList(
        tarjeta_metrica("Media", round(e$media, 2), "media"),
        tarjeta_metrica("Mediana", round(e$mediana, 2), "mediana"),
        tarjeta_metrica("Moda (aprox.)", round(e$moda, 2), "moda"),
        tarjeta_metrica("Asimetr\u00eda (g1)", round(e$asimetria, 2),
                        "asimetria", tipo = "secondary", ultima = TRUE)
      )
    })

    output$plot_sim_tc <- renderPlot({
      x <- datos_sim_tc()
      req(length(x) > 1)
      e  <- estadisticos_sim_tc()
      df <- data.frame(x = x)
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = colores$secundario, color = "white", alpha = 0.85) +
        geom_density(color = colores$texto, linewidth = 0.6) +
        geom_vline(xintercept = e$media, color = colores$primario,
                  linewidth = 1) +
        geom_vline(xintercept = e$mediana, color = colores$acento,
                  linewidth = 1, linetype = "dashed") +
        geom_vline(xintercept = e$moda, color = colores$peligro,
                  linewidth = 1, linetype = "dotted") +
        annotate("text", x = e$media, y = Inf, label = "Media", vjust = 2,
                color = colores$primario, fontface = "bold", size = 3.5) +
        annotate("text", x = e$mediana, y = Inf, label = "Mediana", vjust = 4,
                color = colores$acento, fontface = "bold", size = 3.5) +
        annotate("text", x = e$moda, y = Inf, label = "Moda", vjust = 6,
                color = colores$peligro, fontface = "bold", size = 3.5) +
        labs(x = "Valor simulado", y = "Densidad") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$insight_sim_tc <- renderUI({
      e   <- estadisticos_sim_tc()
      rel <- if (abs(e$media - e$mediana) < 0.5) {
        "Media \u2248 Mediana: la distribuci\u00f3n es casi sim\u00e9trica."
      } else if (e$media > e$mediana) {
        paste0("Media > Mediana: la distribuci\u00f3n est\u00e1 sesgada a la derecha \u2014 ",
               "algunos valores altos est\u00e1n \"jalando\" la media hacia arriba.")
      } else {
        "Media < Mediana: la distribuci\u00f3n est\u00e1 sesgada a la izquierda."
      }
      lect_asim <- if (abs(e$asimetria) < 0.5) {
        "cercano a 0 \u2192 forma aproximadamente sim\u00e9trica."
      } else if (e$asimetria >= 0.5) {
        "positivo \u2192 cola larga hacia la derecha."
      } else {
        "negativo \u2192 cola larga hacia la izquierda."
      }
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          rel,
          if (input$outliers_sim_tc > 0)
            paste0(" Con ", input$outliers_sim_tc, "% de valores at\u00edpicos, ",
                   "nota c\u00f3mo la media se mueve m\u00e1s que la mediana."),
          tags$br(),
          strong("Coeficiente de asimetr\u00eda: "), round(e$asimetria, 2),
          " \u2014 ", lect_asim
      )
    })

    # ── Datos reactivos (ejemplo) ─────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_tc
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
    output$info_dataset_tc <- renderUI({
      fuente <- input$fuente_datos_tc
      if (is.null(fuente)) return(NULL)
      switch(fuente,
        penguins = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Ping\u00fcinos Palmer."), " Medidas corporales de ",
          strong("344 ping\u00fcinos"), " de 3 especies en la Antártida ",
          "(Gorman, Williams & Fraser, 2014). Incluye ",
          strong("bill_length_mm"), ", ", strong("bill_depth_mm"), ", ",
          strong("flipper_length_mm"), " y ", strong("body_mass_g"), "."
        ),
        birthwt = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Peso al nacer (Hosmer & Lemeshow)."), " Datos de ",
          strong("189 neonatos"), " del Baystate Medical Center, ",
          "Springfield, MA (1986). Incluye ", strong("bwt"),
          " (peso al nacer en gramos), ", strong("age"), " (edad de la madre) ",
          "y ", strong("lwt"), " (peso de la madre en libras)."
        ),
        salaries = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Salaries (carData)."), " Salarios de ",
          strong("397 profesores universitarios"), " en EE. UU. (2008-09). ",
          "Incluye ", strong("salary"), " (sesgado a la derecha \u2014 ",
          "ideal para ver la diferencia media/mediana), ",
          strong("yrs.service"), " y ", strong("yrs.since.phd"), "."
        ),
        attitude = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: attitude."), " Encuesta de clima organizacional a ",
          strong("30 departamentos"), " de una empresa financiera. Variables ",
          "en escala 0-100: ", strong("rating"), " (satisfacci\u00f3n general), ",
          strong("complaints"), ", ", strong("learning"), ", entre otras."
        )
      )
    })

    # ── Vista previa datos de ejemplo ────────────────
    output$cards_datos_tc <- renderUI({
      req(datos())
      d    <- datos()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
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

    output$tabla_preview_tc <- renderDT({
      req(datos())
      datatable(datos(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Datos propios ─────────────────────────────────
    datos_propio_tc <- reactive({
      req(input$archivo_tc)
      ext <- tools::file_ext(input$archivo_tc$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_tc$datapath)
        else
          readr::read_delim(input$archivo_tc$datapath,
                            delim = input$separador_tc %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error al leer archivo:", e$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio_tc(), { datos_mod(datos_propio_tc()) })

    output$resumen_datos_propio_tc <- renderUI({
      req(datos_propio_tc())
      d <- datos_propio_tc()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_tc <- renderUI({
      req(datos_propio_tc())
      d    <- datos_propio_tc()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
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

    output$tabla_preview_propio_tc <- renderDT({
      req(datos_propio_tc())
      datatable(datos_propio_tc(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_tc <- reactiveVal(NULL)

    output$tabla_tipos_tc <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_tc()
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

    observeEvent(input$aplicar_tipos_tc, {
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
      tipos_usuario_tc(tu)
      datos_mod(d)
      output$tipos_aplicados_msg_tc <- renderUI({
        div(class = "alert alert-success small mt-3",
            bs_icon("check-circle-fill", class = "me-1"),
            "Tipos de variable aplicados.")
      })
    })

    observeEvent(input$resetear_tipos_tc, {
      tipos_usuario_tc(NULL)
      datos_mod(datos())
      output$tipos_aplicados_msg_tc <- renderUI({
        div(class = "alert alert-info small mt-3",
            bs_icon("arrow-counterclockwise", class = "me-1"),
            "Tipos restaurados a los originales.")
      })
    })

    # ── Explorar y calcular ───────────────────────────
    vars_numericas_tc <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    output$sel_var_tc <- renderUI({
      req(vars_numericas_tc())
      selectInput(ns("var_tc"), "Variable num\u00e9rica:",
                  choices = vars_numericas_tc())
    })

    # Vector con outlier opcional agregado
    valores_tc <- reactive({
      req(datos_mod(), input$var_tc)
      x <- datos_mod()[[input$var_tc]]
      x <- x[!is.na(x)]
      if (isTRUE(input$agregar_outlier_tc) && !is.null(input$valor_outlier_tc)) {
        x <- c(x, input$valor_outlier_tc)
      }
      x
    })

    estadisticos_tc <- reactive({
      x <- valores_tc()
      req(length(x) > 0)
      list(
        n       = length(x),
        media   = mean(x),
        mediana = median(x),
        moda    = moda_tc(x)
      )
    })

    output$cards_estadisticos_tc <- renderUI({
      e <- estadisticos_tc()
      tagList(
        tarjeta_metrica("Media", round(e$media, 2), "media"),
        tarjeta_metrica("Mediana", round(e$mediana, 2), "mediana"),
        tarjeta_metrica("Moda", round(e$moda, 2), "moda", ultima = TRUE)
      )
    })

    output$plot_histograma_tc <- renderPlot({
      x <- valores_tc()
      req(length(x) > 1)
      e <- estadisticos_tc()
      df <- data.frame(x = x)
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 20,
                       fill = colores$secundario, color = "white", alpha = 0.85) +
        geom_density(color = colores$texto, linewidth = 0.6) +
        geom_vline(xintercept = e$media, color = colores$primario,
                  linewidth = 1, linetype = "solid") +
        geom_vline(xintercept = e$mediana, color = colores$acento,
                  linewidth = 1, linetype = "dashed") +
        geom_vline(xintercept = e$moda, color = colores$peligro,
                  linewidth = 1, linetype = "dotted") +
        annotate("text", x = e$media, y = Inf, label = "Media", vjust = 2,
                color = colores$primario, fontface = "bold", size = 3.5) +
        annotate("text", x = e$mediana, y = Inf, label = "Mediana", vjust = 4,
                color = colores$acento, fontface = "bold", size = 3.5) +
        annotate("text", x = e$moda, y = Inf, label = "Moda", vjust = 6,
                color = colores$peligro, fontface = "bold", size = 3.5) +
        labs(x = input$var_tc, y = "Densidad") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA))
    })

    output$insight_tc <- renderUI({
      e <- estadisticos_tc()
      dif <- abs(e$media - e$mediana)
      if (isTRUE(input$agregar_outlier_tc)) {
        div(class = "alert alert-warning small mt-2",
            bs_icon("lightbulb-fill", class = "me-1"),
            strong("Con el outlier agregado: "),
            "la diferencia entre media y mediana es ", round(dif, 2),
            ". Nota c\u00f3mo la media se desplaza hacia el valor at\u00edpico ",
            "mientras la mediana casi no se mueve."
        )
      } else {
        div(class = "alert alert-info small mt-2",
            bs_icon("lightbulb-fill", class = "me-1"),
            "Activa \"Agregar un valor at\u00edpico\" en los controles para ",
            "ver c\u00f3mo un solo dato extremo afecta a la media pero no a ",
            "la mediana."
        )
      }
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_tc <- renderText({
      req(input$var_tc)
      fuente <- input$fuente_datos_tc
      es_propio <- !is.null(datos_propio_tc()) && !is.null(input$archivo_tc)
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
        "# \u2500\u2500 Medidas de tendencia central \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "# Dataset: ", nm_datos, "\n",
        "# Variable: ", input$var_tc, "\n\n",
        linea_carga, "\n\n",
        "x <- ", nm_datos, "$", input$var_tc, "\n\n",
        "mean(x)              # media\n",
        "median(x)            # mediana\n",
        "table(x)[which.max(table(x))]  # moda (aproximada)\n"
      )
    })

    output$descargar_script_tc <- downloadHandler(
      filename = function() paste0("StatBasics_tendencia_central_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_tc(), file)
    )

  })
}
