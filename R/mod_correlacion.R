# ============================================================
# mod_correlacion.R — Correlación
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Simulación interactiva (control\u00e1s la fuerza de r)
#   3. Los datos
#   4. Practica con datos reales
#   5. Código R
#
# Nota pedagógica: el punto m\u00e1s importante de este módulo es
# "correlación no es causalidad" — se ilustra con el ejemplo
# cl\u00e1sico (helados y ahogamientos, ambos causados por el calor
# del verano, una variable de confusi\u00f3n). Tambi\u00e9n reutiliza el
# IC de Fisher para r, cerrando el ciclo con los dos módulos
# anteriores.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_correlacion_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("graph-up", class = "me-2"),
        "Correlaci\u00f3n",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Hasta ahora describiste ", strong("una variable"), " a la vez. La ",
        "correlaci\u00f3n es el primer paso hacia describir ", strong(
        "relaciones entre dos variables"), "."
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
            1, "graph-up", colores$primario,
            "\u00bfQu\u00e9 mide la correlaci\u00f3n?",
            p(class = "mb-2",
              "El coeficiente de correlaci\u00f3n (r) resume, en un solo ",
              "n\u00famero entre \u22121 y +1, qu\u00e9 tan fuerte y en qu\u00e9 direcci\u00f3n ",
              "se relacionan dos variables:"
            ),
            fluidRow(
              column(4,
                div(style = paste0("text-align:center; background:",
                                  colores$fondo, "; border-radius:8px;",
                                  "padding:14px;"),
                    div(style = paste0("font-size:1.6rem; font-weight:700;",
                                      "color:", colores$peligro, ";"), "\u22121"),
                    div(class = "small text-muted", "Relaci\u00f3n negativa perfecta")
                )
              ),
              column(4,
                div(style = paste0("text-align:center; background:",
                                  colores$fondo, "; border-radius:8px;",
                                  "padding:14px;"),
                    div(style = paste0("font-size:1.6rem; font-weight:700;",
                                      "color:", colores$secundario, ";"), "0"),
                    div(class = "small text-muted", "Sin relaci\u00f3n lineal")
                )
              ),
              column(4,
                div(style = paste0("text-align:center; background:",
                                  colores$fondo, "; border-radius:8px;",
                                  "padding:14px;"),
                    div(style = paste0("font-size:1.6rem; font-weight:700;",
                                      "color:", colores$primario, ";"), "+1"),
                    div(class = "small text-muted", "Relaci\u00f3n positiva perfecta")
                )
              )
            )
          ),

          conector_infografia("¿Cuál método usar? Depende del tipo de relación"),

          fluidRow(
            column(4,
              tarjeta_concepto(
                "graph-up", colores$primario, "Pearson",
                p(class = "mb-0",
                  "Mide relaciones ", strong("lineales"), ". Es el m\u00e1s ",
                  "usado, pero asume que ambas variables son num\u00e9ricas y ",
                  "que la relaci\u00f3n (si existe) es una l\u00ednea recta."
                )
              )
            ),
            column(4,
              tarjeta_concepto(
                "arrow-up-right", colores$acento, "Spearman",
                p(class = "mb-0",
                  "Mide relaciones ", strong("mon\u00f3tonas"), " (no ",
                  "necesariamente lineales) usando los rangos de los ",
                  "datos, no los valores crudos. M\u00e1s robusto a valores ",
                  "at\u00edpicos."
                )
              )
            ),
            column(4,
              tarjeta_concepto(
                "sort-numeric-down", colores$secundario, "Kendall",
                p(class = "mb-0",
                  "Tambi\u00e9n basado en rangos, pero m\u00e1s conservador. \u00datil ",
                  "con muestras peque\u00f1as o muchos valores empatados."
                )
              )
            )
          ),

          conector_infografia("Un límite importante: r solo detecta ciertos tipos de relación"),

          tarjeta_concepto(
            "exclamation-triangle", colores$acento,
            "r puede ser 0 aunque SÍ exista una relación clara",
            p(class = "mb-2",
              "Pearson solo detecta relaciones ", strong("lineales"),
              " (l\u00ednea recta); Spearman/Kendall solo relaciones ",
              strong("mon\u00f3tonas"), " (siempre sube o siempre baja). Si la ",
              "relaci\u00f3n tiene otra forma \u2014 una U, una onda, una X, un ",
              "c\u00edrculo \u2014 estos coeficientes pueden dar ", strong("r \u2248 0"),
              ", ", em("a pesar de que hay un patr\u00f3n perfectamente claro"),
              ":"
            ),
            fluidRow(
              column(3, plotOutput(ns("plot_nolineal_onda"), height = "160px")),
              column(3, plotOutput(ns("plot_nolineal_parabola"), height = "160px")),
              column(3, plotOutput(ns("plot_nolineal_x"), height = "160px")),
              column(3, plotOutput(ns("plot_nolineal_circulo"), height = "160px"))
            ),
            p(class = "mt-2 mb-0 text-muted small",
              "Por eso ", strong("siempre conviene graficar tus datos"),
              " (nunca confiar solo en el n\u00famero de r) \u2014 un scatter ",
              "revela patrones que el coeficiente por s\u00ed solo puede ",
              "esconder por completo."
            )
          ),

          conector_infografia("La lección más importante de todo el módulo"),

          tarjeta_concepto(
            "exclamation-octagon", colores$peligro,
            "Correlaci\u00f3n NO es causalidad",
            p(class = "mb-2",
              "El ejemplo cl\u00e1sico: en verano, ", strong("las ventas de ",
              "helado"), " y ", strong("los ahogamientos"), " est\u00e1n ",
              "fuertemente correlacionados. \u00bfComer helado causa ",
              "ahogamientos? No \u2014 ambos son causados por una tercera ",
              "variable: ", strong("el calor del verano"), " (m\u00e1s calor \u2192 ",
              "m\u00e1s gente compra helado ", em("y"), " m\u00e1s gente va a ",
              "nadar)."
            ),
            p(class = "mb-0",
              "Esta \"tercera variable\" se llama ", strong(
              "variable de confusi\u00f3n"), " (confounder). Encontrar una ",
              "correlaci\u00f3n fuerte nunca es, por s\u00ed sola, evidencia de que ",
              "una variable causa la otra \u2014 podr\u00eda ser causalidad, ",
              "causalidad inversa, una variable de confusi\u00f3n, o ",
              "simple coincidencia."
            )
          ),

          p(class = "text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en ", strong("Simulaci\u00f3n interactiva"),
            " \u2014 controla la fuerza de la relaci\u00f3n y observa c\u00f3mo ",
            "cambia el gr\u00e1fico y el valor de r."
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
            "Mueve el control de fuerza de relaci\u00f3n y observa c\u00f3mo ",
            "cambian el gr\u00e1fico de dispersi\u00f3n y el valor de r."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("r_objetivo_sim_cor"),
                            "Fuerza de la relaci\u00f3n (r objetivo):",
                            min = -1, max = 1, value = 0.7, step = 0.05),
                sliderInput(ns("n_sim_cor"), "Tama\u00f1o de muestra (n):",
                            min = 10, max = 500, value = 100, step = 10),
                actionButton(ns("regenerar_sim_cor"), "Nueva muestra aleatoria",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_cor"), height = "400px"),
              uiOutput(ns("insight_sim_cor")),
              tags$hr(),
              uiOutput(ns("cards_sim_cor"))
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
                col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
                div(
                  radioButtons(
                    ns("fuente_datos_cor"),
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
                  uiOutput(ns("info_dataset_cor"))
                ),
                div(
                  uiOutput(ns("cards_datos_cor")),
                  DTOutput(ns("tabla_preview_cor"))
                )
              )
            ),

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
                    ns("archivo_cor"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar\u2026",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador_cor"),
                    label   = "Separador (CSV):",
                    choices = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    )
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio_cor"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio_cor")),
                    br(),
                    DTOutput(ns("tabla_preview_propio_cor"))
                  )
                )
              )
            ),

            nav_panel(
              title = tagList(bs_icon("list-columns-reverse", class = "me-1"),
                              "Tipos de variables"),
              br(),
              uiOutput(ns("tabla_tipos_cor")),
              div(class = "mt-3",
                actionButton(ns("aplicar_tipos_cor"), "Aplicar tipos",
                             class = "btn-primary btn-sm me-2"),
                actionButton(ns("resetear_tipos_cor"), "Restaurar originales",
                             class = "btn-outline-secondary btn-sm")
              ),
              uiOutput(ns("tipos_aplicados_msg_cor"))
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
            "Elige dos variables num\u00e9ricas y un m\u00e9todo de correlaci\u00f3n."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                uiOutput(ns("sel_var_x_cor")),
                uiOutput(ns("sel_var_y_cor")),
                radioButtons(ns("metodo_cor"), "M\u00e9todo:",
                             choices = c("Pearson" = "pearson",
                                        "Spearman" = "spearman",
                                        "Kendall" = "kendall"),
                             selected = "pearson"),
                actionButton(ns("calcular_cor"), "Calcular correlaci\u00f3n",
                             class = "btn-primary w-100 btn-sm",
                             icon  = icon("play"))
              )
            ),
            div(
              plotOutput(ns("plot_practica_cor"), height = "380px"),
              uiOutput(ns("insight_practica_cor")),
              tags$hr(),
              uiOutput(ns("cards_practica_cor"))
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
          verbatimTextOutput(ns("codigo_r_cor")),
          downloadButton(ns("descargar_script_cor"), "Descargar script .R",
                         class = "btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_correlacion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── ¿Qué es? — relaciones no lineales con r ≈ 0 ───
    # Cada panel es independiente y simple (mismo patrón seguro
    # usado en Error Est\u00e1ndar) para evitar problemas de tama\u00f1o
    # de dispositivo gr\u00e1fico con paneles combinados.
    tema_mini_cor <- function() {
      theme_light(base_size = 14) +
        theme(axis.text = element_blank(), axis.title = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(face = "bold", size = 14,
                                        color = colores$primario, hjust = 0.5),
              plot.background = element_rect(fill = colores$fondo, color = NA),
              panel.grid = element_blank())
    }

    output$plot_nolineal_onda <- renderPlot({
      set.seed(42)
      x <- stats::runif(400, -4, 4)
      y <- sin(x) + stats::rnorm(400, sd = 0.15)
      r <- round(stats::cor(x, y), 2)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_point(color = colores$primario, alpha = 0.5, size = 0.8) +
        labs(title = paste0("Onda (r=", r, ")")) +
        tema_mini_cor()
    })

    output$plot_nolineal_parabola <- renderPlot({
      set.seed(43)
      x <- stats::runif(400, -3, 3)
      y <- x^2 + stats::rnorm(400, sd = 1)
      r <- round(stats::cor(x, y), 2)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_point(color = colores$acento, alpha = 0.5, size = 0.8) +
        labs(title = paste0("Par\u00e1bola (r=", r, ")")) +
        tema_mini_cor()
    })

    output$plot_nolineal_x <- renderPlot({
      set.seed(44)
      x <- stats::rnorm(400)
      signo <- ifelse(stats::runif(400) < 0.5, 1, -1)
      y <- signo * x + stats::rnorm(400, sd = 0.2)
      r <- round(stats::cor(x, y), 2)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_point(color = colores$secundario, alpha = 0.5, size = 0.8) +
        labs(title = paste0("Forma de X (r=", r, ")")) +
        tema_mini_cor()
    })

    output$plot_nolineal_circulo <- renderPlot({
      set.seed(45)
      theta <- stats::runif(400, 0, 2 * pi)
      x <- cos(theta) + stats::rnorm(400, sd = 0.05)
      y <- sin(theta) + stats::rnorm(400, sd = 0.05)
      r <- round(stats::cor(x, y), 2)
      ggplot(data.frame(x, y), aes(x, y)) +
        geom_point(color = colores$peligro, alpha = 0.5, size = 0.8) +
        labs(title = paste0("C\u00edrculo (r=", r, ")")) +
        tema_mini_cor()
    })

    # ── Simulación interactiva ─────────────────────────
    datos_sim_cor <- reactive({
      input$regenerar_sim_cor
      r_obj <- input$r_objetivo_sim_cor
      n     <- input$n_sim_cor
      req(r_obj, n)

      x <- stats::rnorm(n)
      y <- r_obj * x + sqrt(1 - r_obj^2) * stats::rnorm(n)
      data.frame(x = x, y = y)
    })

    output$cards_sim_cor <- renderUI({
      df <- datos_sim_cor()
      r_obs <- stats::cor(df$x, df$y)
      tagList(
        tarjeta_metrica("r objetivo", round(input$r_objetivo_sim_cor, 2),
                        "r_correlacion"),
        tarjeta_metrica("r observado (Pearson)", round(r_obs, 2),
                        "r_correlacion", ultima = TRUE)
      )
    })

    output$plot_sim_cor <- renderPlot({
      df <- datos_sim_cor()
      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = FALSE, color = colores$acento,
                   linewidth = 1) +
        labs(x = "Variable X", y = "Variable Y",
             title = paste0("r observado = ",
                           round(stats::cor(df$x, df$y), 2))) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 17))
    })

    output$insight_sim_cor <- renderUI({
      df    <- datos_sim_cor()
      r_obs <- stats::cor(df$x, df$y)
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("Fuerza ", interpretar_fuerza_r(r_obs), " y direcci\u00f3n ",
                if (r_obs > 0) "positiva" else "negativa", ". Nota que el ",
                "r observado no es id\u00e9ntico al objetivo \u2014 r tambi\u00e9n es ",
                "un estad\u00edstico calculado de una muestra, as\u00ed que tiene su ",
                "propio error est\u00e1ndar y var\u00eda de muestra en muestra (el ",
                "mismo concepto que ya viste con la media, aplicado ahora a ",
                "r). Sube n para que el r observado se acerque m\u00e1s al ",
                "objetivo.")
      )
    })

    # ── Datos reactivos (ejemplo) ─────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_cor
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

    output$info_dataset_cor <- renderUI({
      fuente <- input$fuente_datos_cor
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

    output$cards_datos_cor <- renderUI({
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

    output$tabla_preview_cor <- renderDT({
      req(datos_mod())
      datatable(datos_mod(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    datos_propio_cor <- reactive({
      req(input$archivo_cor)
      ext <- tools::file_ext(input$archivo_cor$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_cor$datapath)
        else
          readr::read_delim(input$archivo_cor$datapath,
                            delim = input$separador_cor %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error al leer archivo:", e$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio_cor(), {
      req(datos_propio_cor())
      datos_mod(as.data.frame(datos_propio_cor()))
    })

    output$resumen_datos_propio_cor <- renderUI({
      req(datos_propio_cor())
      d <- datos_propio_cor()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_cor <- renderUI({
      req(datos_propio_cor())
      d    <- datos_propio_cor()
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

    output$tabla_preview_propio_cor <- renderDT({
      req(datos_propio_cor())
      datatable(datos_propio_cor(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    tipos_usuario_cor <- reactiveVal(NULL)

    output$tabla_tipos_cor <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_cor()
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

    observeEvent(input$aplicar_tipos_cor, {
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
      tipos_usuario_cor(tu)
      datos_mod(d)
      output$tipos_aplicados_msg_cor <- renderUI({
        div(class = "alert alert-success small mt-3",
            bs_icon("check-circle-fill", class = "me-1"),
            "Tipos de variable aplicados.")
      })
    })

    observeEvent(input$resetear_tipos_cor, {
      tipos_usuario_cor(NULL)
      datos_mod(datos())
      output$tipos_aplicados_msg_cor <- renderUI({
        div(class = "alert alert-info small mt-3",
            bs_icon("arrow-counterclockwise", class = "me-1"),
            "Tipos restaurados a los originales.")
      })
    })

    # ── Practica con datos reales ──────────────────────
    vars_numericas_cor <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    output$sel_var_x_cor <- renderUI({
      req(vars_numericas_cor())
      selectInput(ns("var_x_cor"), "Variable X:",
                  choices = vars_numericas_cor())
    })

    output$sel_var_y_cor <- renderUI({
      req(vars_numericas_cor())
      vars <- vars_numericas_cor()
      sel  <- if (length(vars) > 1) vars[2] else vars[1]
      selectInput(ns("var_y_cor"), "Variable Y:",
                  choices = vars, selected = sel)
    })

    datos_xy_cor <- reactive({
      req(datos_mod(), input$var_x_cor, input$var_y_cor)
      d <- datos_mod()[, c(input$var_x_cor, input$var_y_cor)]
      d <- d[stats::complete.cases(d), ]
      names(d) <- c("x", "y")
      d
    })

    resultado_practica_cor <- eventReactive(input$calcular_cor, {
      d      <- datos_xy_cor()
      metodo <- input$metodo_cor
      test   <- stats::cor.test(d$x, d$y, method = metodo)
      r      <- unname(test$estimate)
      lista  <- list(r = r, p_value = test$p.value, metodo = metodo, n = nrow(d))
      if (metodo == "pearson") {
        ic <- ic_correlacion_pearson(r, nrow(d))
        lista$li <- ic$li
        lista$ls <- ic$ls
      }
      lista
    })

    output$cards_practica_cor <- renderUI({
      req(resultado_practica_cor())
      res <- resultado_practica_cor()
      tagList(
        tarjeta_metrica(paste0("r (", res$metodo, ")"), round(res$r, 3),
                        "r_correlacion"),
        tarjeta_metrica("Valor p", format.pval(res$p_value, digits = 3),
                        "valor_p_cor",
                        ultima = is.null(res$li)),
        if (!is.null(res$li)) {
          tarjeta_metrica("IC 95% para r",
                          paste0("[", round(res$li, 2), ", ",
                                round(res$ls, 2), "]"),
                          "ic_r", ultima = TRUE)
        }
      )
    })

    output$plot_practica_cor <- renderPlot({
      req(datos_xy_cor(), resultado_practica_cor())
      d   <- datos_xy_cor()
      res <- resultado_practica_cor()
      p <- ggplot(d, aes(x = x, y = y)) +
        geom_point(color = colores$primario, alpha = 0.6, size = 2)
      if (res$metodo == "pearson") {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = colores$acento,
                             fill = colores$acento, alpha = 0.15)
      }
      p +
        labs(x = input$var_x_cor, y = input$var_y_cor,
             title = paste0("r = ", round(res$r, 3), " (", res$metodo, ")")) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 17))
    })

    output$insight_practica_cor <- renderUI({
      req(resultado_practica_cor())
      res <- resultado_practica_cor()
      sig <- if (res$p_value < 0.05) "estad\u00edsticamente significativa" else
             "no estad\u00edsticamente significativa"
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("Correlaci\u00f3n ", interpretar_fuerza_r(res$r), " y ",
                if (res$r > 0) "positiva" else "negativa", " (r = ",
                round(res$r, 3), "), ", sig, " (p = ",
                format.pval(res$p_value, digits = 3), "). Recuerda: esto no ",
                "implica que una variable "),
          em("cause"),
          " la otra."
      )
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_cor <- renderText({
      req(input$var_x_cor, input$var_y_cor, input$metodo_cor)
      fuente <- input$fuente_datos_cor
      es_propio <- !is.null(datos_propio_cor()) && !is.null(input$archivo_cor)
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
        "# \u2500\u2500 Correlaci\u00f3n \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "# Dataset: ", nm_datos, "\n\n",
        linea_carga, "\n\n",
        "x <- ", nm_datos, "$", input$var_x_cor, "\n",
        "y <- ", nm_datos, "$", input$var_y_cor, "\n\n",
        "cor.test(x, y, method = \"", input$metodo_cor, "\")\n"
      )
    })

    output$descargar_script_cor <- downloadHandler(
      filename = function() paste0("StatBasics_correlacion_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_cor(), file)
    )

  })
}
