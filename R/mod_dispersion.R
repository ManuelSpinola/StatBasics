# ============================================================
# mod_dispersion.R — Medidas de dispersión
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Los datos (Datos de ejemplo + Mis datos + Tipos de variables)
#   3. Explorar y calcular
#   4. Código R
#
# Nota pedagógica: incluye un checkbox para agregar un valor
# atípico y comparar en vivo cómo responden SD/varianza (sensibles)
# frente a IQR (robusto) — el mismo principio que en tendencia
# central pero aplicado a la dispersión.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_dispersion_ui <- function(id) {
  ns <- NS(id)

  tagList(

    navset_card_tab(

      # ════════════════════════════════════════════════
      # PESTAÑA 1: ¿Qué es?
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "Medidas de dispersi\u00f3n"),
          p(class = "small text-muted mb-3",
            "La tendencia central sola puede enga\u00f1ar: dos grupos pueden ",
            "tener la misma media y ser radicalmente distintos en qu\u00e9 tan ",
            strong("dispersos"), " o \"esparcidos\" est\u00e1n sus datos. Las ",
            "medidas de dispersi\u00f3n cuantifican qu\u00e9 tan alejados est\u00e1n ",
            "los datos entre s\u00ed y de su centro."
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
                  tags$th("Interpretaci\u00f3n")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(strong("Rango")),
                  tags$td("m\u00e1x \u2212 m\u00edn"),
                  tags$td("Muy sensible a outliers")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("Varianza (s\u00b2)")),
                  tags$td("\u03a3(x \u2212 \u0078\u0304)\u00b2 / (n\u22121)"),
                  tags$td("En unidades al cuadrado \u2014 dif\u00edcil de interpretar sola")
                ),
                tags$tr(
                  tags$td(strong("Desv. est\u00e1ndar (s)")),
                  tags$td("\u221as\u00b2"),
                  tags$td("Misma unidad que los datos \u2014 la m\u00e1s usada")
                ),
                tags$tr(
                  style = paste0("background:", colores$fondo),
                  tags$td(strong("IQR")),
                  tags$td("Q3 \u2212 Q1"),
                  tags$td("Robusto a outliers, va con la mediana")
                ),
                tags$tr(
                  tags$td(strong("Coef. de variaci\u00f3n (CV)")),
                  tags$td("s / \u0078\u0304 \u00d7 100%"),
                  tags$td("Compara dispersi\u00f3n entre variables con distinta escala")
                )
              )
            )
          ),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfRobusta o sensible a valores at\u00edpicos?"),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
            div(
              class = "alert alert-warning small py-2 px-3 mb-0",
              bs_icon("exclamation-triangle-fill", class = "me-1",
                      style = paste0("color:", colores$peligro)),
              strong("Sensibles a outliers"), br(),
              "Rango, varianza y desviaci\u00f3n est\u00e1ndar: un solo valor ",
              "extremo las infla dram\u00e1ticamente."
            ),
            div(
              class = "alert alert-info small py-2 px-3 mb-0",
              bs_icon("shield-check", class = "me-1",
                      style = paste0("color:", colores$primario)),
              strong("Robusto a outliers"), br(),
              "El IQR ignora el 25% m\u00e1s bajo y el 25% m\u00e1s alto \u2014 casi no ",
              "se mueve aunque haya un valor extremo."
            )
          ),

          tags$hr(),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "\u00bfCu\u00e1ndo usar el coeficiente de variaci\u00f3n?"),
          p(class = "small text-muted mb-0",
            "Cuando quieres comparar la dispersi\u00f3n de ", strong("dos variables"),
            " que est\u00e1n en escalas distintas (por ejemplo, peso en kg vs. ",
            "altura en cm). Como el CV es un porcentaje sin unidades, hace ",
            "la comparaci\u00f3n justa."
          ),

          p(class = "small text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en la pesta\u00f1a ",
            strong("Simulaci\u00f3n interactiva"), " (con sliders) o en ",
            strong("Practica con datos reales"), "."
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
            "Genera una muestra simulada y observa en vivo c\u00f3mo cambian ",
            "rango, varianza, sd, IQR y CV seg\u00fan la ", strong("dispersi\u00f3n"),
            ", la ", strong("proporci\u00f3n de valores at\u00edpicos"), " y el ",
            strong("tama\u00f1o de muestra"), "."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("sd_sim_disp"),
                            "Desviaci\u00f3n est\u00e1ndar poblacional:",
                            min = 1, max = 20, value = 5, step = 1),
                sliderInput(ns("outliers_sim_disp"),
                            "Valores at\u00edpicos (%):",
                            min = 0, max = 20, value = 0, step = 1,
                            post = "%"),
                sliderInput(ns("n_sim_disp"), "Tama\u00f1o de muestra (n):",
                            min = 20, max = 1000, value = 200, step = 10),
                actionButton(ns("regenerar_sim_disp"),
                             "Nueva muestra aleatoria",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_disp"), height = "380px"),
              uiOutput(ns("insight_sim_disp")),
              tags$hr(),
              uiOutput(ns("cards_sim_disp"))
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
                    ns("fuente_datos_disp"),
                    label   = tagList(bs_icon("database", class = "me-1"),
                                      "Seleccionar dataset:"),
                    choices = c(
                      "Ping\u00fcinos Palmer \u2014 medidas corporales (ecolog\u00eda)" = "penguins",
                      "Peso al nacer \u2014 salud perinatal (Hosmer & Lemeshow)" = "birthwt",
                      "Salarios de profesores universitarios (ciencias sociales)" = "salaries",
                      "Actitud hacia el trabajo \u2014 encuesta organizacional (psicolog\u00eda)" = "attitude"
                    ),
                    selected = "penguins"
                  ),
                  tags$hr(),
                  uiOutput(ns("info_dataset_disp"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_disp")),
                    br(),
                    DTOutput(ns("tabla_preview_disp"))
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
                col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
                div(
                  p(class = "small text-muted mb-3",
                    bs_icon("info-circle", class = "me-1"),
                    "Sube un archivo CSV o Excel. ",
                    "La primera fila debe contener los nombres de las columnas."),
                  fileInput(
                    ns("archivo_disp"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar\u2026",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador_disp"),
                    label   = "Separador (CSV):",
                    choices = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    )
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio_disp"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio_disp")),
                    br(),
                    DTOutput(ns("tabla_preview_propio_disp"))
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
                "calcular medidas de dispersi\u00f3n \u2014 estas s\u00f3lo tienen sentido ",
                "para variables ", strong("num\u00e9ricas"), "."
              ),
              layout_columns(
                col_widths = breakpoints(sm = c(12, 12), lg = c(10, 2)),
                uiOutput(ns("tabla_tipos_disp")),
                div(
                  class = "pt-2",
                  actionButton(ns("aplicar_tipos_disp"), "Aplicar tipos",
                               class = "btn-primary w-100",
                               icon  = icon("check")),
                  br(), br(),
                  actionButton(ns("resetear_tipos_disp"), "Restaurar",
                               class = "btn-outline-secondary w-100 btn-sm",
                               icon  = icon("rotate-left"))
                )
              ),
              uiOutput(ns("tipos_aplicados_msg_disp"))
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
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                uiOutput(ns("sel_var_disp")),
                tags$hr(),
                checkboxInput(ns("agregar_outlier_disp"),
                              "Agregar un valor at\u00edpico",
                              value = FALSE),
                conditionalPanel(
                  condition = "input.agregar_outlier_disp == true",
                  ns = ns,
                  numericInput(ns("valor_outlier_disp"), "Valor del outlier:",
                               value = 0)
                )
              )
            ),
            div(
              plotOutput(ns("plot_boxplot_disp"), height = "380px"),
              uiOutput(ns("insight_disp")),
              tags$hr(),
              uiOutput(ns("cards_estadisticos_disp"))
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
          div(class = "codigo-bloque", verbatimTextOutput(ns("codigo_r_disp"))),
          downloadButton(ns("descargar_script_disp"), "Descargar script .R",
                        class = "btn-primary mt-3")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_dispersion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Simulación interactiva (teórica) ──────────────
    datos_sim_disp <- reactive({
      input$regenerar_sim_disp  # dependencia: botón regenera nueva muestra
      n    <- input$n_sim_disp
      sd0  <- input$sd_sim_disp
      pct  <- input$outliers_sim_disp
      req(n, sd0, !is.null(pct))

      x <- stats::rnorm(n, mean = 50, sd = sd0)
      n_out <- round(n * pct / 100)
      if (n_out > 0) {
        idx <- sample(seq_len(n), n_out)
        signo <- sample(c(-1, 1), n_out, replace = TRUE)
        x[idx] <- 50 + signo * (sd0 * stats::runif(n_out, 4, 7))
      }
      x
    })

    estadisticos_sim_disp <- reactive({
      x <- datos_sim_disp()
      req(length(x) > 1)
      list(
        rango = diff(range(x)),
        var   = stats::var(x),
        sd    = stats::sd(x),
        iqr   = stats::IQR(x),
        cv    = stats::sd(x) / mean(x) * 100
      )
    })

    output$cards_sim_disp <- renderUI({
      e <- estadisticos_sim_disp()
      tagList(
        tarjeta_metrica("Rango", round(e$rango, 2), "rango"),
        tarjeta_metrica("Varianza", round(e$var, 2), "varianza"),
        tarjeta_metrica("Desv. est\u00e1ndar", round(e$sd, 2), "sd"),
        tarjeta_metrica("IQR", round(e$iqr, 2), "iqr"),
        tarjeta_metrica("Coef. de variaci\u00f3n", round(e$cv, 1), "cv",
                        sufijo = "%", ultima = TRUE)
      )
    })

    output$plot_sim_disp <- renderPlot({
      x <- datos_sim_disp()
      req(length(x) > 1)
      df <- data.frame(x = x, grupo = "Muestra simulada")
      ggplot(df, aes(x = grupo, y = x)) +
        geom_boxplot(fill = colores$secundario, color = colores$primario,
                    alpha = 0.7, width = 0.35, outlier.color = colores$peligro,
                    outlier.size = 2.5) +
        geom_jitter(width = 0.1, alpha = 0.4, color = colores$texto, size = 1.3) +
        coord_flip() +
        labs(x = NULL, y = "Valor simulado") +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              axis.text.y = element_blank())
    })

    output$insight_sim_disp <- renderUI({
      e <- estadisticos_sim_disp()
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          strong("SD: "), round(e$sd, 2), " \u2014 ", strong("IQR: "),
          round(e$iqr, 2),
          if (input$outliers_sim_disp > 0)
            paste0(" Con ", input$outliers_sim_disp, "% de valores at\u00edpicos, ",
                   "la SD y la varianza se disparan mucho m\u00e1s que el IQR: ",
                   "esa es la robustez del IQR en acci\u00f3n.")
          else
            " Sube el slider de valores at\u00edpicos para comparar la sensibilidad de cada medida."
      )
    })

    # ── Datos reactivos (ejemplo) ─────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_disp
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
    output$info_dataset_disp <- renderUI({
      fuente <- input$fuente_datos_disp
      if (is.null(fuente)) return(NULL)
      switch(fuente,
        penguins = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Ping\u00fcinos Palmer."), " Medidas corporales de ",
          strong("344 ping\u00fcinos"), " de 3 especies en la Ant\u00e1rtida. ",
          "Buen ejemplo para comparar dispersi\u00f3n entre especies."
        ),
        birthwt = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Peso al nacer (Hosmer & Lemeshow)."), " Datos de ",
          strong("189 neonatos"), ". Incluye ", strong("bwt"),
          " (peso al nacer en gramos), con variabilidad influida por ",
          "factores de riesgo como tabaquismo e hipertensi\u00f3n."
        ),
        salaries = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: Salaries (carData)."), " Salarios de ",
          strong("397 profesores universitarios"), ". La dispersi\u00f3n en ",
          strong("salary"), " es considerable \u2014 el CV ayuda a comparar su ",
          "variabilidad con la de ", strong("yrs.service"), "."
        ),
        attitude = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Dataset: attitude."), " Encuesta de clima organizacional a ",
          strong("30 departamentos"), ". Variables en escala 0-100, \u00fatiles ",
          "para comparar cu\u00e1l pregunta gener\u00f3 respuestas m\u00e1s dispersas."
        )
      )
    })

    # ── Vista previa datos de ejemplo ────────────────
    output$cards_datos_disp <- renderUI({
      req(datos())
      d    <- datos()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
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

    output$tabla_preview_disp <- renderDT({
      req(datos())
      datatable(datos(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Datos propios ─────────────────────────────────
    datos_propio_disp <- reactive({
      req(input$archivo_disp)
      ext <- tools::file_ext(input$archivo_disp$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_disp$datapath)
        else
          readr::read_delim(input$archivo_disp$datapath,
                            delim = input$separador_disp %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error al leer archivo:", e$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio_disp(), { datos_mod(datos_propio_disp()) })

    output$resumen_datos_propio_disp <- renderUI({
      req(datos_propio_disp())
      d <- datos_propio_disp()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_disp <- renderUI({
      req(datos_propio_disp())
      d    <- datos_propio_disp()
      nnum <- sum(sapply(d, is.numeric))
      ncat <- sum(sapply(d, function(x) is.factor(x) || is.character(x)))
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

    output$tabla_preview_propio_disp <- renderDT({
      req(datos_propio_disp())
      datatable(datos_propio_disp(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_disp <- reactiveVal(NULL)

    output$tabla_tipos_disp <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_disp()
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

    observeEvent(input$aplicar_tipos_disp, {
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
      tipos_usuario_disp(tu)
      datos_mod(d)
      output$tipos_aplicados_msg_disp <- renderUI({
        div(class = "alert alert-success small mt-3",
            bs_icon("check-circle-fill", class = "me-1"),
            "Tipos de variable aplicados.")
      })
    })

    observeEvent(input$resetear_tipos_disp, {
      tipos_usuario_disp(NULL)
      datos_mod(datos())
      output$tipos_aplicados_msg_disp <- renderUI({
        div(class = "alert alert-info small mt-3",
            bs_icon("arrow-counterclockwise", class = "me-1"),
            "Tipos restaurados a los originales.")
      })
    })

    # ── Explorar y calcular ───────────────────────────
    vars_numericas_disp <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    output$sel_var_disp <- renderUI({
      req(vars_numericas_disp())
      selectInput(ns("var_disp"), "Variable num\u00e9rica:",
                  choices = vars_numericas_disp())
    })

    # Vector con outlier opcional agregado
    valores_disp <- reactive({
      req(datos_mod(), input$var_disp)
      x <- datos_mod()[[input$var_disp]]
      x <- x[!is.na(x)]
      if (isTRUE(input$agregar_outlier_disp) && !is.null(input$valor_outlier_disp)) {
        x <- c(x, input$valor_outlier_disp)
      }
      x
    })

    estadisticos_disp <- reactive({
      x <- valores_disp()
      req(length(x) > 1)
      list(
        n     = length(x),
        media = mean(x),
        rango = diff(range(x)),
        var   = stats::var(x),
        sd    = stats::sd(x),
        iqr   = stats::IQR(x),
        cv    = stats::sd(x) / mean(x) * 100
      )
    })

    output$cards_estadisticos_disp <- renderUI({
      e <- estadisticos_disp()
      tagList(
        tarjeta_metrica("Rango", round(e$rango, 2), "rango"),
        tarjeta_metrica("Varianza", round(e$var, 2), "varianza"),
        tarjeta_metrica("Desv. est\u00e1ndar", round(e$sd, 2), "sd"),
        tarjeta_metrica("IQR", round(e$iqr, 2), "iqr"),
        tarjeta_metrica("Coef. de variaci\u00f3n", round(e$cv, 1), "cv",
                        sufijo = "%", ultima = TRUE)
      )
    })

    output$plot_boxplot_disp <- renderPlot({
      x <- valores_disp()
      req(length(x) > 1)
      df <- data.frame(x = x, grupo = "Datos")
      ggplot(df, aes(x = grupo, y = x)) +
        geom_boxplot(fill = colores$secundario, color = colores$primario,
                    alpha = 0.7, width = 0.35, outlier.color = colores$peligro,
                    outlier.size = 2.5) +
        geom_jitter(width = 0.1, alpha = 0.4, color = colores$texto, size = 1.3) +
        coord_flip() +
        labs(x = NULL, y = input$var_disp) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              axis.text.y = element_blank())
    })

    output$insight_disp <- renderUI({
      e <- estadisticos_disp()
      if (isTRUE(input$agregar_outlier_disp)) {
        div(class = "alert alert-warning small mt-2",
            bs_icon("lightbulb-fill", class = "me-1"),
            strong("Con el outlier agregado: "),
            "fij\u00e1te c\u00f3mo el rango, la varianza y la desviaci\u00f3n est\u00e1ndar ",
            "se disparan, mientras que el IQR (", round(e$iqr, 2),
            ") apenas cambia \u2014 por eso se le llama una medida robusta."
        )
      } else {
        div(class = "alert alert-info small mt-2",
            bs_icon("lightbulb-fill", class = "me-1"),
            "Activa \"Agregar un valor at\u00edpico\" en los controles para ",
            "comparar c\u00f3mo responde el IQR (robusto) frente a la ",
            "desviaci\u00f3n est\u00e1ndar y la varianza (sensibles)."
        )
      }
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_disp <- renderText({
      req(input$var_disp)
      fuente <- input$fuente_datos_disp
      es_propio <- !is.null(datos_propio_disp()) && !is.null(input$archivo_disp)
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
        "# \u2500\u2500 Medidas de dispersi\u00f3n \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "# Dataset: ", nm_datos, "\n",
        "# Variable: ", input$var_disp, "\n\n",
        linea_carga, "\n\n",
        "x <- ", nm_datos, "$", input$var_disp, "\n\n",
        "diff(range(x))       # rango\n",
        "var(x)               # varianza\n",
        "sd(x)                # desviaci\u00f3n est\u00e1ndar\n",
        "IQR(x)               # rango intercuart\u00edlico\n",
        "sd(x) / mean(x) * 100  # coeficiente de variaci\u00f3n (%)\n"
      )
    })

    output$descargar_script_disp <- downloadHandler(
      filename = function() paste0("StatBasics_dispersion_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_disp(), file)
    )

  })
}
