# ============================================================
# mod_tamano_efecto.R — Tamaño del efecto
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_tamano_efecto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "py-3 px-2",
      h4(bs_icon("rulers", class = "me-2"), "Tama\u00f1o del efecto",
         style = paste0("color:", colores$primario, "; font-weight:700;")),
      p(class = "text-muted mb-0",
        "Una prueba de hip\u00f3tesis te dice si hay evidencia de una ",
        "diferencia. El tama\u00f1o del efecto te dice ", strong("qu\u00e9 tan ",
        "grande es esa diferencia"), " \u2014 la pregunta que realmente ",
        "importa en la pr\u00e1ctica."
      )
    ),
    navset_card_tab(

      nav_panel(title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(
          paso_infografia(1, "exclamation-triangle", colores$peligro,
            "El problema: \"significativo\" no es \"grande\"",
            p(class = "mb-0",
              "Con una muestra enorme, hasta una diferencia trivial (0.1 kg ",
              "de peso, por ejemplo) puede salir ", strong("estad\u00edsticamente ",
              "significativa"), ". El valor p depende de n; el tama\u00f1o del ",
              "efecto NO \u2014 mide la magnitud real de la diferencia."
            )
          ),
          conector_infografia("Dos formas de medir esa magnitud"),
          fluidRow(
            column(6, tarjeta_concepto("rulers", colores$primario,
              "No estandarizado (el m\u00e1s \u00fatil)",
              p(class="mb-0",
                "La diferencia en las ", strong("unidades originales"), " ",
                "(ej.: \"3.2 kg m\u00e1s\", \"5 d\u00edas menos de recuperaci\u00f3n\"). ",
                strong("Es el m\u00e1s f\u00e1cil de interpretar"), " para tomar ",
                "decisiones reales \u2014 casi siempre es lo primero que ",
                "deber\u00edas reportar."))),
            column(6, tarjeta_concepto("arrows-angle-contract", colores$acento,
              "Estandarizado (Cohen's d, r)",
              p(class="mb-0",
                "La diferencia expresada en ", strong("desviaciones ",
                "est\u00e1ndar"), " (Cohen's d) o como correlaci\u00f3n (r). No ",
                "tiene unidades \u2014 por eso permite comparar resultados de ",
                "estudios distintos o hacer meta-an\u00e1lisis, pero pierde el ",
                "significado concreto de las unidades originales."))
            )
          ),
          tarjeta_concepto("bar-chart-line", colores$secundario,
            "Regla general para interpretar Cohen's d",
            tags$table(class="table table-sm table-bordered mb-0",
              tags$thead(tags$tr(
                tags$th("d", style=paste0("background:",colores$primario,";color:#fff;")),
                tags$th("Interpretaci\u00f3n", style=paste0("background:",colores$primario,";color:#fff;")))),
              tags$tbody(
                tags$tr(tags$td("0.2"), tags$td("Peque\u00f1o")),
                tags$tr(tags$td("0.5"), tags$td("Moderado")),
                tags$tr(tags$td("0.8+"), tags$td("Grande"))
              )
            )
          ),
          p(class="text-muted mt-3 mb-0",
            "Compru\u00e9balo en ", strong("Practica con datos reales"),
            " \u2014 ver\u00e1s ambos tipos de tama\u00f1o de efecto lado a lado, ",
            "para la misma comparaci\u00f3n.")
        )
      ),

      nav_panel(title = tagList(bs_icon("clipboard-data", class="me-1"), "Practica con datos reales"),
        card_body(
          p(class="small text-muted mb-3",
            "Compara una variable num\u00e9rica entre dos grupos \u2014 se ",
            "calculan ambos tipos de tama\u00f1o de efecto."),
          layout_columns(col_widths = c(4,8), fill = FALSE,
            card(card_header(bs_icon("sliders", class="me-1"), "Par\u00e1metros"),
              card_body(
                radioButtons(ns("fuente_datos_te"), "Dataset:",
                  choices = c("Ping\u00fcinos (palmerpenguins)" = "penguins",
                             "Peso al nacer (MASS::birthwt)" = "birthwt",
                             "Salarios (carData::Salaries)" = "salaries"),
                  selected = "penguins"),
                uiOutput(ns("sel_var_num_te")),
                uiOutput(ns("sel_var_grupo_te")),
                actionButton(ns("calcular_te"), "Calcular tama\u00f1o del efecto",
                             class="btn-primary w-100 btn-sm", icon=icon("play")),
                tags$hr(), uiOutput(ns("cards_practica_te"))
              )),
            div(plotOutput(ns("plot_practica_te"), height="320px"),
                uiOutput(ns("insight_practica_te")))
          )
        )
      ),

      nav_panel(title = tagList(bs_icon("code-slash", class="me-1"), "C\u00f3digo R"),
        card_body(
          verbatimTextOutput(ns("codigo_r_te")),
          downloadButton(ns("descargar_script_te"), "Descargar script .R",
                         class="btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

mod_tamano_efecto_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    datos_te <- reactive({
      switch(input$fuente_datos_te,
        penguins = as.data.frame(palmerpenguins::penguins),
        birthwt  = MASS::birthwt,
        salaries = carData::Salaries
      )
    })

    output$sel_var_num_te <- renderUI({
      d <- datos_te(); vars <- names(which(sapply(d, is.numeric)))
      selectInput(ns("var_num_te"), "Variable num\u00e9rica:", choices = vars)
    })

    output$sel_var_grupo_te <- renderUI({
      d <- datos_te()
      vars <- names(which(sapply(d, function(x) (is.factor(x)||is.character(x)) &&
                                  length(unique(stats::na.omit(x))) == 2)))
      req(length(vars) > 0)
      selectInput(ns("var_grupo_te"), "Variable de grupo (2 niveles):", choices = vars)
    })

    resultado_te <- eventReactive(input$calcular_te, {
      d <- datos_te()
      req(input$var_num_te, input$var_grupo_te)
      d <- d[, c(input$var_num_te, input$var_grupo_te)]
      names(d) <- c("y", "grupo")
      d <- d[stats::complete.cases(d), ]
      d$grupo <- droplevels(as.factor(d$grupo))
      g <- split(d$y, d$grupo)
      m1 <- mean(g[[1]]); m2 <- mean(g[[2]])
      n1 <- length(g[[1]]); n2 <- length(g[[2]])
      s1 <- stats::sd(g[[1]]); s2 <- stats::sd(g[[2]])
      sd_combinada <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
      dif_no_estandarizada <- m2 - m1
      cohen_d <- dif_no_estandarizada / sd_combinada
      list(d = d, m1 = m1, m2 = m2, dif = dif_no_estandarizada,
           cohen_d = cohen_d, niveles = levels(d$grupo))
    })

    output$cards_practica_te <- renderUI({
      req(resultado_te())
      r <- resultado_te()
      tagList(
        tarjeta_metrica("Diferencia (no estandarizada)",
                        paste0(round(r$dif, 2), " ", input$var_num_te), "media"),
        tarjeta_metrica("Cohen's d (estandarizado)",
                        paste0(round(r$cohen_d, 2), " (",
                              interpretar_cohen_d(r$cohen_d), ")"),
                        "media", ultima = TRUE)
      )
    })

    output$plot_practica_te <- renderPlot({
      req(resultado_te())
      r <- resultado_te()
      ggplot(r$d, aes(x = grupo, y = y, fill = grupo)) +
        geom_boxplot(show.legend = FALSE, alpha = 0.8) +
        scale_fill_manual(values = c(colores$primario, colores$acento)) +
        labs(x = NULL, y = input$var_num_te,
             title = paste0("Diferencia = ", round(r$dif, 2), " | Cohen's d = ",
                           round(r$cohen_d, 2))) +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 13))
    })

    output$insight_practica_te <- renderUI({
      req(resultado_te())
      r <- resultado_te()
      div(class = "alert alert-info small mt-2", bs_icon("lightbulb-fill", class="me-1"),
          paste0("La diferencia real es ", round(r$dif,2), " ", input$var_num_te,
                " entre '", r$niveles[2], "' y '", r$niveles[1], "' \u2014 eso es lo ",
                "que le importa a alguien tomando una decisi\u00f3n pr\u00e1ctica. El Cohen's d (",
                round(r$cohen_d,2), ") dice que esa diferencia es de tama\u00f1o ",
                interpretar_cohen_d(r$cohen_d), " en t\u00e9rminos de desviaciones est\u00e1ndar ",
                "\u2014 \u00fatil para comparar con otros estudios, pero por s\u00ed solo no te dice ",
                "nada sobre las unidades reales."))
    })

    output$codigo_r_te <- renderText({
      req(input$var_num_te, input$var_grupo_te)
      paste0(
        "# \u2500\u2500 Tama\u00f1o del efecto: no estandarizado y Cohen's d \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "g <- split(datos$", input$var_num_te, ", datos$", input$var_grupo_te, ")\n\n",
        "# No estandarizado (unidades originales)\n",
        "dif <- mean(g[[2]]) - mean(g[[1]])\n\n",
        "# Cohen's d (estandarizado)\n",
        "n1 <- length(g[[1]]); n2 <- length(g[[2]])\n",
        "sd_combinada <- sqrt(((n1-1)*sd(g[[1]])^2 + (n2-1)*sd(g[[2]])^2) / (n1+n2-2))\n",
        "cohen_d <- dif / sd_combinada\n\n",
        "c(diferencia = dif, cohen_d = cohen_d)\n",
        "\n# Tambi\u00e9n disponible en el paquete effectsize:\n",
        "# effectsize::cohens_d(", input$var_num_te, " ~ ", input$var_grupo_te,
        ", data = datos)\n"
      )
    })
    output$descargar_script_te <- downloadHandler(
      filename = function() paste0("StatBasics_tamano_efecto_", Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_te(), file)
    )
  })
}
