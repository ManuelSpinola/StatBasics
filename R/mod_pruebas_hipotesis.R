# ============================================================
# mod_pruebas_hipotesis.R — Pruebas de hipótesis
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_pruebas_hipotesis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "py-3 px-2",
      h4(bs_icon("check2-square", class = "me-2"), "Pruebas de hip\u00f3tesis",
         style = paste0("color:", colores$primario, "; font-weight:700;")),
      p(class = "text-muted mb-0",
        "Una prueba de hip\u00f3tesis responde: ", strong("\u00bfson mis datos ",
        "compatibles con la hip\u00f3tesis nula (H\u2080), o son lo suficientemente ",
        "raros como para dudar de ella?")
      )
    ),
    navset_card_tab(

      nav_panel(title = tagList(bs_icon("book", class = "me-1"), "\u00bfQu\u00e9 es?"),
        card_body(
          paso_infografia(1, "signpost-split", colores$primario,
            "H\u2080 vs. H\u2081",
            p(class = "mb-0",
              strong("H\u2080 (nula): "), "no hay efecto/diferencia (ej.: la ",
              "media es igual a X). ", strong("H\u2081 (alternativa): "),
              "s\u00ed hay efecto/diferencia. La prueba nunca \"prueba\" H\u2080 ",
              "\u2014 solo decide si hay evidencia suficiente para rechazarla."
            )
          ),
          conector_infografia("Dos formas de equivocarte"),
          fluidRow(
            column(6, tarjeta_concepto("exclamation-circle", colores$peligro,
              "Error tipo I (\u03b1)",
              p(class="mb-0", "Rechazar H\u2080 cuando en realidad es verdadera ",
                "(\"falsa alarma\"). Por convenci\u00f3n, \u03b1 = 0.05: aceptas un ",
                "5% de riesgo de esto."))),
            column(6, tarjeta_concepto("exclamation-triangle", colores$acento,
              "Error tipo II (\u03b2)",
              p(class="mb-0", "NO rechazar H\u2080 cuando en realidad es falsa ",
                "(\"oportunidad perdida\"). El ", strong("poder"),
                " de la prueba es 1\u2212\u03b2 \u2014 aumenta con n."))
            )
          ),
          conector_infografia("Lo que un valor de p NO significa"),
          tarjeta_concepto("x-octagon", colores$peligro,
            "Errores comunes de interpretaci\u00f3n",
            tags$ul(class="mb-2",
              tags$li(strong("p NO es "), "la probabilidad de que H\u2080 sea ",
                     "verdadera (ni 1\u2212p la de H\u2081)."),
              tags$li(strong("p < 0.05 es una convenci\u00f3n"), ", no una l\u00ednea ",
                     "divisoria de la verdad \u2014 p=0.049 y p=0.051 no son ",
                     "realmente distintos."),
              tags$li(strong("Significancia estad\u00edstica \u2260 importancia ",
                     "pr\u00e1ctica"), ": con n enorme, hasta una diferencia ",
                     "trivial sale \"significativa\". Por eso el siguiente ",
                     "m\u00f3dulo (Tama\u00f1o del efecto) importa tanto como este.")
            ),
            p(class="mb-0 small text-muted",
              "Esto refleja la declaraci\u00f3n oficial de la American ",
              "Statistical Association (2016) sobre el uso e ",
              "interpretaci\u00f3n de valores de p.")
          ),
          p(class="text-muted mt-3 mb-0", "Compru\u00e9balo en ",
            strong("Simulaci\u00f3n interactiva"), " \u2014 simula cientos de ",
            "pruebas cuando H\u2080 es VERDADERA y observa qu\u00e9 % igual sale ",
            "\"significativo\" por puro azar.")
        )
      ),

      nav_panel(title = tagList(bs_icon("sliders", class="me-1"), "Simulaci\u00f3n interactiva"),
        card_body(
          p(class="small text-muted mb-3",
            "Se simulan muchas pruebas t de dos muestras cuando H\u2080 es ",
            "VERDADERA (ambos grupos vienen de la misma poblaci\u00f3n) \u2014 ",
            "cualquier \"significativo\" aqu\u00ed es, por definici\u00f3n, un falso ",
            "positivo (error tipo I)."),
          layout_columns(col_widths = c(4,8), fill = FALSE,
            card(card_header(bs_icon("sliders", class="me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("alpha_sim_ph"), "Nivel de significancia (\u03b1):",
                            min=0.01, max=0.20, value=0.05, step=0.01),
                sliderInput(ns("n_sim_ph"), "Tama\u00f1o de cada grupo (n):",
                            min=5, max=200, value=30, step=5),
                sliderInput(ns("reps_sim_ph"), "N\u00famero de pruebas simuladas:",
                            min=100, max=2000, value=500, step=100),
                actionButton(ns("regenerar_sim_ph"), "Nueva simulaci\u00f3n",
                             class="btn-outline-secondary w-100 btn-sm", icon=icon("shuffle")),
                tags$hr(), uiOutput(ns("cards_sim_ph"))
              )),
            div(plotOutput(ns("plot_sim_ph"), height="380px"),
                uiOutput(ns("insight_sim_ph")))
          )
        )
      ),

      nav_panel(title = tagList(bs_icon("clipboard-data", class="me-1"), "Practica con datos reales"),
        card_body(
          p(class="small text-muted mb-3",
            "Compara una variable num\u00e9rica entre dos grupos de un dataset."),
          layout_columns(col_widths = c(4,8), fill = FALSE,
            card(card_header(bs_icon("sliders", class="me-1"), "Par\u00e1metros"),
              card_body(
                radioButtons(ns("fuente_datos_ph"), "Dataset:",
                  choices = c("Ping\u00fcinos (palmerpenguins)" = "penguins",
                             "Peso al nacer (MASS::birthwt)" = "birthwt",
                             "Salarios (carData::Salaries)" = "salaries"),
                  selected = "penguins"),
                uiOutput(ns("sel_var_num_ph")),
                uiOutput(ns("sel_var_grupo_ph")),
                sliderInput(ns("alpha_practica_ph"), "Nivel de significancia (\u03b1):",
                            min=0.01, max=0.20, value=0.05, step=0.01),
                actionButton(ns("calcular_ph"), "Correr prueba t",
                             class="btn-primary w-100 btn-sm", icon=icon("play")),
                tags$hr(), uiOutput(ns("cards_practica_ph"))
              )),
            div(plotOutput(ns("plot_practica_ph"), height="320px"),
                uiOutput(ns("insight_practica_ph")))
          )
        )
      ),

      nav_panel(title = tagList(bs_icon("code-slash", class="me-1"), "C\u00f3digo R"),
        card_body(
          verbatimTextOutput(ns("codigo_r_ph")),
          downloadButton(ns("descargar_script_ph"), "Descargar script .R",
                         class="btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

mod_pruebas_hipotesis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sim_ph <- reactive({
      input$regenerar_sim_ph
      alpha <- input$alpha_sim_ph; n <- input$n_sim_ph; reps <- input$reps_sim_ph
      req(alpha, n, reps)
      pvals <- replicate(reps, {
        g1 <- stats::rnorm(n); g2 <- stats::rnorm(n)
        stats::t.test(g1, g2)$p.value
      })
      list(pvals = pvals, alpha = alpha)
    })

    output$cards_sim_ph <- renderUI({
      s <- sim_ph()
      tasa <- mean(s$pvals < s$alpha) * 100
      tagList(
        tarjeta_metrica("\u03b1 (nominal)", paste0(round(s$alpha*100,1), "%"), "r_correlacion"),
        tarjeta_metrica("Falsos positivos observados", paste0(round(tasa,1), "%"),
                        "r_correlacion", ultima = TRUE)
      )
    })

    output$plot_sim_ph <- renderPlot({
      s <- sim_ph()
      df <- data.frame(p = s$pvals)
      ggplot(df, aes(x = p)) +
        geom_histogram(bins = 30, fill = colores$primario, color = "white", alpha = 0.85) +
        geom_vline(xintercept = s$alpha, color = colores$peligro, linetype = "dashed", linewidth = 1) +
        labs(x = "Valor p", y = "Frecuencia",
             title = "Distribuci\u00f3n de p cuando H\u2080 es verdadera (debe ser uniforme)") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 12))
    })

    output$insight_sim_ph <- renderUI({
      s <- sim_ph()
      tasa <- mean(s$pvals < s$alpha) * 100
      div(class = "alert alert-info small mt-2", bs_icon("lightbulb-fill", class="me-1"),
          paste0("Aunque H\u2080 es verdadera (no hay diferencia real), el ",
                round(tasa,1), "% de las pruebas igual sali\u00f3 \"significativa\" ",
                "\u2014 muy cerca del ", round(s$alpha*100,1), "% que promete \u03b1. ",
                "Esto NO es un error del m\u00e9todo: es exactamente lo que \u03b1 ",
                "significa \u2014 el riesgo de falsa alarma que aceptas de antemano."))
    })

    datos_ph <- reactive({
      switch(input$fuente_datos_ph,
        penguins = as.data.frame(palmerpenguins::penguins),
        birthwt  = MASS::birthwt,
        salaries = carData::Salaries
      )
    })

    output$sel_var_num_ph <- renderUI({
      d <- datos_ph(); vars <- names(which(sapply(d, is.numeric)))
      selectInput(ns("var_num_ph"), "Variable num\u00e9rica:", choices = vars)
    })

    output$sel_var_grupo_ph <- renderUI({
      d <- datos_ph()
      vars <- names(which(sapply(d, function(x) (is.factor(x)||is.character(x)) &&
                                  length(unique(stats::na.omit(x))) == 2)))
      req(length(vars) > 0)
      selectInput(ns("var_grupo_ph"), "Variable de grupo (2 niveles):", choices = vars)
    })

    resultado_ph <- eventReactive(input$calcular_ph, {
      d <- datos_ph()
      req(input$var_num_ph, input$var_grupo_ph)
      d <- d[, c(input$var_num_ph, input$var_grupo_ph)]
      names(d) <- c("y", "grupo")
      d <- d[stats::complete.cases(d), ]
      d$grupo <- droplevels(as.factor(d$grupo))
      test <- stats::t.test(y ~ grupo, data = d)
      list(test = test, d = d, alpha = input$alpha_practica_ph)
    })

    output$cards_practica_ph <- renderUI({
      req(resultado_ph())
      r <- resultado_ph()
      tagList(
        tarjeta_metrica("Diferencia de medias", round(diff(rev(r$test$estimate)), 2), "media"),
        tarjeta_metrica("Valor p", format.pval(r$test$p.value, digits = 3), "media",
                        ultima = TRUE)
      )
    })

    output$plot_practica_ph <- renderPlot({
      req(resultado_ph())
      r <- resultado_ph()
      ggplot(r$d, aes(x = grupo, y = y, fill = grupo)) +
        geom_boxplot(show.legend = FALSE, alpha = 0.8) +
        scale_fill_manual(values = c(colores$primario, colores$acento)) +
        labs(x = NULL, y = input$var_num_ph,
             title = paste0("t = ", round(r$test$statistic, 2), ", p = ",
                           format.pval(r$test$p.value, digits = 3))) +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 13))
    })

    output$insight_practica_ph <- renderUI({
      req(resultado_ph())
      r <- resultado_ph()
      sig <- r$test$p.value < r$alpha
      div(class = "alert alert-info small mt-2", bs_icon("lightbulb-fill", class="me-1"),
          paste0("Con \u03b1 = ", r$alpha, ", el resultado es ",
                if (sig) "estad\u00edsticamente significativo" else "no estad\u00edsticamente significativo",
                " (p = ", format.pval(r$test$p.value, digits=3), "). Recuerda: esto no dice ",
                "qu\u00e9 tan GRANDE es la diferencia \u2014 para eso, ve al m\u00f3dulo de Tama\u00f1o del efecto."))
    })

    output$codigo_r_ph <- renderText({
      req(input$var_num_ph, input$var_grupo_ph)
      paste0(
        "# \u2500\u2500 Prueba t de dos muestras \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "t.test(", input$var_num_ph, " ~ ", input$var_grupo_ph, ", data = datos)\n"
      )
    })
    output$descargar_script_ph <- downloadHandler(
      filename = function() paste0("StatBasics_pruebas_hipotesis_", Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_ph(), file)
    )
  })
}
