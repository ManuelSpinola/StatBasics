# ============================================================
# mod_probabilidad.R — Probabilidad básica
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Simulación interactiva (ley de los grandes números)
#   3. Teorema de Bayes (introducción intuitiva)
#   4. Calculadora de Bayes (interactiva, con tus propios valores)
#   5. Código R
#
# Nota pedagógica: este módulo es el puente hacia StatBayes. No
# usa datasets reales (por eso no tiene "Los datos"/"Practica
# con datos reales") — usa ejemplos simulados (monedas, dados,
# diagn\u00f3sticos m\u00e9dicos), como plantea el t\u00edtulo del módulo.
# El punto culminante es el Teorema de Bayes explicado con
# frecuencias naturales (el m\u00e9todo de Gigerenzer, mucho m\u00e1s
# intuitivo que la f\u00f3rmula sola), conectando directamente con
# c\u00f3mo StatBayes combina prior + evidencia = posterior.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_probabilidad_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("dice-5", class = "me-2"),
        "Probabilidad b\u00e1sica",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "La probabilidad se puede entender de dos formas: como ",
        strong("frecuencia relativa a largo plazo"), " (si repites un ",
        "experimento muchas veces, \u00bfqu\u00e9 proporci\u00f3n de veces ocurre el ",
        "evento?) o como ", strong("grado de creencia"), " (qu\u00e9 tan ",
        "cre\u00edble es una afirmaci\u00f3n, dada la evidencia). La primera es la ",
        "base de la estad\u00edstica cl\u00e1sica; la segunda es la base de la ",
        "estad\u00edstica bayesiana."
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
            1, "dice-5", colores$primario,
            "\u00bfQu\u00e9 es la probabilidad?",
            p(class = "mb-0",
              "Un n\u00famero entre ", strong("0"), " (imposible) y ", strong(
              "1"), " (seguro) que mide qu\u00e9 tan probable es que ocurra un ",
              "evento. Por ejemplo, P(cara al lanzar una moneda) = 0.5."
            )
          ),

          conector_infografia("Cuatro reglas básicas que necesitas conocer"),

          fluidRow(
            column(6,
              tarjeta_concepto(
                "plus-circle", colores$primario, "Regla de la suma",
                p(class = "mb-0",
                  "P(A o B) = P(A) + P(B) \u2212 P(A y B). Si A y B no pueden ",
                  "ocurrir juntos (son mutuamente excluyentes), se ",
                  "simplifica a P(A) + P(B)."
                )
              )
            ),
            column(6,
              tarjeta_concepto(
                "dash-circle", colores$acento, "Regla del complemento",
                p(class = "mb-0",
                  "P(no A) = 1 \u2212 P(A). Si P(lluvia) = 0.3, entonces ",
                  "P(no lluvia) = 0.7. Siempre suman 1."
                )
              )
            )
          ),
          fluidRow(
            column(6,
              tarjeta_concepto(
                "x-circle", colores$secundario, "Regla de la multiplicaci\u00f3n",
                p(class = "mb-0",
                  "Si A y B son ", strong("independientes"), " (uno no afecta ",
                  "al otro): P(A y B) = P(A) \u00d7 P(B). Ej.: P(dos caras ",
                  "seguidas) = 0.5 \u00d7 0.5 = 0.25."
                )
              )
            ),
            column(6,
              tarjeta_concepto(
                "signpost-split", colores$peligro, "Probabilidad condicional",
                p(class = "mb-0",
                  "P(A | B) = probabilidad de A ", strong("dado que"), " B ya ",
                  "ocurri\u00f3. Puede ser muy distinta de P(A) \u2014 este es el ",
                  "concepto que abre la puerta al Teorema de Bayes."
                )
              )
            )
          ),

          p(class = "text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en ", strong("Simulaci\u00f3n interactiva"),
            " \u2014 lanza una moneda miles de veces y observa c\u00f3mo la ",
            "frecuencia relativa converge a la probabilidad te\u00f3rica."
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
            "La ", strong("ley de los grandes n\u00fameros"), ": a medida que ",
            "repites un experimento aleatorio m\u00e1s veces, la frecuencia ",
            "relativa observada converge a la probabilidad te\u00f3rica."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                radioButtons(ns("experimento_sim_prob"), "Experimento:",
                             choices = c("Moneda (P=0.5)" = "moneda",
                                        "Dado, sacar un 6 (P=1/6)" = "dado"),
                             selected = "moneda"),
                sliderInput(ns("n_lanzamientos_sim_prob"),
                            "N\u00famero de lanzamientos:",
                            min = 10, max = 5000, value = 500, step = 10),
                actionButton(ns("regenerar_sim_prob"), "Nueva simulaci\u00f3n",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle")),
                tags$hr(),
                uiOutput(ns("cards_sim_prob"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_prob"), height = "380px"),
              uiOutput(ns("insight_sim_prob"))
            )
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 3: Teorema de Bayes
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("arrow-repeat", class = "me-1"),
                        "Teorema de Bayes"),
        card_body(

          paso_infografia(
            1, "question-circle", colores$secundario,
            "El problema: confundir P(A|B) con P(B|A)",
            p(class = "mb-2",
              "Un examen m\u00e9dico tiene 99% de ", strong("sensibilidad"),
              " (detecta correctamente al 99% de los enfermos) y 95% de ",
              strong("especificidad"), " (descarta correctamente al 95% de ",
              "los sanos). La enfermedad afecta al 1% de la poblaci\u00f3n. ",
              "Si a alguien le sale positivo, ", em("\u00bfqu\u00e9 probabilidad ",
              "hay de que realmente est\u00e9 enfermo?")
            ),
            div(class = "alert alert-warning small mb-0",
                bs_icon("exclamation-triangle", class = "me-1"),
                "La intuici\u00f3n com\u00fan dice \"como el 99%\" \u2014 pero eso es ",
                "confundir P(positivo | enfermo) [lo que sabemos del test] ",
                "con P(enfermo | positivo) [lo que realmente queremos ",
                "saber]. Son n\u00fameros muy distintos."
            )
          ),

          conector_infografia("Razonemos con frecuencias naturales (mucho más fácil que la fórmula)"),

          paso_infografia(
            2, "grid-3x3", colores$primario,
            "De 10,000 personas...",
            uiOutput(ns("frecuencias_naturales_bayes"))
          ),

          conector_infografia("Eso es exactamente el Teorema de Bayes"),

          tarjeta_concepto(
            "arrow-repeat", colores$acento,
            "La f\u00f3rmula (lo mismo que ya calculaste arriba)",
            div(style = paste0("background:", colores$fondo,
                              "; border-radius:8px; padding:14px;",
                              "text-align:center; font-size:1.1rem;",
                              "font-weight:600; color:", colores$primario, ";"),
                "P(A | B) = P(B | A) \u00d7 P(A) / P(B)"
            ),
            p(class = "mt-2 mb-0 small text-muted",
              "P(A) es el ", strong("prior"), " (lo que sab\u00edas antes: la ",
              "prevalencia). P(B|A) es la ", strong("verosimilitud"), " (qu\u00e9 ",
              "tan bien explican los datos esa hip\u00f3tesis). P(A|B) es el ",
              strong("posterior"), " (tu creencia actualizada, ya con la ",
              "evidencia). ", strong("Esto es exactamente lo que hace ",
              "StatBayes"), ", pero en vez de \"enfermo/sano\" el par\u00e1metro ",
              "A puede ser una media, una proporci\u00f3n, o cualquier otro ",
              "par\u00e1metro de un modelo."
            )
          ),

          p(class = "text-muted mt-3 mb-0",
            "Ajusta los valores t\u00fa mismo en ", strong("Calculadora de Bayes"),
            " \u2014 cambia la prevalencia, sensibilidad y especificidad y ",
            "mira c\u00f3mo cambia el resultado."
          )
        )
      ),

      # ════════════════════════════════════════════════
      # PESTAÑA 4: Calculadora de Bayes
      # ════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("calculator", class = "me-1"),
                        "Calculadora de Bayes"),
        card_body(
          p(class = "small text-muted mb-3",
            "Cambia los tres valores y observa c\u00f3mo cambia P(enfermo | ",
            "positivo)."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("prevalencia_calc"), "Prevalencia (prior):",
                            min = 0.001, max = 0.5, value = 0.01, step = 0.001),
                sliderInput(ns("sensibilidad_calc"), "Sensibilidad:",
                            min = 0.5, max = 0.999, value = 0.99, step = 0.001),
                sliderInput(ns("especificidad_calc"), "Especificidad:",
                            min = 0.5, max = 0.999, value = 0.95, step = 0.001),
                tags$hr(),
                uiOutput(ns("cards_calc_bayes"))
              )
            ),
            div(
              plotOutput(ns("plot_calc_bayes"), height = "300px"),
              uiOutput(ns("insight_calc_bayes"))
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
            strong("Calculadora de Bayes"), "."
          ),
          verbatimTextOutput(ns("codigo_r_prob")),
          downloadButton(ns("descargar_script_prob"), "Descargar script .R",
                         class = "btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_probabilidad_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Simulación interactiva (ley de grandes números) ──
    datos_sim_prob <- reactive({
      input$regenerar_sim_prob
      exp_tipo <- input$experimento_sim_prob
      n        <- input$n_lanzamientos_sim_prob
      req(exp_tipo, n)

      if (exp_tipo == "moneda") {
        p_teorica <- 0.5
        resultados <- stats::rbinom(n, 1, p_teorica)
      } else {
        p_teorica <- 1 / 6
        resultados <- as.numeric(sample(1:6, n, replace = TRUE) == 6)
      }
      frec_relativa <- cumsum(resultados) / seq_len(n)
      list(p_teorica = p_teorica, frec_relativa = frec_relativa, n = n)
    })

    output$cards_sim_prob <- renderUI({
      sim <- datos_sim_prob()
      tagList(
        tarjeta_metrica("Probabilidad te\u00f3rica", round(sim$p_teorica, 3),
                        "r_correlacion"),
        tarjeta_metrica("Frecuencia relativa final",
                        round(sim$frec_relativa[sim$n], 3), "r_correlacion",
                        ultima = TRUE)
      )
    })

    output$plot_sim_prob <- renderPlot({
      sim <- datos_sim_prob()
      df  <- data.frame(lanzamiento = seq_len(sim$n),
                        frec = sim$frec_relativa)
      ggplot(df, aes(x = lanzamiento, y = frec)) +
        geom_line(color = colores$primario, linewidth = 0.8) +
        geom_hline(yintercept = sim$p_teorica, linetype = "dashed",
                  color = colores$peligro, linewidth = 1) +
        annotate("text", x = sim$n * 0.85, y = sim$p_teorica,
                 label = paste0("P te\u00f3rica = ", round(sim$p_teorica, 3)),
                 vjust = -1, color = colores$peligro, fontface = "bold",
                 size = 3.5) +
        labs(x = "N\u00famero de lanzamientos", y = "Frecuencia relativa acumulada",
             title = "Ley de los grandes n\u00fameros") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 13))
    })

    output$insight_sim_prob <- renderUI({
      sim <- datos_sim_prob()
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("Con pocos lanzamientos la frecuencia relativa \"salta\" ",
                "mucho \u2014 con ", sim$n, " lanzamientos, ya se acerca bastante ",
                "a la probabilidad te\u00f3rica (", round(sim$p_teorica, 3),
                "). Sube el n\u00famero de lanzamientos para verla estabilizarse ",
                "casi por completo.")
      )
    })

    # ── Teorema de Bayes: frecuencias naturales (fijo) ──
    output$frecuencias_naturales_bayes <- renderUI({
      res <- bayes_frecuencias_naturales(0.01, 0.99, 0.95)
      tagList(
        tags$table(
          class = "table table-sm table-bordered",
          tags$thead(
            tags$tr(
              tags$th("", style = paste0("background:", colores$primario, "; color:#fff;")),
              tags$th("Test positivo", style = paste0("background:", colores$primario, "; color:#fff;")),
              tags$th("Test negativo", style = paste0("background:", colores$primario, "; color:#fff;")),
              tags$th("Total", style = paste0("background:", colores$primario, "; color:#fff;"))
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(strong("Enfermos (1%)")),
              tags$td(paste0(res$vp, " (verdaderos positivos)")),
              tags$td(paste0(res$fn, " (falsos negativos)")),
              tags$td(res$enfermos)
            ),
            tags$tr(
              tags$td(strong("Sanos (99%)")),
              tags$td(paste0(res$fp, " (falsos positivos)")),
              tags$td(paste0(res$vn, " (verdaderos negativos)")),
              tags$td(res$sanos)
            ),
            tags$tr(
              style = paste0("background:", colores$fondo, "; font-weight:700;"),
              tags$td("Total"), tags$td(res$total_positivos),
              tags$td(res$poblacion - res$total_positivos), tags$td(res$poblacion)
            )
          )
        ),
        div(class = "alert alert-success small mt-2 mb-0",
            bs_icon("check-circle-fill", class = "me-1"),
            paste0("De los ", res$total_positivos, " que dieron positivo, ",
                  "solo ", res$vp, " realmente est\u00e1n enfermos: P(enfermo | ",
                  "positivo) = ", res$vp, "/", res$total_positivos, " = ",
                  round(100 * res$p_enfermo_dado_positivo, 1), "% \u2014 ",
                  "much\u00edsimo menos que el 99% intuitivo, porque la ",
                  "enfermedad es rara (prior bajo) y la mayor\u00eda de ",
                  "positivos vienen de los falsos positivos entre los ",
                  "much\u00edsimos sanos.")
        )
      )
    })

    # ── Calculadora de Bayes interactiva ──────────────
    resultado_calc_bayes <- reactive({
      req(input$prevalencia_calc, input$sensibilidad_calc,
          input$especificidad_calc)
      bayes_frecuencias_naturales(input$prevalencia_calc,
                                  input$sensibilidad_calc,
                                  input$especificidad_calc)
    })

    output$cards_calc_bayes <- renderUI({
      res <- resultado_calc_bayes()
      tagList(
        tarjeta_metrica("P(enfermo | positivo)",
                        paste0(round(100 * res$p_enfermo_dado_positivo, 1), "%"),
                        "r_correlacion", ultima = TRUE)
      )
    })

    output$plot_calc_bayes <- renderPlot({
      res <- resultado_calc_bayes()
      df <- data.frame(
        categoria = factor(c("Verdaderos\npositivos", "Falsos\npositivos",
                             "Falsos\nnegativos", "Verdaderos\nnegativos"),
                          levels = c("Verdaderos\npositivos", "Falsos\npositivos",
                                    "Falsos\nnegativos", "Verdaderos\nnegativos")),
        conteo = c(res$vp, res$fp, res$fn, res$vn)
      )
      ggplot(df, aes(x = categoria, y = conteo, fill = categoria)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = conteo), vjust = -0.4, size = 4) +
        scale_fill_manual(values = c(colores$primario, colores$peligro,
                                    colores$acento, colores$secundario)) +
        labs(x = NULL, y = paste0("Personas (de ", res$poblacion, ")"),
             title = "Desglose en frecuencias naturales") +
        theme_minimal(base_size = 12) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 13))
    })

    output$insight_calc_bayes <- renderUI({
      res <- resultado_calc_bayes()
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("Con una prevalencia de ", round(100 * input$prevalencia_calc, 2),
                "%, de ", res$poblacion, " personas hay ", res$total_positivos,
                " positivos en total, pero solo ", res$vp, " realmente ",
                "enfermos. Baja la prevalencia y observa c\u00f3mo P(enfermo | ",
                "positivo) cae \u2014 entre m\u00e1s rara sea la condici\u00f3n, m\u00e1s ",
                "importa este efecto.")
      )
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_prob <- renderText({
      req(input$prevalencia_calc, input$sensibilidad_calc,
          input$especificidad_calc)
      paste0(
        "# \u2500\u2500 Teorema de Bayes: diagn\u00f3stico m\u00e9dico \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "prevalencia   <- ", input$prevalencia_calc, "  # P(enfermo), el prior\n",
        "sensibilidad  <- ", input$sensibilidad_calc, "  # P(positivo | enfermo)\n",
        "especificidad <- ", input$especificidad_calc, "  # P(negativo | sano)\n\n",
        "# Frecuencias naturales (de 10,000 personas)\n",
        "poblacion <- 10000\n",
        "enfermos  <- round(poblacion * prevalencia)\n",
        "sanos     <- poblacion - enfermos\n\n",
        "verdaderos_positivos <- round(enfermos * sensibilidad)\n",
        "falsos_positivos     <- round(sanos * (1 - especificidad))\n\n",
        "# Teorema de Bayes\n",
        "p_enfermo_dado_positivo <- verdaderos_positivos /\n",
        "  (verdaderos_positivos + falsos_positivos)\n\n",
        "p_enfermo_dado_positivo\n"
      )
    })

    output$descargar_script_prob <- downloadHandler(
      filename = function() paste0("StatBasics_probabilidad_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_prob(), file)
    )

  })
}
