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
            p(class = "mb-2",
              strong("H\u2080 (nula): "), "no hay efecto/diferencia (ej.: la ",
              "media poblacional es 175 kg, como con las dantas). ", strong(
              "H\u2081 (alternativa): "), "s\u00ed hay diferencia. La prueba nunca ",
              "\"prueba\" H\u2080 \u2014 solo decide si hay evidencia suficiente ",
              "para rechazarla."
            ),
            plotOutput(ns("plot_intro_ph"), height = "260px"),
            tags$ul(class = "mt-2 mb-0 small text-muted ps-3",
              tags$li(strong("La curva azul: "), "muestra todos los valores ",
                     "que ser\u00edan normales de observar ", em("si H\u2080 fuera ",
                     "cierta"), " (si \u03bc realmente fuera 175)."),
              tags$li(strong("La zona roja: "), "son los valores tan alejados ",
                     "de 175 que ser\u00edan raros de ver si H\u2080 fuera cierta \u2014 ",
                     "por eso se llama \"zona de rechazo\"."),
              tags$li(strong("Tu dato (178): "), "cae dentro de esa zona roja ",
                     "\u2014 es decir, es lo bastante alejado de 175 como para ",
                     "dudar de que H\u2080 sea cierta. Por eso se rechaza H\u2080.")
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
          conector_infografia("Primero, la definición formal correcta"),

          tarjeta_concepto("check-circle", colores$primario,
            "¿Qué ES un valor de p, exactamente?",
            div(style = paste0("background:", colores$fondo,
                              "; border-radius:8px; padding:14px;",
                              "font-size:1.02rem; font-weight:600; color:",
                              colores$primario, ";"),
                "p = Probabilidad de obtener un resultado tan extremo (o ",
                "m\u00e1s) que el observado, ", strong("asumiendo que H\u2080 es ",
                "verdadera"), "."
            ),
            p(class = "mt-2 mb-0 small text-muted",
              "Es una probabilidad condicional: P(datos as\u00ed de extremos ",
              "o m\u00e1s | H\u2080 verdadera) \u2014 NO P(H\u2080 verdadera | datos), que ",
              "es lo que la mayor\u00eda de la gente cree que est\u00e1 calculando."
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
            "As\u00ed se ve en la pr\u00e1ctica: en ecolog\u00eda casi nunca comparamos ",
            "una muestra contra un valor fijo \u2014 lo m\u00e1s com\u00fan es comparar ",
            strong("dos grupos"), " (machos vs. hembras, sitio A vs. sitio B). ",
            "H\u2080 dice que ambos grupos vienen de la misma poblaci\u00f3n (sin ",
            "diferencia real). Mueve las medias, las DE y los tama\u00f1os de ",
            "muestra de cada grupo y observa cu\u00e1ndo la prueba rechaza H\u2080."),
          layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)), fill = FALSE,
            card(card_header(bs_icon("sliders", class="me-1"), "Par\u00e1metros"),
              card_body(
                h6(class = "text-muted mb-1", "Grupo 1 (ej.: machos)"),
                sliderInput(ns("media_g1_sim_ph"), "Media grupo 1:",
                            min=150, max=200, value=175, step=1),
                sliderInput(ns("sd_g1_sim_ph"), "DE grupo 1:",
                            min=2, max=30, value=15, step=1),
                sliderInput(ns("n1_sim_ph"), "n grupo 1:",
                            min=5, max=200, value=30, step=5),
                tags$hr(),
                h6(class = "text-muted mb-1", "Grupo 2 (ej.: hembras)"),
                sliderInput(ns("media_g2_sim_ph"), "Media grupo 2:",
                            min=150, max=200, value=175, step=1),
                sliderInput(ns("sd_g2_sim_ph"), "DE grupo 2:",
                            min=2, max=30, value=15, step=1),
                sliderInput(ns("n2_sim_ph"), "n grupo 2:",
                            min=5, max=200, value=30, step=5),
                tags$hr(),
                sliderInput(ns("alpha_sim_ph"), "Nivel de significancia (\u03b1):",
                            min=0.01, max=0.20, value=0.05, step=0.01)
              )),
            div(plotOutput(ns("plot_sim_ph"), height="380px"),
                uiOutput(ns("insight_sim_ph")),
                tags$hr(), uiOutput(ns("cards_sim_ph")))
          )
        )
      ),

      nav_panel(title = tagList(bs_icon("clipboard-data", class="me-1"), "Practica con datos reales"),
        card_body(
          p(class="small text-muted mb-3",
            "Compara una variable num\u00e9rica entre dos grupos de un dataset."),
          layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)), fill = FALSE,
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
                             class="btn-primary w-100 btn-sm", icon=icon("play"))
              )),
            div(plotOutput(ns("plot_practica_ph"), height="320px"),
                uiOutput(ns("insight_practica_ph")),
                tags$hr(), uiOutput(ns("cards_practica_ph")))
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

    # ── ¿Qué es? — diagrama simple de introducción ────
    output$plot_intro_ph <- renderPlot({
      mu   <- 175        # H0: media poblacional
      xbar <- 178        # dato muestral observado
      se   <- 1          # error est\u00e1ndar (ilustrativo, ajustado para que
                          # 178 caiga claramente en la zona de rechazo)
      xs   <- seq(mu - 4*se, mu + 4*se, length.out = 500)
      df   <- data.frame(x = xs, y = stats::dnorm(xs, mu, se))
      zc   <- mu + stats::qnorm(0.95) * se  # cola derecha, \u03b1=0.05 un lado

      ggplot(df, aes(x = x, y = y)) +
        geom_area(data = subset(df, x >= zc), aes(x = x, y = y),
                 fill = colores$peligro, alpha = 0.6) +
        geom_line(color = colores$primario, linewidth = 1) +
        geom_vline(xintercept = mu, linetype = "solid", color = colores$primario) +
        geom_vline(xintercept = xbar, linetype = "dashed", color = colores$acento,
                  linewidth = 1) +
        annotate("text", x = mu, y = max(df$y) * 1.05, label = "H0: \u03bc = 175",
                 color = colores$primario, fontface = "bold", size = 4) +
        annotate("text", x = xbar, y = max(df$y) * 0.55, label = "dato\nmuestral = 178",
                 color = colores$acento, fontface = "bold", size = 3.3) +
        labs(x = "Valor", y = "Densidad",
             title = "Distribuci\u00f3n bajo H0 \u2014 \u00bfqu\u00e9 tan raro es mi dato?") +
        theme_light(base_size = 16) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 16))
    })

    sim_ph <- reactive({
      m1    <- input$media_g1_sim_ph
      m2    <- input$media_g2_sim_ph
      s1    <- input$sd_g1_sim_ph
      s2    <- input$sd_g2_sim_ph
      n1    <- input$n1_sim_ph
      n2    <- input$n2_sim_ph
      alpha <- input$alpha_sim_ph
      req(m1, m2, s1, s2, n1, n2, alpha)

      se1 <- s1^2 / n1
      se2 <- s2^2 / n2
      se_dif <- sqrt(se1 + se2)

      # grados de libertad de Welch-Satterthwaite
      gl <- (se1 + se2)^2 / (se1^2/(n1-1) + se2^2/(n2-1))

      t_obs   <- (m2 - m1) / se_dif
      p_valor <- 2 * stats::pt(-abs(t_obs), df = gl)
      tc      <- stats::qt(1 - alpha/2, df = gl)
      rechaza <- p_valor < alpha

      list(m1 = m1, m2 = m2, s1 = s1, s2 = s2, n1 = n1, n2 = n2,
           alpha = alpha, se_dif = se_dif, gl = gl, t_obs = t_obs,
           p_valor = p_valor, tc = tc, rechaza = rechaza)
    })

    output$cards_sim_ph <- renderUI({
      s <- sim_ph()
      tagList(
        tarjeta_metrica("Diferencia (x\u0304\u2082\u2212x\u0304\u2081)",
                        round(s$m2 - s$m1, 2), "media"),
        tarjeta_metrica("Estad\u00edstico (t de Welch)", round(s$t_obs, 2), "media"),
        tarjeta_metrica("Valor p", format.pval(s$p_valor, digits = 3), "media"),
        tarjeta_metrica("Decisi\u00f3n",
                        if (s$rechaza) "Rechaza H\u2080" else "No rechaza H\u2080",
                        "media", ultima = TRUE)
      )
    })

    output$plot_sim_ph <- renderPlot({
      s <- sim_ph()
      color_dato <- if (s$rechaza) colores$peligro else colores$secundario

      xmin <- min(s$m1 - 4*s$s1, s$m2 - 4*s$s2)
      xmax <- max(s$m1 + 4*s$s1, s$m2 + 4*s$s2)
      xs   <- seq(xmin, xmax, length.out = 500)
      df   <- data.frame(
        x     = rep(xs, 2),
        y     = c(stats::dnorm(xs, s$m1, s$s1), stats::dnorm(xs, s$m2, s$s2)),
        grupo = rep(c("Grupo 1", "Grupo 2"), each = length(xs))
      )

      ggplot(df, aes(x = x, y = y, color = grupo, fill = grupo)) +
        geom_area(alpha = 0.35, position = "identity") +
        geom_line(linewidth = 1) +
        geom_vline(xintercept = s$m1, color = colores$primario,
                  linewidth = 0.8) +
        geom_vline(xintercept = s$m2, color = colores$acento,
                  linetype = "dashed", linewidth = 1.1) +
        scale_color_manual(values = c("Grupo 1" = colores$primario,
                                      "Grupo 2" = colores$acento)) +
        scale_fill_manual(values = c("Grupo 1" = colores$primario,
                                     "Grupo 2" = colores$acento)) +
        labs(x = "Valor", y = "Densidad", color = NULL, fill = NULL,
             title = if (s$rechaza) "La diferencia es estad\u00edsticamente significativa"
                     else "La diferencia NO es estad\u00edsticamente significativa") +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = color_dato, face = "bold", size = 17),
              legend.position = "top")
    })

    output$insight_sim_ph <- renderUI({
      s <- sim_ph()
      div(class = "alert alert-info small mt-2", bs_icon("lightbulb-fill", class="me-1"),
          paste0("H\u2080 dice que ambos grupos tienen la misma media. La ",
                "diferencia observada (", round(s$m2 - s$m1, 2), ") equivale a ",
                round(abs(s$t_obs), 2), " errores est\u00e1ndar de la diferencia ",
                "(t de Welch, gl \u2248 ", round(s$gl, 1), "). Con \u03b1 = ",
                s$alpha, ", el valor p (", format.pval(s$p_valor, digits = 3),
                ") ", if (s$rechaza) "cae por debajo \u2014 se rechaza H\u2080."
                else "no cae por debajo \u2014 no se rechaza H\u2080.", " Prob\u00e1 subir ",
                "n1/n2 con la misma diferencia peque\u00f1a: notar\u00e1s que muestras ",
                "m\u00e1s grandes detectan diferencias m\u00e1s sutiles."))
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
      alpha <- input$alpha_practica_ph

      resumen <- do.call(rbind, lapply(split(d, d$grupo), function(g) {
        n    <- nrow(g)
        m    <- mean(g$y)
        se   <- stats::sd(g$y) / sqrt(n)
        tc   <- stats::qt(1 - alpha/2, df = n - 1)
        data.frame(grupo = g$grupo[1], media = m, se = se,
                  li = m - tc * se, ls = m + tc * se, n = n)
      }))

      list(test = test, d = d, alpha = alpha, resumen = resumen)
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
      ggplot(r$resumen, aes(x = grupo, y = media, color = grupo)) +
        geom_errorbar(aes(ymin = li, ymax = ls), width = 0.15, linewidth = 1) +
        geom_point(size = 4) +
        scale_color_manual(values = c(colores$primario, colores$acento)) +
        labs(x = NULL, y = paste0("Media de ", input$var_num_ph),
             title = paste0("t = ", round(r$test$statistic, 2), ", p = ",
                           format.pval(r$test$p.value, digits = 3)),
             subtitle = paste0("Puntos = media, barras = IC ",
                               round((1 - r$alpha) * 100), "%")) +
        theme_light(base_size = 17) +
        theme(legend.position = "none",
              plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 17),
              plot.subtitle = element_text(color = "grey40", size = 10.5))
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
