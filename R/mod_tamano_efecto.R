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

      nav_panel(title = tagList(bs_icon("sliders", class="me-1"), "Simulaci\u00f3n interactiva"),
        card_body(
          p(class="small text-muted mb-3",
            "Fija una diferencia real entre dos grupos y despu\u00e9s sub\u00ed el ",
            "tama\u00f1o de muestra. Vas a ver que el ", strong("Cohen's d NO ",
            "cambia"), " (mide la magnitud real, que no toc\u00e1s), pero el ",
            strong("valor p S\u00cd cambia"), " \u2014 una muestra m\u00e1s grande ",
            "detecta la misma diferencia con m\u00e1s confianza."),
          layout_columns(col_widths = c(4,8), fill = FALSE,
            card(card_header(bs_icon("sliders", class="me-1"), "Par\u00e1metros"),
              card_body(
                h6(class = "text-muted mb-1", "Grupo 1 (ej.: machos)"),
                sliderInput(ns("media_g1_sim_te"), "Media grupo 1:",
                            min=150, max=200, value=175, step=1),
                sliderInput(ns("sd_g1_sim_te"), "DE grupo 1:",
                            min=2, max=30, value=15, step=1),
                sliderInput(ns("n1_sim_te"), "n grupo 1:",
                            min=5, max=200, value=30, step=5),
                tags$hr(),
                h6(class = "text-muted mb-1", "Grupo 2 (ej.: hembras)"),
                sliderInput(ns("media_g2_sim_te"), "Media grupo 2:",
                            min=150, max=200, value=180, step=1),
                sliderInput(ns("sd_g2_sim_te"), "DE grupo 2:",
                            min=2, max=30, value=15, step=1),
                sliderInput(ns("n2_sim_te"), "n grupo 2:",
                            min=5, max=200, value=30, step=5),
                tags$hr(), uiOutput(ns("cards_sim_te"))
              )),
            div(plotOutput(ns("plot_sim_te"), height="380px"),
                tags$hr(class = "mt-3 mb-2"),
                h6(bs_icon("rulers", class = "me-1"),
                   "Gr\u00e1fico de estimaci\u00f3n (Gardner-Altman)",
                   style = paste0("color:", colores$primario, "; font-weight:600;")),
                plotOutput(ns("plot_efecto_sim_te"), height="150px"),
                uiOutput(ns("insight_sim_te")))
          )
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
                tags$hr(class = "mt-3 mb-2"),
                h6(bs_icon("rulers", class = "me-1"),
                   "Gr\u00e1fico de estimaci\u00f3n (Gardner-Altman)",
                   style = paste0("color:", colores$primario, "; font-weight:600;")),
                plotOutput(ns("plot_efecto_practica_te"), height="150px"),
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

    sim_te <- reactive({
      m1 <- input$media_g1_sim_te
      m2 <- input$media_g2_sim_te
      s1 <- input$sd_g1_sim_te
      s2 <- input$sd_g2_sim_te
      n1 <- input$n1_sim_te
      n2 <- input$n2_sim_te
      req(m1, m2, s1, s2, n1, n2)

      dif <- m2 - m1
      sd_combinada <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
      cohen_d <- dif / sd_combinada

      se1 <- s1^2 / n1
      se2 <- s2^2 / n2
      se_dif <- sqrt(se1 + se2)
      gl <- (se1 + se2)^2 / (se1^2/(n1-1) + se2^2/(n2-1))
      t_obs <- dif / se_dif
      p_valor <- 2 * stats::pt(-abs(t_obs), df = gl)
      tc_dif <- stats::qt(0.975, df = gl)
      dif_li <- dif - tc_dif * se_dif
      dif_ls <- dif + tc_dif * se_dif

      # IC de Cohen's d: aproximaci\u00f3n de Hedges & Olkin (1985)
      se_d <- sqrt((n1 + n2) / (n1 * n2) + cohen_d^2 / (2 * (n1 + n2)))
      d_li <- cohen_d - 1.96 * se_d
      d_ls <- cohen_d + 1.96 * se_d

      list(m1 = m1, m2 = m2, s1 = s1, s2 = s2, n1 = n1, n2 = n2,
           dif = dif, dif_li = dif_li, dif_ls = dif_ls,
           cohen_d = cohen_d, d_li = d_li, d_ls = d_ls,
           t_obs = t_obs, p_valor = p_valor)
    })

    output$cards_sim_te <- renderUI({
      s <- sim_te()
      tagList(
        tarjeta_metrica("Diferencia (no estandarizada)",
                        paste0(round(s$dif, 2), "  [IC 95%: ", round(s$dif_li, 2),
                              " a ", round(s$dif_ls, 2), "]"),
                        "diferencia_efecto"),
        tarjeta_metrica("Cohen's d (estandarizado)",
                        paste0(round(s$cohen_d, 2), " (", interpretar_cohen_d(s$cohen_d),
                              ")  [IC 95%: ", round(s$d_li, 2), " a ", round(s$d_ls, 2), "]"),
                        "cohen_d"),
        tarjeta_metrica("Valor p (referencia)",
                        format.pval(s$p_valor, digits = 3), "valor_p_ref", ultima = TRUE)
      )
    })

    output$plot_sim_te <- renderPlot({
      s <- sim_te()
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
        geom_vline(xintercept = s$m1, color = colores$primario, linewidth = 0.8) +
        geom_vline(xintercept = s$m2, color = colores$acento,
                  linetype = "dashed", linewidth = 1.1) +
        scale_color_manual(values = c("Grupo 1" = colores$primario,
                                      "Grupo 2" = colores$acento)) +
        scale_fill_manual(values = c("Grupo 1" = colores$primario,
                                     "Grupo 2" = colores$acento)) +
        labs(x = "Valor", y = "Densidad", color = NULL, fill = NULL,
             title = paste0("Cohen's d = ", round(s$cohen_d, 2), "  |  p = ",
                           format.pval(s$p_valor, digits = 3))) +
        theme_light(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 13),
              legend.position = "top")
    })

    output$plot_efecto_sim_te <- renderPlot({
      s <- sim_te()
      color_ic <- if (s$dif_li > 0 || s$dif_ls < 0) colores$primario else "grey50"
      lim <- max(abs(s$dif_li), abs(s$dif_ls), abs(s$dif), 1) * 1.3

      ggplot(data.frame(x = s$dif, y = 0.5), aes(x = x, y = y)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey40",
                  linewidth = 0.7) +
        geom_errorbarh(aes(xmin = s$dif_li, xmax = s$dif_ls), height = 0.15,
                      color = color_ic, linewidth = 1.1) +
        geom_point(size = 4, color = color_ic) +
        scale_x_continuous(limits = c(-lim, lim)) +
        scale_y_continuous(limits = c(0, 1), breaks = NULL) +
        labs(x = "Diferencia (no estandarizada)", y = NULL,
             title = "Diferencia e IC 95%") +
        theme_light(base_size = 12) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 11),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank())
    })

    output$insight_sim_te <- renderUI({
      s <- sim_te()
      div(class = "alert alert-info small mt-2", bs_icon("lightbulb-fill", class="me-1"),
          paste0("La diferencia real (", round(s$dif, 2), ", IC 95% [", round(s$dif_li, 2),
                ", ", round(s$dif_ls, 2), "]) es lo que le importa a alguien tomando una ",
                "decisi\u00f3n pr\u00e1ctica. Cohen's d (", round(s$cohen_d, 2), ", IC 95% [",
                round(s$d_li, 2), ", ", round(s$d_ls, 2), "]) no depende de n1 ni n2 \u2014 ",
                "solo de la diferencia y la variabilidad \u2014 pero su IC s\u00ed se angosta con ",
                "n, igual que el de la diferencia. Prob\u00e1 subir n1 y n2 sin tocar las medias ",
                "ni las DE: Cohen's d se queda igual, pero ambos IC se angostan y el valor p baja."))
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

      test <- stats::t.test(y ~ grupo, data = d)
      dif_ic <- rev(-test$conf.int)  # conf.int viene como grupo1 - grupo2; invertimos

      se_d <- sqrt((n1 + n2) / (n1 * n2) + cohen_d^2 / (2 * (n1 + n2)))
      d_li <- cohen_d - 1.96 * se_d
      d_ls <- cohen_d + 1.96 * se_d

      resumen <- do.call(rbind, lapply(split(d, d$grupo), function(gr) {
        n  <- nrow(gr)
        m  <- mean(gr$y)
        se <- stats::sd(gr$y) / sqrt(n)
        tc <- stats::qt(0.975, df = n - 1)
        data.frame(grupo = gr$grupo[1], media = m, se = se,
                  li = m - tc * se, ls = m + tc * se, n = n)
      }))

      list(d = d, resumen = resumen, m1 = m1, m2 = m2, dif = dif_no_estandarizada,
           dif_li = dif_ic[1], dif_ls = dif_ic[2],
           cohen_d = cohen_d, d_li = d_li, d_ls = d_ls,
           niveles = levels(d$grupo))
    })

    output$cards_practica_te <- renderUI({
      req(resultado_te())
      r <- resultado_te()
      tagList(
        tarjeta_metrica(paste0("Media ", r$niveles[1]), round(r$m1, 2), "media"),
        tarjeta_metrica(paste0("Media ", r$niveles[2]), round(r$m2, 2), "media"),
        tarjeta_metrica("Diferencia (no estandarizada)",
                        paste0(round(r$dif, 2), " ", input$var_num_te,
                              "  [IC 95%: ", round(r$dif_li, 2), " a ",
                              round(r$dif_ls, 2), "]"),
                        "diferencia_efecto"),
        tarjeta_metrica("Cohen's d (estandarizado)",
                        paste0(round(r$cohen_d, 2), " (",
                              interpretar_cohen_d(r$cohen_d),
                              ")  [IC 95%: ", round(r$d_li, 2), " a ",
                              round(r$d_ls, 2), "]"),
                        "cohen_d", ultima = TRUE)
      )
    })

    output$plot_practica_te <- renderPlot({
      req(resultado_te())
      r <- resultado_te()
      ggplot(r$resumen, aes(x = grupo, y = media, color = grupo)) +
        geom_errorbar(aes(ymin = li, ymax = ls), width = 0.15, linewidth = 1) +
        geom_point(size = 4) +
        scale_color_manual(values = c(colores$primario, colores$acento)) +
        labs(x = NULL, y = paste0("Media de ", input$var_num_te),
             title = paste0("Diferencia = ", round(r$dif, 2), " | Cohen's d = ",
                           round(r$cohen_d, 2)),
             subtitle = "Puntos = media, barras = IC 95%") +
        theme_light(base_size = 13) +
        theme(legend.position = "none",
              plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 13),
              plot.subtitle = element_text(color = "grey40", size = 10.5))
    })

    output$plot_efecto_practica_te <- renderPlot({
      req(resultado_te())
      r <- resultado_te()
      color_ic <- if (r$dif_li > 0 || r$dif_ls < 0) colores$primario else "grey50"
      lim <- max(abs(r$dif_li), abs(r$dif_ls), abs(r$dif), 1) * 1.3

      ggplot(data.frame(x = r$dif, y = 0.5), aes(x = x, y = y)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey40",
                  linewidth = 0.7) +
        geom_errorbarh(aes(xmin = r$dif_li, xmax = r$dif_ls), height = 0.15,
                      color = color_ic, linewidth = 1.1) +
        geom_point(size = 4, color = color_ic) +
        scale_x_continuous(limits = c(-lim, lim)) +
        scale_y_continuous(limits = c(0, 1), breaks = NULL) +
        labs(x = paste0("Diferencia (", input$var_num_te, ")"), y = NULL,
             title = "Diferencia e IC 95%") +
        theme_light(base_size = 12) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario, face = "bold", size = 11),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank())
    })

    output$insight_practica_te <- renderUI({
      req(resultado_te())
      r <- resultado_te()
      div(class = "alert alert-info small mt-2", bs_icon("lightbulb-fill", class="me-1"),
          paste0("La diferencia real es ", round(r$dif,2), " ", input$var_num_te,
                " (IC 95%: ", round(r$dif_li,2), " a ", round(r$dif_ls,2),
                ") entre '", r$niveles[2], "' y '", r$niveles[1], "' \u2014 eso es lo ",
                "que le importa a alguien tomando una decisi\u00f3n pr\u00e1ctica. El Cohen's d (",
                round(r$cohen_d,2), ", IC 95%: ", round(r$d_li,2), " a ", round(r$d_ls,2),
                ") dice que esa diferencia es de tama\u00f1o ",
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
