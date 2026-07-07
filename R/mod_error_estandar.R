# ============================================================
# mod_error_estandar.R — Error estándar del estadístico
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Pestañas:
#   1. ¿Qué es?
#   2. Simulación interactiva (muestreo repetido desde una
#      población teórica — demuestra el Teorema Central del
#      Límite y cómo el EE decrece con \u221an)
#   3. Los datos (Datos de ejemplo + Mis datos + Tipos de variables)
#   4. Practica con datos reales (bootstrap vs. fórmula clásica)
#   5. Código R
#
# Nota pedagógica: la confusión más común en este tema es
# DE vs. EE. La pestaña "¿Qué es?" los contrasta explícitamente
# en una tabla, y "Simulación interactiva" deja ver en vivo que
# el EE encoge al aumentar n, mientras la DE de la población no
# cambia.
# ============================================================

# ── UI ────────────────────────────────────────────────────
mod_error_estandar_ui <- function(id) {
  ns <- NS(id)

  tagList(

    div(
      class = "py-3 px-2",
      h4(
        bs_icon("rulers", class = "me-2"),
        "Error est\u00e1ndar del estad\u00edstico",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "En la estad\u00edstica inferencial, lo m\u00e1s com\u00fan es que quieras ",
        "conocer un ", strong("par\u00e1metro poblacional"), " (como una media) ",
        "cuyo valor verdadero ", strong("nunca vamos a conocer con ",
        "certeza"), " \u2014 por eso lo estimamos a partir de una muestra. ",
        "Tomas esa muestra, calculas un estimado \u2014 pero con una sola ",
        "muestra surge una pregunta inevitable: ", strong(
        "\u00bfqu\u00e9 tan cerca est\u00e1 ese estimado del verdadero valor ",
        "poblacional?"), " El error est\u00e1ndar (EE) es la respuesta a esa ",
        "pregunta: mide ", strong("qu\u00e9 tan preciso"), " es tu estimado."
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
             "\u00bfPara qu\u00e9 sirve el error est\u00e1ndar?"),

          paso_infografia(
            1, "question-circle", colores$secundario,
            "El problema: no sabes qu\u00e9 tan preciso es tu estimado",
            p(class = "mb-3",
              "Tomas una muestra (no puedes medir a toda la poblaci\u00f3n) y ",
              "calculas un estimado \u2014 por ejemplo, una media. Pero con ",
              "esa \u00fanica muestra, no tienes forma de saber si tu estimado ",
              "qued\u00f3 cerca o lejos del verdadero valor poblacional. Este ",
              "problema aparece en cualquier campo:"
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              card(
                style = paste0("border-left:4px solid ", colores$secundario, ";"),
                card_body(
                  h6(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                     bs_icon("tree", class = "me-1"), "Ecolog\u00eda"),
                  p(class = "small mb-0",
                    "Una cueva con ", strong("miles de vampiros"), " (murci\u00e9lago ",
                    em("Desmodus rotundus"), "). Estimas su peso promedio con ",
                    "una muestra \u2014 \u00bfqu\u00e9 tan confiable es ese n\u00famero?"
                  )
                )
              ),
              card(
                style = paste0("border-left:4px solid ", colores$secundario, ";"),
                card_body(
                  h6(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                     bs_icon("people", class = "me-1"), "Ciencias sociales"),
                  p(class = "small mb-0",
                    "Miles de ", strong("estudiantes universitarios"), " en ",
                    "Costa Rica. Estimas su satisfacci\u00f3n promedio con una ",
                    "encuesta \u2014 \u00bfqu\u00e9 tan confiable es ese n\u00famero?"
                  )
                )
              ),
              card(
                style = paste0("border-left:4px solid ", colores$secundario, ";"),
                card_body(
                  h6(style = paste0("color:", colores$secundario, "; font-weight:700;"),
                     bs_icon("heart-pulse", class = "me-1"), "Salud p\u00fablica"),
                  p(class = "small mb-0",
                    "Miles de ", strong("pacientes"), " con una enfermedad. ",
                    "Estimas su tiempo de recuperaci\u00f3n promedio con una ",
                    "muestra \u2014 \u00bfqu\u00e9 tan confiable es ese n\u00famero?"
                  )
                )
              )
            ),
            p(class = "mb-2 mt-1",
              "En los tres casos, tomas una muestra y calculas un estimado ",
              "\u2014 pero con una sola muestra en mano, no sabes si te ",
              "qued\u00f3 cerca o lejos del verdadero valor. ", strong(
              "El error est\u00e1ndar mide exactamente esa precisi\u00f3n."),
              " Para verlo, imaginemos (en teor\u00eda) que s\u00ed pudieras ",
              "repetir el muestreo miles de veces: ", em("\u00bfqu\u00e9 pasar\u00eda ",
              "con tus estimados?")
            ),
            div(class = "alert alert-info small mb-0",
                bs_icon("lightbulb-fill", class = "me-1"),
                "Cada muestra te dar\u00eda una media distinta. El promedio de ",
                em("todas esas medias"), " es la ", strong("\"gran media\""),
                " (coincide con \u03bc). Y qu\u00e9 tanto se dispersan esas medias ",
                "alrededor de la gran media es exactamente el ",
                strong("error est\u00e1ndar"), ". Pru\u00e9balo t\u00fa mismo en ",
                strong("Simulaci\u00f3n interactiva"), " \u2014 ah\u00ed s\u00ed puedes ",
                "repetir el muestreo miles de veces con un clic."
            )
          ),

          p(class = "text-muted mt-3 mb-2",
            "As\u00ed se ve esa idea de ", strong("precisi\u00f3n"), " en un ",
            "gr\u00e1fico:"
          ),
          fluidRow(
            column(6, plotOutput(ns("plot_de_vs_ee_ind"), height = "280px")),
            column(6, plotOutput(ns("plot_de_vs_ee_medias"), height = "280px"))
          ),
          div(class = "alert alert-secondary small mt-2 mb-3",
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "A la izquierda: pesos individuales de vampiros \u2014 var\u00edan ",
              "bastante entre un individuo y otro. A la derecha: la ",
              em("distribuci\u00f3n"), " de 300 medias muestrales distintas ",
              "(de 10 vampiros cada una) \u2014 mucho m\u00e1s angosta. Tu ",
              "estimado real (la media de la \u00fanica muestra que t\u00fa ",
              "tomar\u00edas) es solo ", strong("un punto"), " de esa ",
              "distribuci\u00f3n de la derecha \u2014 no tiene una \"precisi\u00f3n\" ",
              "propia, pero como la mayor\u00eda de esa distribuci\u00f3n cae ",
              "cerca de \u03bc, es probable que tu estimado tambi\u00e9n lo est\u00e9. ",
              "Esa dispersi\u00f3n de la derecha \u2014qu\u00e9 tan cerca caen la ",
              "mayor\u00eda de las medias posibles\u2014 es exactamente lo que ",
              "mide el error est\u00e1ndar."
          ),

          conector_infografia("Para ver exactamente c\u00f3mo funciona, usemos un caso especial donde S\u00cd podemos medir a todos"),

          fluidRow(
            column(
              width = 6,
              paso_infografia(
                2, "collection", colores$primario,
                "Un caso especial: poblaci\u00f3n peque\u00f1a y completa",
                p(class = "text-muted mb-2",
                  "En el Parque Nacional Tapant\u00ed quedan, hipot\u00e9ticamente, ",
                  "solo ", strong("5 dantas adultas"), " (", em("Tapirus bairdii"),
                  "). Como son tan pocas, ", strong("s\u00ed puedes pesarlas a todas"),
                  ":"
                ),
                uiOutput(ns("poblacion_5_ee"))
              )
            ),
            column(
              width = 6,
              paso_infografia(
                3, "shuffle", colores$acento,
                "Tomas una muestra",
                p(class = "text-muted mb-2",
                  "Por ejemplo, eliges 2 dantas al azar:"
                ),
                uiOutput(ns("muestra_ejemplo_ee")),
                p(class = "text-muted mt-3 mb-0",
                  "Como la poblaci\u00f3n es tan peque\u00f1a, podemos listar ",
                  strong("las 10 muestras posibles"), " de tama\u00f1o 2 y ver qu\u00e9 ",
                  "pasa con cada una."
                )
              )
            )
          ),

          conector_infografia("\u00bfY si repites el muestreo una y otra vez?"),

          paso_infografia(
            4, "table", colores$primario,
            "Todas las muestras posibles",
            uiOutput(ns("tabla_enumeracion_ee"))
          ),

          uiOutput(ns("resumen_tabla_enumeracion_ee")),

          h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
             "DE vs. EE \u2014 la confusi\u00f3n m\u00e1s com\u00fan"),
          p(class = "text-muted mb-2",
            "Suenan parecido, pero miden cosas distintas (ya viste la ",
            "diferencia en el gr\u00e1fico de arriba). En resumen:"
          ),

          fluidRow(
            column(6,
              tarjeta_concepto(
                "bar-chart-line", colores$primario,
                "Desviaci\u00f3n Est\u00e1ndar (DE)",
                tags$ul(class = "mb-0 ps-3",
                  tags$li(strong("Mide: "), "qu\u00e9 tan dispersos est\u00e1n los ",
                         "datos individuales"),
                  tags$li(strong("F\u00f3rmula: "),
                         "s = \u221a\u03a3(x\u2212\u0078\u0304)\u00b2/(n\u22121)"),
                  tags$li(strong("\u00bfCambia con n? "), "No \u2014 es propiedad ",
                         "de la poblaci\u00f3n"),
                  tags$li(strong("Se usa para: "), "describir la variabilidad ",
                         "de los datos crudos")
                )
              )
            ),
            column(6,
              tarjeta_concepto(
                "rulers", colores$acento,
                "Error Est\u00e1ndar (EE)",
                tags$ul(class = "mb-0 ps-3",
                  tags$li(strong("Mide: "), "qu\u00e9 tan dispersa estar\u00eda la ",
                         "media muestral entre repeticiones"),
                  tags$li(strong("F\u00f3rmula: "), "EE = s / \u221an"),
                  tags$li(strong("\u00bfCambia con n? "), "S\u00ed \u2014 disminuye ",
                         "al aumentar n"),
                  tags$li(strong("Se usa para: "), "intervalos de confianza y ",
                         "pruebas de hip\u00f3tesis")
                )
              )
            )
          ),

          div(
            style = paste0("background:", colores$fondo,
                          "; border-left:6px solid ", colores$primario,
                          "; border-radius:10px; padding:14px 20px;",
                          "margin: 16px 0;"),
            bs_icon("blockquote-left", style = paste0("color:", colores$primario,
                                           "; font-size:1.3rem;")),
            p(class = "mb-0 mt-1", style = "font-size:1.05rem; font-weight:600;",
              "La DE describe tus datos; el EE describe qu\u00e9 tan bien un ",
              em("estad\u00edstico muestral"), " estima el verdadero valor ",
              "poblacional."
            )
          ),

          tarjeta_concepto(
            "diagram-3", colores$secundario,
            "El EE no es solo para la media",
            p(class = "mb-0",
              "Es un concepto general que aplica a ", em("cualquier"),
              " estad\u00edstico calculado de una muestra \u2014 una ",
              "proporci\u00f3n, un coeficiente de regresi\u00f3n, o (en ",
              "ecolog\u00eda) una estimaci\u00f3n de abundancia poblacional N ",
              "o de probabilidad de detecci\u00f3n p. Lo que cambia es ",
              em("c\u00f3mo"), " se calcula \u2014 EE = s/\u221an es espec\u00edfica ",
              "de la media. Este m\u00f3dulo usa la media porque es el ejemplo ",
              "m\u00e1s simple, pero el mismo principio aplica en ",
              "StatModels/StatBayes/StatOccu/StatAbundance para sus ",
              "respectivos par\u00e1metros."
            )
          ),

          h5(style = paste0("color:", colores$primario,
                          "; font-weight:700; margin-top:20px;"),
             "El Teorema Central del L\u00edmite (TCL) y otra propiedad relacionada"),
          p(class = "text-muted mb-2",
            "Si tomas muchas muestras del mismo tama\u00f1o n y calculas la ",
            "media de cada una, esas medias forman su propia distribuci\u00f3n ",
            "\u2014 la ", strong("distribuci\u00f3n muestral de la media"), ". Dos ",
            "cosas ciertas sobre ella \u2014 pero ", strong("no vienen del mismo ",
            "lugar"), ":"
          ),
          fluidRow(
            column(6,
              tarjeta_concepto(
                "bell", colores$primario, "Se vuelve normal (esto S\u00cd es el TCL)",
                p(class = "mb-0",
                  "Sin importar la forma de la poblaci\u00f3n original (incluso ",
                  "sesgada), la distribuci\u00f3n de las medias muestrales se ",
                  "aproxima a una normal cuando n es suficientemente ",
                  "grande. Esto es lo que el Teorema Central del L\u00edmite ",
                  "realmente afirma \u2014 y s\u00ed necesita n razonablemente ",
                  "grande para cumplirse bien."
                )
              )
            ),
            column(6,
              tarjeta_concepto(
                "arrow-down-short", colores$acento,
                "Se encoge (esto NO es el TCL)",
                p(class = "mb-0",
                  "Su dispersi\u00f3n (el EE) disminuye proporcionalmente a ",
                  "1/\u221an \u2014 cuadruplicar n solo reduce el EE a la ",
                  "mitad. Esto es simple \u00e1lgebra de varianzas (Var de un ",
                  "promedio = \u03c3\u00b2/n): es cierto para ", em("cualquier"),
                  " n, incluso n peque\u00f1o \u2014 no depende del TCL ni ",
                  "necesita que la distribuci\u00f3n ya sea normal. Vi\u00e9ndolo ",
                  "en un gr\u00e1fico:"
                ),
                plotOutput(ns("plot_de_vs_n"), height = "220px")
              )
            )
          ),

          tarjeta_concepto(
            "exclamation-triangle", colores$peligro,
            "Cuidado \u2014 el TCL es sobre medias",
            p(class = "mb-0",
              "Una ", em("tasa de supervivencia"), " o cualquier proporci\u00f3n ",
              "S\u00cd est\u00e1 cubierta por el TCL, porque en el fondo es una ",
              "media disfrazada (codifica cada individuo como 1 = sobrevivi\u00f3 ",
              "/ 0 = muri\u00f3). Pero par\u00e1metros como el ",
              strong("tama\u00f1o poblacional (N)"), " o la ", strong(
              "probabilidad de detecci\u00f3n (p)"), " en modelos de ",
              "ocupaci\u00f3n ", em("no"), " son medias \u2014 su ",
              "normalidad aproximada viene de un teorema hermano: la ",
              strong("teor\u00eda asint\u00f3tica de m\u00e1xima verosimilitud"),
              ". Por eso StatOccu/StatAbundance usan intervalos ",
              em("log-normales"), " para N en vez de normales."
            )
          ),

          p(class = "text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en ", strong("Simulaci\u00f3n interactiva"),
            " \u2014 mueve el tama\u00f1o de muestra y observa c\u00f3mo cambia ",
            "la forma y el ancho de la distribuci\u00f3n de medias."
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
            "Como en el ejemplo de la Isla Sola: se genera una poblaci\u00f3n ",
            "finita conocida de tama\u00f1o N. Cada vez que mueves el ",
            "tama\u00f1o de muestra, se toman muchas muestras ", em("sin ",
            "reemplazo"), " de ese tama\u00f1o, se calcula la media de cada ",
            "una, y se grafica c\u00f3mo se distribuyen esas medias."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                radioButtons(
                  ns("forma_pob_sim_ee"), "Forma de la poblaci\u00f3n:",
                  choices  = c("Normal", "Uniforme", "Exponencial (sesgada)" = "Exponencial"),
                  selected = "Normal"
                ),
                sliderInput(ns("N_sim_ee"), "Tama\u00f1o de la poblaci\u00f3n (N):",
                            min = 20, max = 1000, value = 200, step = 10),
                uiOutput(ns("slider_n_muestra_sim_ee")),
                sliderInput(ns("reps_sim_ee"),
                            "N\u00famero de muestras simuladas:",
                            min = 100, max = 5000, value = 1000, step = 100),
                actionButton(ns("regenerar_sim_ee"), "Nueva poblaci\u00f3n",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle")),
                tags$hr(),
                uiOutput(ns("cards_sim_ee"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_ee"), height = "380px"),
              uiOutput(ns("insight_sim_ee"))
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
                    ns("fuente_datos_ee"),
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
                  uiOutput(ns("info_dataset_ee"))
                ),
                div(
                  uiOutput(ns("cards_datos_ee")),
                  DTOutput(ns("tabla_preview_ee"))
                )
              )
            ),

            # ── Sub 2: Mis datos ─────────────────────
            nav_panel(
              title = tagList(bs_icon("upload", class = "me-1"), "Mis datos"),
              br(),
              fileInput(ns("archivo_ee"), "Sube un archivo CSV:",
                        accept = c(".csv")),
              tags$hr(),
              DTOutput(ns("tabla_preview_propio_ee"))
            ),

            # ── Sub 3: Tipos de variables ────────────
            nav_panel(
              title = tagList(bs_icon("list-columns-reverse", class = "me-1"),
                              "Tipos de variables"),
              br(),
              uiOutput(ns("tabla_tipos_ee")),
              div(class = "mt-3",
                actionButton(ns("aplicar_tipos_ee"), "Aplicar tipos",
                             class = "btn-primary btn-sm me-2"),
                actionButton(ns("resetear_tipos_ee"), "Restaurar originales",
                             class = "btn-outline-secondary btn-sm")
              ),
              uiOutput(ns("tipos_aplicados_msg_ee"))
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
            "Elige una variable num\u00e9rica. Se compara el EE calculado por ",
            strong("f\u00f3rmula cl\u00e1sica"), " (s/\u221an, sobre tus datos ",
            "reales) contra el EE estimado por ", strong("bootstrap"),
            " (remuestreando tus propios datos miles de veces, sin usar ",
            "ninguna f\u00f3rmula) \u2014 deber\u00edan coincidir de cerca."
          ),
          layout_columns(
            col_widths = c(4, 8),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                uiOutput(ns("sel_var_ee")),
                uiOutput(ns("sel_n_boot_ee")),
                sliderInput(ns("reps_boot_ee"),
                            "N\u00famero de remuestreos bootstrap:",
                            min = 100, max = 5000, value = 1000, step = 100),
                actionButton(ns("calcular_ee"), "Calcular",
                             class = "btn-primary w-100 btn-sm",
                             icon  = icon("play")),
                tags$hr(),
                uiOutput(ns("cards_practica_ee"))
              )
            ),
            div(
              plotOutput(ns("plot_practica_ee"), height = "380px"),
              uiOutput(ns("insight_practica_ee"))
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
          verbatimTextOutput(ns("codigo_r_ee")),
          downloadButton(ns("descargar_script_ee"), "Descargar script .R",
                         class = "btn-outline-secondary btn-sm mt-2")
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_error_estandar_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

        # ── ¿Qué es? — curva DE constante vs EE decreciente ──
    output$plot_de_vs_n <- renderPlot({
      n     <- 1:50
      sigma <- 15
      ee    <- sigma / sqrt(n)
      df <- data.frame(
        n     = rep(n, 2),
        valor = c(rep(sigma, length(n)), ee),
        tipo  = rep(c("DE (constante)", "EE (disminuye)"), each = length(n))
      )
      ggplot(df, aes(x = n, y = valor, color = tipo)) +
        geom_line(linewidth = 1.2) +
        scale_color_manual(values = c(colores$primario, colores$acento),
                          name = NULL) +
        labs(x = "Tama\u00f1o de muestra (n)", y = NULL) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top",
              plot.background = element_rect(fill = colores$fondo, color = NA))
    }, width = 480, height = 220, res = 96)

    # ── ¿Qué es? — histograma comparativo DE vs EE ────
    # Recrea la idea cl\u00e1sica de "histograma de datos" vs.
    # "histograma de medias muestrales": la comparaci\u00f3n visual
    # m\u00e1s directa para explicar DE vs EE sin f\u00f3rmulas.
    output$plot_de_vs_ee_ind <- renderPlot({
      set.seed(123)
      poblacion  <- stats::rnorm(5000, mean = 35, sd = 5)
      individuos <- sample(poblacion, 30)
      df <- data.frame(x = individuos)
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = 12, fill = colores$primario, color = "white",
                       alpha = 0.9) +
        labs(x = "Peso (g)", y = "Frecuencia",
             title = "Pesos individuales (30 vampiros)") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 12))
    })

    output$plot_de_vs_ee_medias <- renderPlot({
      set.seed(123)
      poblacion <- stats::rnorm(5000, mean = 35, sd = 5)
      medias    <- replicate(300, mean(sample(poblacion, 10)))
      df <- data.frame(x = medias)
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = 12, fill = colores$acento, color = "white",
                       alpha = 0.9) +
        labs(x = "Peso (g)", y = "Frecuencia",
             title = "Medias de 300 muestras (n=10 c/u)") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$acento,
                                        face = "bold", size = 12))
    })



    # ── ¿Qué es? — ejemplo de enumeración completa ────
    output$poblacion_5_ee <- renderUI({
      pesos <- pesos_5_dantas_ee
      mu    <- mean(pesos)
      sigma <- sd_poblacional(pesos)

      tagList(
        div(style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:14px;",
          lapply(pesos, function(p) {
            div(style = paste0("background:", colores$primario,
                              "; color:#ffffff; border-radius:8px;",
                              "padding:12px 18px; font-weight:700;",
                              "font-size:1.1rem;"),
                paste0(p, " kg"))
          })
        ),
        par_numeros_grandes(
          "Media poblacional (\u03bc)", paste0(round(mu, 1), " kg"),
          "Desviaci\u00f3n est\u00e1ndar poblacional (\u03c3)", paste0(round(sigma, 2), " kg"),
          colores$primario
        ),
        div(class = "alert alert-success small mt-3 mb-0",
            bs_icon("check-circle-fill", class = "me-1"),
            strong("Por lo tanto: "), "conoces el ", strong(
            "par\u00e1metro poblacional exacto"), " \u2014 \u03bc y \u03c3 no son ",
            "estimados, son la verdad, porque mediste a TODA la poblaci\u00f3n."
        )
      )
    })

    output$muestra_ejemplo_ee <- renderUI({
      muestra <- pesos_5_dantas_ee[1:2]
      mu      <- mean(pesos_5_dantas_ee)
      tagList(
        div(style = "display:flex; flex-wrap:wrap; gap:10px; margin-bottom:14px;",
          lapply(muestra, function(p) {
            div(style = paste0("background:#fff3e0; border:2px solid ",
                              colores$acento,
                              "; border-radius:8px; padding:12px 18px;",
                              "font-weight:700; font-size:1.1rem; color:",
                              colores$acento, ";"),
                paste0(p, " kg"))
          })
        ),
        par_numeros_grandes(
          "Media muestral (\u0233)", paste0(round(mean(muestra), 1), " kg"),
          "Desviaci\u00f3n est\u00e1ndar muestral (s)",
          paste0(round(stats::sd(muestra), 1), " kg"),
          colores$acento
        ),
        div(class = "alert alert-warning small mt-3 mb-0",
            bs_icon("arrow-repeat", class = "me-1"),
            "Esta muestra intenta ", strong("aproximarse"), " a \u03bc = ",
            round(mu, 1), " kg \u2014 pero solo ",
            "acierta parcialmente. ", strong("\u00bfQu\u00e9 pasa con las otras 9 ",
            "muestras posibles?")
        )
      )
    })

    output$tabla_enumeracion_ee <- renderUI({
      df <- tabla_enumeracion_completa(pesos_5_dantas_ee)
      filas <- lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          style = paste0("background:", if (i %% 2 == 0) colores$fondo else "#ffffff", ";"),
          tags$td(i),
          tags$td(paste0(df$individuo_a[i], " kg")),
          tags$td(paste0(df$individuo_b[i], " kg")),
          tags$td(round(df$media_muestral[i], 1)),
          tags$td(round(df$de_muestral[i], 1))
        )
      })
      tags$table(
        class = "table table-sm table-bordered mb-2",
        style = "background:#ffffff;",
        tags$thead(
          tags$tr(
            tags$th("# Muestra", style = paste0("background:", colores$primario, "; color:#ffffff;")),
            tags$th("Danta A", style = paste0("background:", colores$primario, "; color:#ffffff;")),
            tags$th("Danta B", style = paste0("background:", colores$primario, "; color:#ffffff;")),
            tags$th("Media muestral", style = paste0("background:", colores$primario, "; color:#ffffff;")),
            tags$th("DE muestral", style = paste0("background:", colores$primario, "; color:#ffffff;"))
          )
        ),
        tags$tbody(
          filas,
          tags$tr(
            tags$td(colspan = "3", "Media \u2192", style = paste0("background:", colores$acento, "; color:#ffffff; font-weight:700;")),
            tags$td(round(mean(df$media_muestral), 1), style = paste0("background:", colores$acento, "; color:#ffffff; font-weight:700;")),
            tags$td(round(mean(df$de_muestral), 1), style = paste0("background:", colores$acento, "; color:#ffffff; font-weight:700;"))
          ),
          tags$tr(
            tags$td(colspan = "3", "DE \u2192", style = paste0("background:", colores$secundario, "; color:#ffffff; font-weight:700;")),
            tags$td(round(sd_poblacional(df$media_muestral), 1), style = paste0("background:", colores$secundario, "; color:#ffffff; font-weight:700;")),
            tags$td(round(stats::sd(df$de_muestral), 1), style = paste0("background:", colores$secundario, "; color:#ffffff; font-weight:700;"))
          )
        )
      )
    })

    output$resumen_tabla_enumeracion_ee <- renderUI({
      df         <- tabla_enumeracion_completa(pesos_5_dantas_ee)
      gran_media <- mean(df$media_muestral)
      ee_obs     <- sd_poblacional(df$media_muestral)
      mu         <- mean(pesos_5_dantas_ee)

      # Demostraci\u00f3n con f\u00f3rmula (igual que en la clase): la f\u00f3rmula
      # simple asume poblaci\u00f3n infinita y sobreestima el EE cuando la
      # muestra es una fracci\u00f3n grande de una poblaci\u00f3n peque\u00f1a.
      N_pob        <- length(pesos_5_dantas_ee)
      n_muest      <- 2
      sigma        <- sd_poblacional(pesos_5_dantas_ee)
      ee_simple    <- sigma / sqrt(n_muest)
      correccion   <- sqrt((N_pob - n_muest) / (N_pob - 1))
      ee_corregido <- ee_simple * correccion

      div(
        class = "alert alert-warning mt-3 mb-4",
        bs_icon("exclamation-triangle", class = "me-2"),
        strong("La media muestral cambia de muestra en muestra"),
        " \u2014 el error est\u00e1ndar mide justamente ", strong("cu\u00e1nto var\u00eda"),
        " alrededor de la media poblacional. Fij\u00e1te en las dos filas de ",
        "abajo de la tabla:",
        tags$ul(class = "mb-2 mt-2",
          tags$li(
            "La fila ", strong("\"Media \u2192\""), " muestra que el ",
            "promedio de las 10 medias muestrales es ",
            strong(paste0(round(gran_media, 1), " kg")), " \u2014 la \"gran ",
            "media\" \u2014 y coincide exactamente con \u03bc = ",
            round(mu, 1), " kg, ", strong("el verdadero valor del par\u00e1metro"),
            " poblacional."
          ),
          tags$li(
            "La fila ", strong("\"DE \u2192\""), " muestra que la ",
            "desviaci\u00f3n est\u00e1ndar de esas 10 medias con respecto a la ",
            "gran media es ", strong(paste0(round(ee_obs, 1), " kg")),
            " \u2014 eso es exactamente el ", strong("error est\u00e1ndar"), "."
          )
        ),
        tags$hr(class = "my-2"),
        p(class = "mb-1",
          strong(paste0("\u00bfDe d\u00f3nde sale ese ", round(ee_obs, 1),
                       "? Comprob\u00e9moslo con la f\u00f3rmula:"))
        ),
        tags$ol(class = "mb-2",
          tags$li("Desviaci\u00f3n est\u00e1ndar poblacional: \u03c3 = ",
                 round(sigma, 2), " kg"),
          tags$li(
            "F\u00f3rmula simple (asume poblaci\u00f3n infinita): EE = ",
            "\u03c3/\u221an = ", round(sigma, 2), "/\u221a", n_muest, " = ",
            strong(paste0(round(ee_simple, 2), " kg")), " \u2014 pero esto ",
            "es incorrecto aqu\u00ed, porque muestreaste ", n_muest, " de ",
            N_pob, " individuos \u2014 el ", round(100 * n_muest / N_pob, 0),
            "% de la poblaci\u00f3n."
          ),
          tags$li(
            "Correcci\u00f3n para poblaciones finitas: \u221a((N\u2212n)/(N\u22121)) = ",
            "\u221a((", N_pob, "\u2212", n_muest, ")/(", N_pob, "\u22121)) = ",
            round(correccion, 3), br(),
            tags$span(class = "text-muted", style = "font-size:0.85rem;",
              "Regla pr\u00e1ctica: si muestreas menos del ", strong("5% de la ",
              "poblaci\u00f3n"), " (n/N < 0.05), esta correcci\u00f3n casi no ",
              "cambia el resultado y muchos la omiten. Aqu\u00ed muestreaste el ",
              round(100 * n_muest / N_pob, 0), "%, muy por encima de ese ",
              "umbral \u2014 por eso la correcci\u00f3n es indispensable."
            )
          ),
          tags$li(
            "EE corregido = ", round(ee_simple, 2), " \u00d7 ",
            round(correccion, 3), " = ",
            strong(paste0(round(ee_corregido, 2), " kg")), " \u2014 \u00a1coincide ",
            "con el ", round(ee_obs, 1), " kg que obtuvimos enumerando las ",
            "10 muestras!"
          )
        ),
        "Con las 5 dantas pudiste comprobarlo de forma exacta (enumeraste ",
        "TODAS las muestras posibles); con los vampiros, solo puedes ",
        "razonarlo en teor\u00eda (hay demasiadas muestras posibles para ",
        "enumerarlas) \u2014 pero el principio es id\u00e9ntico."
      )
    })
    poblacion_sim_ee <- reactive({
      input$regenerar_sim_ee
      forma <- input$forma_pob_sim_ee
      N     <- input$N_sim_ee
      req(forma, N)
      switch(forma,
        "Normal"      = stats::rnorm(N, mean = 50, sd = 15),
        "Uniforme"    = stats::runif(N, min = 0, max = 100),
        "Exponencial" = stats::rexp(N, rate = 1 / 20)
      )
    })

    output$slider_n_muestra_sim_ee <- renderUI({
      req(input$N_sim_ee)
      max_n <- max(3, floor(input$N_sim_ee * 0.8))
      sliderInput(ns("n_muestra_sim_ee"), "Tama\u00f1o de cada muestra (n):",
                  min = 2, max = max_n, value = min(5, max_n), step = 1)
    })

    medias_muestrales_sim_ee <- reactive({
      pob  <- poblacion_sim_ee()
      n    <- input$n_muestra_sim_ee
      reps <- input$reps_sim_ee
      req(pob, n, reps, n < length(pob))
      # Muestreo SIN reemplazo — la población es finita y conocida,
      # igual que en el ejemplo de la Isla Sola.
      replicate(reps, mean(sample(pob, size = n, replace = FALSE)))
    })

    output$cards_sim_ee <- renderUI({
      pob    <- poblacion_sim_ee()
      medias <- medias_muestrales_sim_ee()
      n      <- input$n_muestra_sim_ee
      N      <- length(pob)
      sigma  <- stats::sd(pob)
      se_sin <- sigma / sqrt(n)
      se_con <- se_poblacion_finita(sigma, n, N)
      se_obs <- stats::sd(medias)
      tagList(
        tarjeta_metrica("Media poblacional (\u03bc)", round(mean(pob), 2),
                        "media"),
        tarjeta_metrica("Desviaci\u00f3n est\u00e1ndar poblacional (\u03c3)",
                        round(sigma, 2), "sd"),
        tarjeta_metrica("EE sin correcci\u00f3n (\u03c3/\u221an)",
                        round(se_sin, 2), "se_teorico"),
        tarjeta_metrica("EE con correcci\u00f3n de poblaci\u00f3n finita",
                        round(se_con, 2), "se_teorico"),
        tarjeta_metrica("EE observado (en esta simulaci\u00f3n)",
                        round(se_obs, 2), "se_observado", ultima = TRUE)
      )
    })

    output$plot_sim_ee <- renderPlot({
      medias <- medias_muestrales_sim_ee()
      pob    <- poblacion_sim_ee()
      req(length(medias) > 1)
      df <- data.frame(x = medias)
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = colores$secundario, color = "white",
                       alpha = 0.85) +
        geom_density(color = colores$primario, linewidth = 0.9) +
        geom_vline(xintercept = mean(pob), color = colores$peligro,
                  linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean(pob), y = Inf,
                 label = "Media poblacional", vjust = 2,
                 color = colores$peligro, fontface = "bold", size = 3.5) +
        labs(x = "Media de cada muestra", y = "Densidad",
             title = paste0("Distribuci\u00f3n muestral de la media (n = ",
                           input$n_muestra_sim_ee, ", N = ",
                           length(pob), ")")) +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 13))
    })

    output$insight_sim_ee <- renderUI({
      n <- input$n_muestra_sim_ee
      N <- input$N_sim_ee
      req(n, N)
      frac <- n / N
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          if (frac < 0.05) {
            paste0("Est\u00e1s muestreando solo el ", round(frac * 100, 1),
                  "% de la poblaci\u00f3n (n = ", n, " de N = ", N, ") \u2014 ",
                  "la correcci\u00f3n para poblaciones finitas casi no cambia ",
                  "el EE; compara las dos tarjetas, son casi id\u00e9nticas. ",
                  "Como en la Isla Sola: si tu muestra es una fracci\u00f3n ",
                  "min\u00fascula de la poblaci\u00f3n, la f\u00f3rmula simple ",
                  "\u03c3/\u221an es suficiente.")
          } else {
            paste0("Est\u00e1s muestreando el ", round(frac * 100, 1),
                  "% de la poblaci\u00f3n (n = ", n, " de N = ", N, ") \u2014 ",
                  "nota c\u00f3mo el EE con correcci\u00f3n de poblaci\u00f3n ",
                  "finita es menor: si ya mediste una fracci\u00f3n ",
                  "considerable de la poblaci\u00f3n, tienes menos ",
                  "incertidumbre de la que la f\u00f3rmula simple sugiere.")
          }
      )
    })

    # ── Datos reactivos (ejemplo) ─────────────────────
    datos <- reactive({
      fuente <- input$fuente_datos_ee
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
    output$info_dataset_ee <- renderUI({
      fuente <- input$fuente_datos_ee
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
          strong("Salarios: "), "sesgado a la derecha \u2014 buen ejemplo ",
          "para comparar EE por f\u00f3rmula vs. bootstrap."
        ),
        attitude = div(
          class = "alert alert-info small py-2 px-3 mb-2",
          strong("Actitud: "), "puntajes de encuesta de clima laboral en ",
          "una oficina de seguros."
        )
      )
    })

    output$cards_datos_ee <- renderUI({
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

    output$tabla_preview_ee <- renderDT({
      req(datos_mod())
      datatable(datos_mod(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Mis datos ──────────────────────────────────────
    datos_propio_ee <- reactive({
      req(input$archivo_ee)
      df <- readr::read_csv(input$archivo_ee$datapath, show_col_types = FALSE)
      df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
    })

    observeEvent(input$archivo_ee, {
      req(datos_propio_ee())
      datos_mod(as.data.frame(datos_propio_ee()))
    })

    output$tabla_preview_propio_ee <- renderDT({
      req(datos_propio_ee())
      datatable(datos_propio_ee(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Tipos de variables ────────────────────────────
    tipos_usuario_ee <- reactiveVal(NULL)

    output$tabla_tipos_ee <- renderUI({
      req(datos_mod())
      d  <- datos_mod()
      tu <- tipos_usuario_ee()
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

    observeEvent(input$aplicar_tipos_ee, {
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
      tipos_usuario_ee(tu)
      datos_mod(d)
      output$tipos_aplicados_msg_ee <- renderUI({
        div(class = "alert alert-success small mt-3",
            bs_icon("check-circle-fill", class = "me-1"),
            "Tipos de variable aplicados.")
      })
    })

    observeEvent(input$resetear_tipos_ee, {
      tipos_usuario_ee(NULL)
      datos_mod(datos())
      output$tipos_aplicados_msg_ee <- renderUI({
        div(class = "alert alert-info small mt-3",
            bs_icon("arrow-counterclockwise", class = "me-1"),
            "Tipos restaurados a los originales.")
      })
    })

    # ── Practica con datos reales ──────────────────────
    vars_numericas_ee <- reactive({
      req(datos_mod())
      names(which(sapply(datos_mod(), is.numeric)))
    })

    output$sel_var_ee <- renderUI({
      req(vars_numericas_ee())
      selectInput(ns("var_ee"), "Variable num\u00e9rica:",
                  choices = vars_numericas_ee())
    })

    valores_ee <- reactive({
      req(datos_mod(), input$var_ee)
      x <- datos_mod()[[input$var_ee]]
      x[!is.na(x)]
    })

    output$sel_n_boot_ee <- renderUI({
      req(valores_ee())
      n_total <- length(valores_ee())
      sliderInput(ns("n_boot_ee"), "Tama\u00f1o de cada remuestreo (n):",
                  min = 2, max = n_total,
                  value = min(30, n_total), step = 1)
    })

    resultado_practica_ee <- eventReactive(input$calcular_ee, {
      x    <- valores_ee()
      n    <- input$n_boot_ee
      reps <- input$reps_boot_ee
      req(x, n, reps)
      list(
        media_muestra = mean(x),
        se_formula    = se_formula(x, n),
        medias_boot   = bootstrap_medias(x, n, reps),
        n             = n
      )
    })

    output$cards_practica_ee <- renderUI({
      req(resultado_practica_ee())
      r <- resultado_practica_ee()
      se_boot <- stats::sd(r$medias_boot)
      tagList(
        tarjeta_metrica("Media de la variable", round(r$media_muestra, 2),
                        "media"),
        tarjeta_metrica("Error est\u00e1ndar (f\u00f3rmula: s/\u221an)",
                        round(r$se_formula, 2), "se_formula"),
        tarjeta_metrica("Error est\u00e1ndar (bootstrap)", round(se_boot, 2),
                        "se_bootstrap", ultima = TRUE)
      )
    })

    output$plot_practica_ee <- renderPlot({
      ancho <- session$clientData[[paste0("output_", ns("plot_practica_ee"),
                                          "_width")]]
      req(ancho, ancho > 0)
      req(resultado_practica_ee())
      r  <- resultado_practica_ee()
      se_boot <- stats::sd(r$medias_boot)
      df <- data.frame(x = r$medias_boot)
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = colores$secundario, color = "white",
                       alpha = 0.85) +
        geom_density(color = colores$primario, linewidth = 0.9) +
        geom_vline(xintercept = r$media_muestra, color = colores$primario,
                  linewidth = 1, linetype = "solid") +
        geom_vline(xintercept = r$media_muestra + c(-1, 1) * r$se_formula,
                  color = colores$acento, linewidth = 0.8,
                  linetype = "dashed") +
        geom_vline(xintercept = r$media_muestra + c(-1, 1) * se_boot,
                  color = colores$peligro, linewidth = 0.8,
                  linetype = "dotted") +
        annotate("text", x = r$media_muestra, y = Inf, label = "Media",
                 vjust = 2, color = colores$primario, fontface = "bold",
                 size = 3.5) +
        annotate("text", x = r$media_muestra + r$se_formula, y = Inf,
                 label = "\u00b1EE f\u00f3rmula", vjust = 4,
                 color = colores$acento, fontface = "bold", size = 3) +
        annotate("text", x = r$media_muestra + se_boot, y = Inf,
                 label = "\u00b1EE bootstrap", vjust = 6,
                 color = colores$peligro, fontface = "bold", size = 3) +
        labs(x = paste0("Media de cada remuestreo (n = ", r$n, ")"),
             y = "Densidad",
             title = "Distribuci\u00f3n bootstrap de la media") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 13))
    })

    output$insight_practica_ee <- renderUI({
      req(resultado_practica_ee())
      r       <- resultado_practica_ee()
      se_boot <- stats::sd(r$medias_boot)
      dif_pct <- abs(r$se_formula - se_boot) / r$se_formula * 100
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("El EE por f\u00f3rmula (", round(r$se_formula, 2),
                ") y el EE por bootstrap (", round(se_boot, 2), ") difieren ",
                "en un ", round(dif_pct, 1), "% \u2014 ambos estiman lo ",
                "mismo por caminos distintos: uno usa una f\u00f3rmula ",
                "matem\u00e1tica, el otro remuestrea tus datos reales miles ",
                "de veces sin asumir ninguna f\u00f3rmula.")
      )
    })

    # ── Código R ──────────────────────────────────────
    output$codigo_r_ee <- renderText({
      req(input$var_ee, input$n_boot_ee, input$reps_boot_ee)
      fuente <- input$fuente_datos_ee
      es_propio <- !is.null(datos_propio_ee()) && !is.null(input$archivo_ee)
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
        "# \u2500\u2500 Error est\u00e1ndar: f\u00f3rmula vs. bootstrap \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n",
        "# Dataset: ", nm_datos, "\n",
        "# Variable: ", input$var_ee, "\n\n",
        linea_carga, "\n\n",
        "x <- ", nm_datos, "$", input$var_ee, "\n",
        "x <- x[!is.na(x)]\n",
        "n <- ", input$n_boot_ee, "\n\n",
        "# EE por f\u00f3rmula cl\u00e1sica\n",
        "se_formula <- sd(x) / sqrt(n)\n\n",
        "# EE por bootstrap (remuestreo con reemplazo)\n",
        "reps <- ", input$reps_boot_ee, "\n",
        "medias_boot <- replicate(reps, mean(sample(x, size = n, replace = TRUE)))\n",
        "se_boot <- sd(medias_boot)\n\n",
        "c(se_formula = se_formula, se_boot = se_boot)\n"
      )
    })

    output$descargar_script_ee <- downloadHandler(
      filename = function() paste0("StatBasics_error_estandar_",
                                    Sys.Date(), ".R"),
      content  = function(file) writeLines(output$codigo_r_ee(), file)
    )

  })
}
