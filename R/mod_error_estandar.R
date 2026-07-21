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
              col_widths = breakpoints(sm = c(12, 12, 12), md = c(4, 4, 4)),
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

          conector_infografia("Para verlo con n\u00fameros concretos, empecemos con un caso donde S\u00cd podemos medir a toda la poblaci\u00f3n"),

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

          conector_infografia("Esto mismo pasa con poblaciones grandes que no pod\u00e9s enumerar \u2014 mir\u00e9moslo con miles de vampiros"),

          p(class = "text-muted mt-3 mb-2",
            "As\u00ed se ve esa idea de ", strong("precisi\u00f3n"), " en un ",
            "gr\u00e1fico, a mayor escala:"
          ),
          fluidRow(
            column(6, plotOutput(ns("plot_de_vs_ee_ind"), height = "280px")),
            column(6, plotOutput(ns("plot_de_vs_ee_medias"), height = "280px"))
          ),
          div(class = "d-flex align-items-center gap-2 justify-content-center mt-2 mb-1",
              actionButton(ns("construir_anim_ee"),
                           tagList(bs_icon("play-fill"), " Construir la distribuci\u00f3n"),
                           class = "btn-primary btn-sm"),
              actionButton(ns("reiniciar_anim_ee"),
                           tagList(bs_icon("arrow-counterclockwise"), " Reiniciar"),
                           class = "btn-outline-secondary btn-sm"),
              span(class = "small text-muted ms-2", uiOutput(ns("contador_anim_ee"), inline = TRUE))
          ),
          div(class = "alert alert-secondary small mt-2 mb-3",
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "Con las dantas pudiste enumerar las 10 muestras posibles ",
              "porque la poblaci\u00f3n era chiquita. Con miles de vampiros no ",
              "se puede enumerar, pero el principio es id\u00e9ntico. A la ",
              "izquierda: pesos individuales de una sola muestra de ",
              "vampiros \u2014 var\u00edan bastante alrededor de su ",
              strong("media"), " (l\u00ednea punteada roja, equivalente a la ",
              em("media muestral"), " x\u0304 de las dantas). A la ",
              "derecha: la ", em("distribuci\u00f3n"), " de 300 medias ",
              "muestrales distintas, cada una calculada de su propia ",
              "muestra de 30 vampiros \u2014 mucho m\u00e1s angosta alrededor de ",
              "su ", strong("gran media"), " (equivalente a la ", em("media ",
              "poblacional"), " \u03bc de las dantas, que ac\u00e1 no conocemos ",
              "con certeza, pero la gran media se le acerca much\u00edsimo). ",
              "Compar\u00e1 el ancho de las dos distribuciones: los datos ",
              "individuales (izquierda) se dispersan mucho respecto a su ",
              "media \u2014 eso es la ", strong("desviaci\u00f3n est\u00e1ndar"),
              ". Las medias muestrales (derecha) se dispersan mucho menos ",
              "respecto a la gran media \u2014 eso es el ",
              strong("error est\u00e1ndar"), ". Toc\u00e1 ",
              strong("\"Construir la distribuci\u00f3n\""), " arriba para ver, ",
              "muestra por muestra, c\u00f3mo se va formando esa nube de la ",
              "derecha \u2014 y c\u00f3mo la gran media se estabiliza a medida ",
              "que se acumulan m\u00e1s muestras. Esa desviaci\u00f3n est\u00e1ndar de ",
              "las medias \u2014el error est\u00e1ndar\u2014 no hace falta simularla ",
              "cada vez: se puede ", strong("calcular"), " con \u03c3/\u221an si ",
              "conoc\u00e9s la desviaci\u00f3n est\u00e1ndar poblacional (\u03c3), o ",
              strong("estimar"), " con s/\u221an usando la desviaci\u00f3n ",
              "est\u00e1ndar de tu propia muestra (s) \u2014 esa es la f\u00f3rmula ",
              "que ver\u00e1s repetida en el resto de este m\u00f3dulo."
          ),

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
              "m\u00e1s simple, pero el mismo principio aplica a otros ",
              "estad\u00edsticos con sus propias f\u00f3rmulas."
            )
          ),

          p(class = "text-muted mt-3 mb-0",
            "Compru\u00e9balo t\u00fa mismo en ", strong("Simulaci\u00f3n interactiva"),
            " \u2014 repet\u00ed el muestreo miles de veces y mir\u00e1 c\u00f3mo la ",
            "gran media y el EE observado se acercan cada vez m\u00e1s a lo ",
            "que predice la f\u00f3rmula."
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
            "Eleg\u00ed el tama\u00f1o de cada muestra y cu\u00e1ntas muestras ",
            "simular. Se toman muchas muestras de ese tama\u00f1o, se ",
            "calcula la media de cada una, y se grafica c\u00f3mo se ",
            "distribuyen esas medias \u2014 igual que hicimos con los ",
            "vampiros, pero ac\u00e1 vos elegís los par\u00e1metros."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                sliderInput(ns("n_muestra_sim_ee"), "Tama\u00f1o de cada muestra (n):",
                            min = 2, max = 200, value = 10, step = 1),
                sliderInput(ns("reps_sim_ee"),
                            "N\u00famero de muestras simuladas:",
                            min = 100, max = 5000, value = 1000, step = 100),
                actionButton(ns("regenerar_sim_ee"), "Nueva simulaci\u00f3n",
                             class = "btn-outline-secondary w-100 btn-sm",
                             icon  = icon("shuffle"))
              )
            ),
            div(
              plotOutput(ns("plot_sim_ee"), height = "380px"),
              uiOutput(ns("insight_sim_ee")),
              tags$hr(),
              uiOutput(ns("cards_sim_ee"))
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
                    ns("archivo_ee"),
                    label       = "Seleccionar archivo:",
                    accept      = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Buscar\u2026",
                    placeholder = "CSV o Excel"
                  ),
                  selectInput(
                    ns("separador_ee"),
                    label   = "Separador (CSV):",
                    choices = c(
                      "Coma (,)"         = ",",
                      "Punto y coma (;)" = ";",
                      "Tabulador"        = "\t"
                    )
                  ),
                  tags$hr(),
                  uiOutput(ns("resumen_datos_propio_ee"))
                ),
                card(
                  card_header(bs_icon("eye", class = "me-1"), "Vista previa"),
                  card_body(
                    style = "overflow: auto;",
                    uiOutput(ns("cards_datos_propio_ee")),
                    br(),
                    DTOutput(ns("tabla_preview_propio_ee"))
                  )
                )
              )
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
            strong("f\u00f3rmula cl\u00e1sica"), " (s/\u221aN, sobre tus datos ",
            "reales) contra el EE estimado por ", strong("bootstrap"),
            " (remuestreando tu dataset completo, con reemplazo, miles de ",
            "veces, sin usar ninguna f\u00f3rmula) \u2014 deber\u00edan coincidir de cerca."
          ),
          layout_columns(
            col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
            fill = FALSE,
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Par\u00e1metros"),
              card_body(
                uiOutput(ns("sel_var_ee")),
                sliderInput(ns("reps_boot_ee"),
                            "N\u00famero de remuestreos bootstrap:",
                            min = 100, max = 5000, value = 1000, step = 100),
                actionButton(ns("calcular_ee"), "Calcular",
                             class = "btn-primary w-100 btn-sm",
                             icon  = icon("play"))
              )
            ),
            div(
              plotOutput(ns("plot_practica_ee"), height = "380px"),
              uiOutput(ns("insight_practica_ee")),
              tags$hr(),
              uiOutput(ns("cards_practica_ee"))
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

    # ── ¿Qué es? — histograma comparativo DE vs EE ────
    # Recrea la idea cl\u00e1sica de "histograma de datos" vs.
    # "histograma de medias muestrales": la comparaci\u00f3n visual
    # m\u00e1s directa para explicar DE vs EE sin f\u00f3rmulas.
    individuos_ee <- reactive({
      set.seed(123)
      poblacion <- stats::rnorm(5000, mean = 35, sd = 5)
      set.seed(1)
      sample(poblacion, 30)
    })

    # Eje X COMPARTIDO entre los dos gr\u00e1ficos (basado en el rango de
    # los datos individuales, que es el m\u00e1s ancho de los dos) \u2014 as\u00ed
    # la diferencia de dispersi\u00f3n se ve a simple vista, sin tener que
    # comparar los n\u00fameros de cada eje.
    xlim_compartido_ee <- reactive({
      rango  <- range(individuos_ee())
      margen <- diff(rango) * 0.06
      rango + c(-margen, margen)
    })

    output$plot_de_vs_ee_ind <- renderPlot({
      individuos <- individuos_ee()
      media_ind  <- mean(individuos)
      de_ind     <- stats::sd(individuos)
      xlim       <- xlim_compartido_ee()

      # Mismo esquema de apilado por puntos que el gr\u00e1fico de la
      # derecha, para que ambos se lean con el mismo lenguaje visual.
      n_bins    <- 12
      bin_ancho <- diff(xlim) / n_bins
      bin_idx   <- floor((individuos - xlim[1]) / bin_ancho)
      bin_idx   <- pmin(bin_idx, n_bins - 1)
      x_columna <- xlim[1] + (bin_idx + 0.5) * bin_ancho
      y_stack   <- ave(seq_along(individuos), bin_idx, FUN = seq_along)
      df <- data.frame(x = x_columna, y = y_stack)

      ggplot(df, aes(x = x, y = y)) +
        geom_point(size = 3.6, shape = 21, fill = colores$primario,
                  color = "white", stroke = 0.7, alpha = 0.95) +
        geom_vline(xintercept = media_ind, color = colores$peligro,
                  linewidth = 1, linetype = "dashed") +
        annotate("text", x = media_ind, y = Inf,
                 label = "Media", vjust = 2,
                 color = colores$peligro, fontface = "bold", size = 4.3) +
        coord_cartesian(xlim = xlim, ylim = c(0, max(y_stack) * 1.18)) +
        scale_y_continuous(breaks = NULL) +
        labs(x = "Peso (g)", y = NULL,
             title = "30 vampiros, uno por uno",
             subtitle = paste0("Media = ", round(media_ind, 2),
                              " g   \u00b7   Desv. est\u00e1ndar = ",
                              round(de_ind, 2), " g")) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 16),
              plot.subtitle = element_text(color = colores$texto, size = 12),
              axis.ticks.y = element_blank())
    })

    # ── Animaci\u00f3n: construcci\u00f3n en vivo de la distribuci\u00f3n muestral ──
    # Precalcula las 300 medias una sola vez; la animaci\u00f3n solo
    # revela progresivamente cu\u00e1ntas de esas 300 se muestran.
    medias_completas_ee <- reactive({
      set.seed(123)
      poblacion <- stats::rnorm(5000, mean = 35, sd = 5)
      set.seed(2)
      replicate(300, mean(sample(poblacion, 30)))
    })

    # L\u00edmites fijos y esquema de "bins" para apilar puntos. Usa el
    # MISMO xlim que el gr\u00e1fico de la izquierda (no el rango propio
    # de las medias) para que la comparaci\u00f3n de ancho sea visual y
    # directa \u2014 y adem\u00e1s no salte mientras se van agregando puntos.
    limites_anim_ee <- reactive({
      m      <- medias_completas_ee()
      n_bins <- 36
      xlim   <- xlim_compartido_ee()
      bin_ancho <- diff(xlim) / n_bins
      bin_idx   <- floor((m - xlim[1]) / bin_ancho)
      bin_idx   <- pmin(bin_idx, n_bins - 1)
      alto_max  <- max(table(bin_idx))
      list(xlim = xlim, bin_ancho = bin_ancho, n_bins = n_bins,
           ymax = alto_max * 1.18)
    })

    n_mostradas_ee <- reactiveVal(1)
    animando_ee    <- reactiveVal(FALSE)

    observeEvent(input$construir_anim_ee, {
      if (n_mostradas_ee() >= 300) n_mostradas_ee(1)
      animando_ee(TRUE)
    })

    observeEvent(input$reiniciar_anim_ee, {
      animando_ee(FALSE)
      n_mostradas_ee(1)
    })

    observe({
      req(animando_ee())
      invalidateLater(35)
      isolate({
        actual <- n_mostradas_ee()
        if (actual >= 300) {
          animando_ee(FALSE)
        } else {
          n_mostradas_ee(min(300, actual + 4))
        }
      })
    })

    output$contador_anim_ee <- renderUI({
      n <- n_mostradas_ee()
      if (n >= 300) {
        tagList(bs_icon("check-circle-fill",
                        style = paste0("color:", colores$exito), class = "me-1"),
                "300 de 300 muestras")
      } else {
        paste0("Muestra ", n, " de 300")
      }
    })

    output$plot_de_vs_ee_medias <- renderPlot({
      medias_todas <- medias_completas_ee()
      lims         <- limites_anim_ee()
      n_mostrar    <- n_mostradas_ee()
      medias       <- medias_todas[seq_len(n_mostrar)]
      media_actual <- mean(medias)
      etiqueta     <- if (n_mostrar >= 300) "Gran media" else "Media hasta ahora"

      # Cada punto se apila sobre los anteriores que cayeron en su
      # mismo "bin" \u2014 el orden de aparici\u00f3n es el orden de la
      # animaci\u00f3n, as\u00ed que un punto nunca cambia de posici\u00f3n una
      # vez que aparece (sin importar cu\u00e1ntos m\u00e1s se agreguen despu\u00e9s).
      bin_idx   <- floor((medias - lims$xlim[1]) / lims$bin_ancho)
      bin_idx   <- pmin(bin_idx, lims$n_bins - 1)
      x_columna <- lims$xlim[1] + (bin_idx + 0.5) * lims$bin_ancho
      y_stack   <- ave(seq_along(medias), bin_idx, FUN = seq_along)
      df <- data.frame(x = x_columna, y = y_stack)

      de_medias <- stats::sd(medias)
      subt <- if (n_mostrar >= 300) {
        paste0("Gran media = ", round(media_actual, 2),
              " g   \u00b7   Desv. est\u00e1ndar de las medias (= EE) = ",
              round(de_medias, 2), " g")
      } else {
        paste0("Cada punto = la media de UNA muestra de 30 vampiros \u2014 ",
              n_mostrar, " de 300 mostradas")
      }

      ggplot(df, aes(x = x, y = y)) +
        geom_point(size = 3, shape = 21, fill = colores$acento,
                  color = "white", stroke = 0.7, alpha = 0.95) +
        geom_vline(xintercept = media_actual, color = colores$peligro,
                  linewidth = 1, linetype = "dashed") +
        annotate("text", x = media_actual, y = Inf,
                 label = etiqueta, vjust = 2,
                 color = colores$peligro, fontface = "bold", size = 4.3) +
        coord_cartesian(xlim = lims$xlim, ylim = c(0, lims$ymax)) +
        scale_y_continuous(breaks = NULL) +
        labs(x = "Peso (g)", y = NULL,
             title = "300 medias muestrales, una por una",
             subtitle = subt) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$acento,
                                        face = "bold", size = 16),
              plot.subtitle = element_text(color = colores$texto, size = 12),
              axis.ticks.y = element_blank())
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
    # Par\u00e1metros TEÓRICOS conocidos de cada forma de poblaci\u00f3n
    # (no se estiman de una muestra: son la f\u00f3rmula exacta de cada
    # distribuci\u00f3n). Evita tener que introducir la correcci\u00f3n por
    # poblaci\u00f3n finita: se muestrea de una poblaci\u00f3n conceptualmente
    # infinita, como en el ejemplo de los vampiros.
    # (no se estiman de una muestra: son la f\u00f3rmula exacta de la
    # distribuci\u00f3n Normal). Evita tener que introducir la correcci\u00f3n
    # por poblaci\u00f3n finita: se muestrea de una poblaci\u00f3n
    # conceptualmente infinita, como en el ejemplo de los vampiros.
    parametros_pob_sim_ee <- reactive({
      list(mu = 50, sigma = 15)
    })

    medias_muestrales_sim_ee <- reactive({
      input$regenerar_sim_ee
      n    <- input$n_muestra_sim_ee
      reps <- input$reps_sim_ee
      req(n, reps)
      replicate(reps, mean(stats::rnorm(n, mean = 50, sd = 15)))
    })

    output$cards_sim_ee <- renderUI({
      medias     <- medias_muestrales_sim_ee()
      param      <- parametros_pob_sim_ee()
      n          <- input$n_muestra_sim_ee
      gran_media <- mean(medias)
      se_teorico <- param$sigma / sqrt(n)
      se_obs     <- stats::sd(medias)
      tagList(
        tarjeta_metrica("Media poblacional (\u03bc)", round(param$mu, 2),
                        "media"),
        tarjeta_metrica("Gran media (promedio de tus muestras)",
                        round(gran_media, 2), "gran_media"),
        tarjeta_metrica("EE esperado por f\u00f3rmula (\u03c3/\u221an)",
                        round(se_teorico, 2), "se_teorico"),
        tarjeta_metrica("EE observado (en esta simulaci\u00f3n)",
                        round(se_obs, 2), "se_observado", ultima = TRUE)
      )
    })

    output$plot_sim_ee <- renderPlot({
      medias <- medias_muestrales_sim_ee()
      param  <- parametros_pob_sim_ee()
      req(length(medias) > 1)
      df <- data.frame(x = medias)
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = colores$secundario, color = "white",
                       alpha = 0.85) +
        geom_density(color = colores$primario, linewidth = 0.9) +
        geom_vline(xintercept = param$mu, color = colores$peligro,
                  linewidth = 1, linetype = "dashed") +
        annotate("text", x = param$mu, y = Inf,
                 label = "Media poblacional (\u03bc)", vjust = 2,
                 color = colores$peligro, fontface = "bold", size = 4.3) +
        geom_vline(xintercept = mean(medias), color = colores$primario,
                  linewidth = 1, linetype = "dotted") +
        annotate("text", x = mean(medias), y = Inf,
                 label = "Gran media", vjust = 4,
                 color = colores$primario, fontface = "bold", size = 4.3) +
        labs(x = "Media de cada muestra", y = "Densidad",
             title = paste0("Distribuci\u00f3n muestral de la media (n = ",
                           input$n_muestra_sim_ee, ")")) +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 17))
    })

    output$insight_sim_ee <- renderUI({
      medias     <- medias_muestrales_sim_ee()
      param      <- parametros_pob_sim_ee()
      n          <- input$n_muestra_sim_ee
      reps       <- input$reps_sim_ee
      gran_media <- mean(medias)
      se_teorico <- param$sigma / sqrt(n)
      se_obs     <- stats::sd(medias)
      div(class = "alert alert-info small mt-2",
          bs_icon("lightbulb-fill", class = "me-1"),
          paste0("La gran media (", round(gran_media, 2), ") ya se ",
                "parece bastante a la verdadera media poblacional (",
                round(param$mu, 2), "), y el EE observado (",
                round(se_obs, 2), ") se acerca al esperado por f\u00f3rmula (",
                round(se_teorico, 2), "). Suben m\u00e1s el n\u00famero de ",
                "muestras simuladas y vas a ver c\u00f3mo ambos pares se ",
                "acercan todav\u00eda m\u00e1s \u2014 eso es la ley de los grandes ",
                "n\u00fameros en acci\u00f3n.")
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

    output$tabla_preview_ee <- renderDT({
      req(datos_mod())
      datatable(datos_mod(), rownames = FALSE,
                options = list(dom = "t", scrollY = "300px", scrollX = TRUE, paging = FALSE),
                class = "table-sm table-striped")
    })

    # ── Mis datos ──────────────────────────────────────
    datos_propio_ee <- reactive({
      req(input$archivo_ee)
      ext <- tools::file_ext(input$archivo_ee$name)
      tryCatch({
        df <- if (ext %in% c("xlsx", "xls"))
          readxl::read_excel(input$archivo_ee$datapath)
        else
          readr::read_delim(input$archivo_ee$datapath,
                            delim = input$separador_ee %||% ",",
                            show_col_types = FALSE)
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }, error = function(e) {
        showNotification(paste("Error al leer archivo:", e$message),
                         type = "error", duration = 6)
        NULL
      })
    })

    observeEvent(datos_propio_ee(), {
      req(datos_propio_ee())
      datos_mod(as.data.frame(datos_propio_ee()))
    })

    output$resumen_datos_propio_ee <- renderUI({
      req(datos_propio_ee())
      d <- datos_propio_ee()
      div(class = "small text-muted",
          bs_icon("check-circle-fill",
                  style = paste0("color:", colores$exito), class = "me-1"),
          paste0(nrow(d), " filas \u00b7 ", ncol(d), " columnas"))
    })

    output$cards_datos_propio_ee <- renderUI({
      req(datos_propio_ee())
      d    <- datos_propio_ee()
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

    resultado_practica_ee <- eventReactive(input$calcular_ee, {
      x    <- valores_ee()
      n    <- length(x)
      reps <- input$reps_boot_ee
      req(x, n, reps)
      list(
        media_muestra = mean(x),
        sd_muestra    = stats::sd(x),
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
        tarjeta_metrica("Desviaci\u00f3n est\u00e1ndar de la muestra (s)",
                        round(r$sd_muestra, 2), "sd"),
        tarjeta_metrica("Error est\u00e1ndar (f\u00f3rmula: s/\u221aN)",
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
                 size = 4.3) +
        annotate("text", x = r$media_muestra + r$se_formula, y = Inf,
                 label = "\u00b1EE f\u00f3rmula", vjust = 4,
                 color = colores$acento, fontface = "bold", size = 3) +
        annotate("text", x = r$media_muestra + se_boot, y = Inf,
                 label = "\u00b1EE bootstrap", vjust = 6,
                 color = colores$peligro, fontface = "bold", size = 3) +
        labs(x = paste0("Media de cada remuestreo (N = ", r$n,
                       ", tama\u00f1o real de tu muestra)"),
             y = "Densidad",
             title = "Distribuci\u00f3n bootstrap de la media") +
        theme_light(base_size = 17) +
        theme(plot.background = element_rect(fill = colores$fondo, color = NA),
              plot.title = element_text(color = colores$primario,
                                        face = "bold", size = 17))
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
      req(input$var_ee, input$reps_boot_ee)
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
        "n <- length(x)  # tama\u00f1o real de tu muestra\n\n",
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
