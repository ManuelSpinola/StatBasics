# ============================================================
# mod_inicio.R — Pantalla de bienvenida
# StatBasics · StatSuite · Manuel Sp\u00ednola · ICOMVIS · UNA
# ============================================================

mod_inicio_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "py-4 px-3 text-center",
      style = "max-width: 780px; margin: 0 auto;",
      img(src = "www/hexsticker_StatBasics.png", height = "130px",
          style = "display:block; margin: 0 auto;"),
      h2(
        class = "mt-3 mb-2",
        style = paste0("color:", colores$primario, "; font-weight:700;"),
        "Bienvenido a StatBasics"
      ),
      p(
        class = "text-muted",
        "Los conceptos fundamentales de la estad\u00edstica, explicados de ",
        "forma did\u00e1ctica e interactiva: tendencia central, dispersi\u00f3n, ",
        "distribuciones, error est\u00e1ndar, intervalos de confianza, ",
        "correlaci\u00f3n y probabilidad b\u00e1sica."
      ),
      tags$hr(),
      layout_columns(
        col_widths = c(6, 6),
        div(
          class = "alert alert-info small text-start py-2 px-3 mb-0",
          bs_icon("info-circle-fill", class = "me-1"),
          strong("Parte de StatSuite."), br(),
          "Complementa a StatModels (frecuentista), StatML (aprendizaje ",
          "autom\u00e1tico), StatBayes (bayesiano) y otras apps m\u00e1s \u2014 conoce ",
          "toda la suite en ",
          tags$a(href = "https://statsuite.netlify.app", target = "_blank",
                 "statsuite.netlify.app"),
          "."
        ),
        div(
          class = "alert alert-info small text-start py-2 px-3 mb-0",
          bs_icon("mortarboard-fill", class = "me-1"),
          strong("Enfoque pedag\u00f3gico."), br(),
          "Cada m\u00f3dulo combina teor\u00eda, datos reales y simulaciones ",
          "interactivas para construir intuici\u00f3n estad\u00edstica."
        )
      )
    )
  )
}

mod_inicio_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sin l\u00f3gica reactiva por ahora
  })
}
