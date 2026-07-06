# ============================================================
# mod_describiendo_datos.R — Describiendo los datos
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Módulo "envolvente" (no tiene lógica propia): agrupa
# Tendencia central y Dispersión como 2 subtabs, siguiendo el
# mismo patrón navset_pill usado dentro de "Los datos" en cada
# módulo individual.
# ============================================================

mod_describiendo_datos_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "py-3 px-2",
      h4(
        bs_icon("bar-chart-steps", class = "me-2"),
        "Describiendo los datos",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Toda variable num\u00e9rica se resume respondiendo dos preguntas: ",
        strong("\u00bfd\u00f3nde se concentran los datos?"),
        " (tendencia central) y ",
        strong("\u00bfqu\u00e9 tan dispersos est\u00e1n?"),
        " (dispersi\u00f3n)."
      )
    ),
    navset_card_tab(
      nav_panel(
        title = tagList(bs_icon("bar-chart", class = "me-1"),
                        "Tendencia central"),
        mod_tendencia_central_ui(ns("tendencia_central"))
      ),
      nav_panel(
        title = tagList(bs_icon("arrows-expand", class = "me-1"),
                        "Dispersi\u00f3n"),
        mod_dispersion_ui(ns("dispersion"))
      )
    )
  )
}

mod_describiendo_datos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    mod_tendencia_central_server("tendencia_central")
    mod_dispersion_server("dispersion")
  })
}
