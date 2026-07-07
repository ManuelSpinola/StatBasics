# ============================================================
# mod_placeholders.R — Módulos "próximamente" de StatBasics
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Cada uno de estos se reemplazará por su propio archivo
# mod_*.R completo cuando se construya (mismo patrón que
# mod_tendencia_central.R).
# ============================================================

# ── Probabilidad básica ──────────────────────────────────────
mod_probabilidad_ui <- function(id) {
  ns <- NS(id)
  proximamente_ui(
    icono     = "dice-5",
    titulo    = "Probabilidad b\u00e1sica",
    subtitulo = paste(
      "Reglas b\u00e1sicas de probabilidad y una introducci\u00f3n intuitiva",
      "al teorema de Bayes \u2014 puente hacia StatBayes."
    ),
    datasets  = "Ejemplos simulados (monedas, dados, diagn\u00f3sticos m\u00e9dicos)"
  )
}
mod_probabilidad_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
