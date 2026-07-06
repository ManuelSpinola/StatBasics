# ============================================================
# mod_placeholders.R — Módulos "próximamente" de StatBasics
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Cada uno de estos se reemplazará por su propio archivo
# mod_*.R completo cuando se construya (mismo patrón que
# mod_tendencia_central.R).
# ============================================================

# ── Distribuciones ─────────────────────────────────────────
mod_distribuciones_ui <- function(id) {
  ns <- NS(id)
  proximamente_ui(
    icono     = "distribute-vertical",
    titulo    = "Distribuciones de probabilidad",
    subtitulo = paste(
      "Normal, binomial, Poisson, t y chi-cuadrada:",
      "forma, par\u00e1metros y cu\u00e1ndo se usa cada una."
    ),
    datasets  = "Simulaciones param\u00e9tricas interactivas (sin dataset fijo)"
  )
}
mod_distribuciones_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

# ── Error estándar ──────────────────────────────────────────
mod_error_estandar_ui <- function(id) {
  ns <- NS(id)
  proximamente_ui(
    icono     = "rulers",
    titulo    = "Error est\u00e1ndar del estad\u00edstico",
    subtitulo = paste(
      "Qu\u00e9 mide el error est\u00e1ndar y c\u00f3mo cambia con el tama\u00f1o",
      "de muestra \u2014 con simulaci\u00f3n de muestreo repetido (TLC)."
    ),
    datasets  = "Muestreo simulado desde datasets de ejemplo de StatBasics"
  )
}
mod_error_estandar_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

# ── Intervalos de confianza ─────────────────────────────────
mod_intervalos_confianza_ui <- function(id) {
  ns <- NS(id)
  proximamente_ui(
    icono     = "arrows-angle-contract",
    titulo    = "Intervalos de confianza",
    subtitulo = paste(
      "IC para medias y proporciones, con la simulaci\u00f3n cl\u00e1sica de",
      "\u00bfqu\u00e9 % de los IC contienen el par\u00e1metro real?"
    ),
    datasets  = "mtcars, iris y simulaciones de muestreo repetido"
  )
}
mod_intervalos_confianza_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

# ── Correlación ──────────────────────────────────────────────
mod_correlacion_ui <- function(id) {
  ns <- NS(id)
  proximamente_ui(
    icono     = "graph-up",
    titulo    = "Correlaci\u00f3n",
    subtitulo = paste(
      "Pearson, Spearman y Kendall con scatter interactivo",
      "\u2014 y por qu\u00e9 correlaci\u00f3n no es causalidad."
    ),
    datasets  = "mtcars, iris y datasets con relaciones no lineales"
  )
}
mod_correlacion_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

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
