# ============================================================
# utils_intervalos_confianza.R — Helper compartido: IC
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

# ── Intervalo de confianza para una media (basado en t) ─────
# Devuelve una lista con media, se, margen de error, y los
# límites inferior/superior, usando la distribución t con
# n-1 grados de libertad (correcto para cualquier n).
ic_media_t <- function(x, conf = 0.95) {
  x   <- x[!is.na(x)]
  n   <- length(x)
  m   <- mean(x)
  se  <- stats::sd(x) / sqrt(n)
  gl  <- n - 1
  tval   <- stats::qt(1 - (1 - conf) / 2, df = gl)
  margen <- tval * se
  list(
    media  = m,
    se     = se,
    gl     = gl,
    tval   = tval,
    margen = margen,
    li     = m - margen,
    ls     = m + margen
  )
}
