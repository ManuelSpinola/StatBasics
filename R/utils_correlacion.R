# ============================================================
# utils_correlacion.R — Helper compartido: correlación
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

# ── IC para un coeficiente de correlación de Pearson ────────
# Usa la transformación z de Fisher (r no se distribuye normal,
# pero z sí, aproximadamente) — el método estándar para
# construir un IC de r.
ic_correlacion_pearson <- function(r, n, conf = 0.95) {
  z_r    <- atanh(r)                     # transformación de Fisher
  se_z   <- 1 / sqrt(n - 3)
  zcrit  <- stats::qnorm(1 - (1 - conf) / 2)
  margen <- zcrit * se_z
  list(
    r      = r,
    li     = tanh(z_r - margen),
    ls     = tanh(z_r + margen),
    se_z   = se_z
  )
}

# ── Interpretación cualitativa de la fuerza de r ────────────
interpretar_fuerza_r <- function(r) {
  ar <- abs(r)
  if (ar < 0.1)      "muy d\u00e9bil / nula"
  else if (ar < 0.3) "d\u00e9bil"
  else if (ar < 0.5) "moderada"
  else if (ar < 0.7) "fuerte"
  else               "muy fuerte"
}
