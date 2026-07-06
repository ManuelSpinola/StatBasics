# ============================================================
# utils_distribuciones.R — Helper compartido: ajuste y
# comparación de distribuciones candidatas (fitdistrplus)
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

# Nombres legibles <-> nombres de distribución de fitdistrplus/stats
etiquetas_distribuciones <- c(
  norm    = "Normal",
  unif    = "Uniforme",
  exp     = "Exponencial",
  lnorm   = "Lognormal",
  gamma   = "Gamma",
  weibull = "Weibull"
)

# ── Candidatas plausibles según el rango de los datos ───────
# Distribuciones que requieren valores estrictamente positivos
# (lognormal, gamma, weibull, exponencial) solo se ofrecen si
# todos los valores de x son > 0.
candidatas_disponibles <- function(x) {
  base <- c("norm", "unif")
  if (all(x > 0)) base <- c(base, "exp", "lnorm", "gamma", "weibull")
  base
}

# ── Ajustar varias distribuciones candidatas por MLE ────────
# Devuelve una lista nombrada de objetos fitdist (fitdistrplus),
# omitiendo silenciosamente las que fallen al converger.
ajustar_candidatas <- function(x, candidatas) {
  fits <- list()
  for (d in candidatas) {
    ajuste <- tryCatch(
      fitdistrplus::fitdist(x, d),
      error = function(e) NULL
    )
    if (!is.null(ajuste)) {
      fits[[etiquetas_distribuciones[[d]]]] <- ajuste
    }
  }
  fits
}

# ── Tabla comparativa de criterios de información ───────────
# Ordenada de mejor (menor AIC) a peor ajuste.
tabla_criterios_ajuste <- function(fits) {
  filas <- lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    data.frame(
      Distribucion = nm,
      logLik = round(f$loglik, 2),
      AIC    = round(f$aic, 2),
      BIC    = round(f$bic, 2)
    )
  })
  df <- do.call(rbind, filas)
  df[order(df$AIC), ]
}
