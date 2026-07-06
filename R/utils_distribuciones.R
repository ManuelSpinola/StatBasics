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
  weibull = "Weibull",
  beta    = "Beta",
  pois    = "Poisson",
  nbinom  = "Binomial Negativa"
)

# ── ¿Es una variable de conteos? ─────────────────────────────
# Entera y no negativa (0, 1, 2, ...). Si es TRUE, se compara
# contra Poisson y Binomial Negativa en vez de las continuas.
es_discreta_conteo <- function(x) {
  all(x >= 0) && all(abs(x - round(x)) < 1e-8)
}

# ── Resumen de dispersión para datos de conteo ──────────────
# Índice de dispersión = varianza / media. Poisson asume que
# ambas son iguales (índice \u2248 1); un índice >> 1 indica
# sobredispersión, que es justo lo que motiva usar Binomial
# Negativa en vez de Poisson (el mismo razonamiento que aplica
# en los modelos GLM/GLMM de StatModels y StatBayes).
resumen_dispersion_conteo <- function(x) {
  m <- mean(x)
  v <- stats::var(x)
  list(media = m, varianza = v, indice = v / m)
}

# ── Candidatas plausibles según el rango de los datos ───────
# Distribuciones que requieren valores estrictamente positivos
# (lognormal, gamma, weibull, exponencial) solo se ofrecen si
# todos los valores de x son > 0. Beta requiere que TODOS los
# valores est\u00e9n estrictamente entre 0 y 1 (la distribuci\u00f3n
# Beta no est\u00e1 definida en los extremos 0 y 1 exactos).
candidatas_disponibles <- function(x) {
  if (es_discreta_conteo(x)) {
    return(c("pois", "nbinom"))
  }
  base <- c("norm", "unif")
  if (all(x > 0)) base <- c(base, "exp", "lnorm", "gamma", "weibull")
  if (all(x > 0 & x < 1)) base <- c(base, "beta")
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
