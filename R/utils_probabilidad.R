# ============================================================
# utils_probabilidad.R — Helper compartido: probabilidad y Bayes
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

# ── Teorema de Bayes con frecuencias naturales ──────────────
# Dado prevalencia (prior), sensibilidad P(+|enfermo) y
# especificidad P(-|sano), calcula P(enfermo|+) y devuelve el
# desglose en frecuencias naturales (de cada 10,000 personas),
# la forma m\u00e1s intuitiva de explicar Bayes (Gigerenzer).
bayes_frecuencias_naturales <- function(prevalencia, sensibilidad,
                                        especificidad, poblacion = 10000) {
  enfermos <- round(poblacion * prevalencia)
  sanos    <- poblacion - enfermos

  verdaderos_positivos <- round(enfermos * sensibilidad)
  falsos_negativos     <- enfermos - verdaderos_positivos
  falsos_positivos     <- round(sanos * (1 - especificidad))
  verdaderos_negativos <- sanos - falsos_positivos

  total_positivos <- verdaderos_positivos + falsos_positivos
  p_enfermo_dado_positivo <- if (total_positivos > 0) {
    verdaderos_positivos / total_positivos
  } else {
    NA
  }

  list(
    poblacion = poblacion,
    enfermos = enfermos,
    sanos = sanos,
    vp = verdaderos_positivos,
    fn = falsos_negativos,
    fp = falsos_positivos,
    vn = verdaderos_negativos,
    total_positivos = total_positivos,
    p_enfermo_dado_positivo = p_enfermo_dado_positivo
  )
}
