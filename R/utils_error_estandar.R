# ============================================================
# utils_error_estandar.R — Helper compartido: error estándar
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

# ── Desviación estándar poblacional (÷N, no ÷N-1) ────────────
# Necesaria cuando el vector x representa TODOS los casos
# posibles (ej. las 10 medias muestrales de una enumeración
# completa) y no una muestra de un universo más grande.
sd_poblacional <- function(x) {
  x <- x[!is.na(x)]
  sqrt(mean((x - mean(x))^2))
}

# ── Error estándar por fórmula clásica ──────────────────────
# SE = s / sqrt(n)
se_formula <- function(x, n = length(x)) {
  stats::sd(x) / sqrt(n)
}

# ── Corrección para poblaciones finitas ──────────────────────
# Cuando la muestra es una fracción considerable de la
# población (no infinita), el EE debe corregirse:
# EE = (s/raiz(n)) x raiz((N-n)/(N-1))
# Sin esta corrección, el EE se sobreestima cuando n/N no es
# despreciable (ej.: muestrear 30 de 50 animales).
se_poblacion_finita <- function(s, n, N) {
  (s / sqrt(n)) * sqrt((N - n) / (N - 1))
}

# ── Ejemplo de enumeración completa (población pequeña) ──────
# Población de ejemplo: peso (kg) de las últimas 5 dantas
# centroamericanas (Tapirus bairdii) adultas que quedan en el
# Parque Nacional Tapantí (escenario hipotético).
pesos_5_dantas_ee <- c(165, 180, 172, 190, 168)

# Genera TODAS las combinaciones posibles de tamaño n de una
# población pequeña x, con la media y desviación estándar de
# cada una — replica el ejercicio de "5 ratones, muestras de
# tamaño 2" para mostrar que la media muestral es insesgada
# pero la DE muestral no lo es exactamente.
# Genera TODAS las combinaciones posibles de tamaño 2 de una
# población pequeña x — devuelve columnas separadas (no un
# string combinado), igual que la tabla del PDF de clase.
tabla_enumeracion_completa <- function(x) {
  combs <- utils::combn(x, 2)
  data.frame(
    individuo_a    = combs[1, ],
    individuo_b    = combs[2, ],
    media_muestral = apply(combs, 2, mean),
    de_muestral    = apply(combs, 2, stats::sd)
  )
}

# ── Distribución muestral de la media por bootstrap ─────────
# Remuestrea x (con reemplazo) `reps` veces, tomando muestras de
# tamaño n cada vez, y devuelve el vector de sus medias. Esto es
# la aproximación empírica a "¿qué tanto variaría la media si
# repitiera el muestreo muchas veces?" sin depender de ninguna
# fórmula teórica.
bootstrap_medias <- function(x, n, reps) {
  replicate(reps, mean(sample(x, size = n, replace = TRUE)))
}
