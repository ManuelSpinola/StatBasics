# ============================================================
# utils_metricas.R — Helper compartido: tarjetas de estadísticos
# StatBasics · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Estandariza cómo se muestran las métricas en todos los módulos
# (tendencia central, dispersión, y los que sigan): cada tarjeta
# muestra "Etiqueta: valor" + una descripción corta en gris de
# qué significa esa métrica, para que el estudiante no tenga que
# volver a la pestaña "¿Qué es?" para recordarlo.
# ============================================================

# ── Diccionario de descripciones cortas por métrica ─────────
# Centralizado aquí para que el texto sea consistente si la misma
# métrica aparece en más de una pestaña o módulo.
descripciones_metricas <- list(
  media     = "Promedio aritm\u00e9tico de los datos; sensible a valores extremos.",
  mediana   = "Valor central al ordenar los datos de menor a mayor; robusta a outliers.",
  moda      = "Valor (o valores) que m\u00e1s se repite(n) en los datos.",
  asimetria = "Qu\u00e9 tan sim\u00e9trica es la distribuci\u00f3n; 0 = sim\u00e9trica, (+) cola a la derecha, (\u2212) cola a la izquierda.",
  rango     = "Diferencia entre el valor m\u00e1ximo y el m\u00ednimo; muy sensible a outliers.",
  varianza  = "Promedio de las distancias al cuadrado respecto a la media; queda en unidades al cuadrado.",
  sd        = "Ra\u00edz cuadrada de la varianza; en la misma unidad que los datos originales.",
  iqr       = "Rango entre el percentil 25 y el 75 (el 50% central de los datos); robusto a outliers.",
  cv        = "Desviaci\u00f3n est\u00e1ndar relativa a la media, en %; permite comparar dispersi\u00f3n entre variables de distinta escala."
)

# ── Constructor de una tarjeta de métrica ───────────────────
# etiqueta:    texto en negrita antes del valor (ej. "Media")
# valor:       valor ya formateado a mostrar (ej. round(x, 2))
# clave:       nombre en descripciones_metricas para buscar el texto
# sufijo:      texto opcional despu\u00e9s del valor (ej. "%")
# tipo:        color de alerta bootstrap (info, warning, secondary...)
# ultima:      TRUE quita el margen inferior (\u00faltima tarjeta del grupo)
tarjeta_metrica <- function(etiqueta, valor, clave, sufijo = "",
                             tipo = "info", ultima = FALSE) {
  descripcion <- descripciones_metricas[[clave]]
  div(
    class = paste0("alert alert-", tipo, " small py-2 px-3 ",
                   if (ultima) "mb-0" else "mb-1"),
    strong(etiqueta, ": "), valor, sufijo,
    if (!is.null(descripcion)) {
      tagList(
        tags$br(),
        tags$span(class = "text-muted", style = "font-size: 0.78rem;",
                  descripcion)
      )
    }
  )
}
