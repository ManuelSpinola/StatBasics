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
  cv        = "Desviaci\u00f3n est\u00e1ndar relativa a la media, en %; permite comparar dispersi\u00f3n entre variables de distinta escala.",
  error_estandar = "Cu\u00e1nto var\u00eda la media muestral de muestra en muestra; disminuye al aumentar el tama\u00f1o de muestra (\u221an).",
  se_teorico     = "EE = \u03c3/\u221an, usando la desviaci\u00f3n est\u00e1ndar POBLACIONAL real (conocida aqu\u00ed porque simulamos toda la poblaci\u00f3n) \u2014 lo que esperar\u00edas por f\u00f3rmula si conocieras \u03c3 con exactitud.",
  se_observado   = "La desviaci\u00f3n est\u00e1ndar de las medias que realmente se obtuvieron al simular \u2014 lo que se observa en la pr\u00e1ctica. Se acerca al esperado por f\u00f3rmula, pero no es id\u00e9ntico por variabilidad de Monte Carlo.",
  se_formula     = "Estimado con EE = s/\u221an, usando la desviaci\u00f3n est\u00e1ndar de TU MUESTRA (s) como aproximaci\u00f3n de la poblacional \u2014 con datos reales nunca conoces \u03c3 con certeza.",
  se_bootstrap   = "Estimado remuestreando tus propios datos con reemplazo miles de veces y midiendo cu\u00e1nto var\u00edan esas medias \u2014 sin asumir ninguna f\u00f3rmula."
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

# ============================================================
# Infografía: paso numerado + conector
# Patrón reutilizable para explicar un concepto como una
# secuencia visual de pasos (tarjeta con color, ícono y número),
# en vez de párrafos de texto plano. Pensado para usarse en
# cualquier módulo de StatSuite donde convenga contar una idea
# paso a paso (ej. población \u2192 muestra \u2192 distribuci\u00f3n muestral).
# ============================================================

# ── Tarjeta de un paso ───────────────────────────────────────
# numero:  texto/n\u00famero que va en el c\u00edrculo (ej. 1, 2, "A")
# icono:   nombre de \u00edcono de bsicons (ej. "collection")
# color:   color de fondo del encabezado (ej. colores$primario)
# titulo:  t\u00edtulo corto del paso
# ...:     contenido del cuerpo de la tarjeta (cualquier UI)
paso_infografia <- function(numero, icono, color, titulo, ...) {
  div(
    style = "border-radius:14px; overflow:hidden; margin-bottom:4px;
             box-shadow:0 2px 10px rgba(0,0,0,0.10);",
    div(
      style = paste0("background:", color, "; color:#ffffff;",
                     "padding:14px 18px; display:flex;",
                     "align-items:center; gap:12px;"),
      div(style = paste0("background:#ffffff; color:", color, ";",
                        "border-radius:50%; width:34px; height:34px;",
                        "min-width:34px; display:flex;",
                        "align-items:center; justify-content:center;",
                        "font-weight:800; font-size:1.05rem;"),
          numero),
      bs_icon(icono, style = "font-size:1.25rem;"),
      h5(class = "mb-0", style = "font-weight:700;", titulo)
    ),
    div(style = "padding:18px; background:#ffffff;", ...)
  )
}

# ── Flecha conectora entre pasos ─────────────────────────────
conector_infografia <- function(texto = NULL) {
  div(
    class = "text-center",
    style = "margin: 4px 0 12px 0;",
    bs_icon("arrow-down-circle-fill",
            style = "color:#adb5bd; font-size:1.7rem;"),
    if (!is.null(texto)) {
      p(class = "mb-0 mt-1", style = "font-weight:700; font-size:0.95rem;",
        texto)
    }
  )
}

# ── Par de "números grandes" (estilo infografía) ─────────────
# Muestra dos estadísticos uno junto al otro, SIN usar ningún
# sistema de columnas/grid (layout_columns, fluidRow, flexbox)
# \u2014 solo dos bloques "inline-block" que fluyen como texto normal,
# uno al lado del otro. No puede apilarse porque no hay filas ni
# columnas involucradas, solo texto en l\u00ednea.
par_numeros_grandes <- function(etiqueta1, valor1, etiqueta2, valor2, color) {
  grupo <- function(etiqueta, valor) {
    tags$div(
      style = "display:inline-block; text-align:center; padding:0 30px; vertical-align:top;",
      tags$div(class = "small text-muted mb-1", etiqueta),
      tags$div(style = paste0("font-size:1.7rem; font-weight:700; color:",
                             color, ";"),
               valor)
    )
  }
  tags$div(
    style = paste0("background:", colores$fondo,
                  "; border-radius:8px; padding:14px; text-align:center;"),
    grupo(etiqueta1, valor1), grupo(etiqueta2, valor2)
  )
}
# ── Tarjeta de concepto (igual estilo, sin número) ───────────
# Para presentar ideas en paralelo (no secuenciales) con el
# mismo lenguaje visual: encabezado de color + \u00edcono + t\u00edtulo.
tarjeta_concepto <- function(icono, color, titulo, ...) {
  div(
    style = "border-radius:14px; overflow:hidden; margin-bottom:4px;
             box-shadow:0 2px 10px rgba(0,0,0,0.10);",
    div(
      style = paste0("background:", color, "; color:#ffffff;",
                     "padding:12px 16px; display:flex;",
                     "align-items:center; gap:10px;"),
      bs_icon(icono, style = "font-size:1.2rem;"),
      h6(class = "mb-0", style = "font-weight:700;", titulo)
    ),
    div(style = "padding:16px; background:#ffffff;", ...)
  )
}
