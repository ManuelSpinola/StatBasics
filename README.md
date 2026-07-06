# StatBasics

Aplicación Shiny (golem) didáctica e interactiva para aprender los
**conceptos fundamentales de la estadística**. Es la app de entrada del
ecosistema **StatSuite**, pensada para construir la intuición estadística
antes de pasar a modelos más avanzados en StatModels, StatML o StatBayes.

## Ecosistema StatSuite

| App | Enfoque |
|---|---|
| **StatBasics** | Conceptos básicos e interactivos (esta app) |
| [StatModels](https://github.com/ManuelSpinola/StatModels) | Modelos frecuentistas |
| StatML | Aprendizaje automático (`tidymodels`) |
| StatBayes | Modelos bayesianos (`brms`) |

## Módulos

| Bloque | Módulo | Estado |
|---|---|---|
| 1. Describiendo datos | Tendencia central (media, mediana, moda) | ✅ |
| 1. Describiendo datos | Dispersión (rango, varianza, sd, IQR, CV) | ✅ |
| 2. De la muestra a la población | Distribuciones de probabilidad | 🚧 próximamente |
| 2. De la muestra a la población | Error estándar del estadístico | 🚧 próximamente |
| 2. De la muestra a la población | Intervalos de confianza | 🚧 próximamente |
| 3. Relaciones entre variables | Correlación | 🚧 próximamente |
| 3. Relaciones entre variables | Probabilidad básica | 🚧 próximamente |

Cada módulo sigue el mismo patrón pedagógico:

1. **¿Qué es?** — teoría, fórmulas y cuándo usar cada medida
2. **Los datos** — datos de ejemplo, carga de datos propios (CSV/Excel), y
   ajuste de tipos de variable
3. **Explorar y calcular** — cálculo interactivo con gráficos y simulaciones
   (por ejemplo, agregar un valor atípico y ver en vivo cómo responde cada
   medida)
4. **Código R** — script reproducible descargable

## Instalación y ejecución local

```r
# Clonar el repositorio y luego, desde la raíz del proyecto:
install.packages("devtools")
devtools::install_deps()

# Correr la app en modo desarrollo
golem::run_dev()

# o simplemente:
shiny::runApp()
```

## Estructura del proyecto

Generado con [`{golem}`](https://thinkr-open.github.io/golem/):

```
R/
├── app_ui.R                  # Navbar principal
├── app_server.R               # Registro de servers de módulos
├── helpers.R                  # Paleta de colores y tema (bslib)
├── golem_utils.R              # Utilidades UI (proximamente_ui, etc.)
├── imports.R                  # Roxygen @import / @importFrom
├── mod_inicio.R                # Pantalla de bienvenida
├── mod_describiendo_datos.R    # Envoltorio: subtabs Tendencia central + Dispersión
├── mod_tendencia_central.R     # Media, mediana, moda
├── mod_dispersion.R            # Rango, varianza, sd, IQR, CV
└── mod_placeholders.R          # Módulos pendientes (próximamente)
```

## Despliegue

Pensado para desplegarse en **Posit Connect Cloud**, igual que el resto de
apps de StatSuite.

## Autor

**Manuel Spínola** — ICOMVIS, Universidad Nacional (UNA), Costa Rica

## Licencia

MIT — ver el archivo [`LICENSE`](LICENSE).
