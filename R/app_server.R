#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_inicio_server("inicio")
  mod_describiendo_datos_server("describiendo_datos")
  mod_distribuciones_server("distribuciones")
  mod_error_estandar_server("error_estandar")
  mod_intervalos_confianza_server("intervalos_confianza")
  mod_correlacion_server("correlacion")
  mod_pruebas_hipotesis_server("pruebas_hipotesis")
  mod_tamano_efecto_server("tamano_efecto")
  mod_probabilidad_server("probabilidad")
}
