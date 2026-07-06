#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = tagList(
        img(src = "www/hexsticker_StatBasics.png", height = "38px",
            style = "margin-right: 8px; vertical-align: middle;"),
        "StatBasics"
      ),
      theme        = tema_app,
      id           = "navbar_principal",
      window_title = "StatBasics \u00b7 StatSuite",
      fillable     = TRUE,
      footer = div(
        class = "text-center small py-2",
        style = paste0("background:", colores$primario, "; color: white;"),
        "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("house", class = "me-1"), "Inicio"),
        mod_inicio_ui("inicio")
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("bar-chart-steps", class = "me-1"),
                        "Describiendo los datos"),
        mod_describiendo_datos_ui("describiendo_datos")
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("distribute-vertical", class = "me-1"),
                        "Distribuciones"),
        mod_distribuciones_ui("distribuciones")
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("rulers", class = "me-1"),
                        "Error est\u00e1ndar"),
        mod_error_estandar_ui("error_estandar")
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("arrows-angle-contract", class = "me-1"),
                        "Intervalos de confianza"),
        mod_intervalos_confianza_ui("intervalos_confianza")
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("graph-up", class = "me-1"),
                        "Correlaci\u00f3n"),
        mod_correlacion_ui("correlacion")
      ),

      bslib::nav_panel(
        title = tagList(bsicons::bs_icon("dice-5", class = "me-1"),
                        "Probabilidad b\u00e1sica"),
        mod_probabilidad_ui("probabilidad")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "StatBasics"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
