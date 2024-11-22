#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib bsicons
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "Informes",
      fluid = T,
      fillable = FALSE,
      theme = bs_theme(font_scale = NULL, primary = "#e63946", secondary = "#457b9d",
                       success = "#97DDC2", info = "#66A8D6", warning = "#DEB46E",
                       danger = "#DE787B", preset = "zephyr"),
      nav_spacer(),
      nav_panel("General", mod_general_ui("general_1"), icon = bs_icon("house"))
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SolicitudesEdomex"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
