#' general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_general_ui <- function(id){
  ns <- NS(id)
  tagList(
    layout_sidebar(
      border = FALSE,
      fill = FALSE,
      bg = "#f1faee",
      sidebar = sidebar(
        class = "bg_light",
        title = "Filtros",
        accordion(
          accordion_panel(
            "Variables",
            icon = bs_icon("sliders"),
            selectInput(ns("acciones"),
                        "Acciones",
                        choices = c("Todas" = "", sort(unique(solicitudes$etiqueta_1)))
            ),
            selectInput(ns("etiqueta_1"),
                        "Etiqueta 1",
                        choices = c("Todas" = "", sort(unique(solicitudes$etiqueta_2)))
            ),
            selectInput(ns("etiqueta_2"),
                        "Etiqueta 2",
                        choices = c("Todas" = "", sort(unique(solicitudes$etiqueta_3)))
            )
          ),
          accordion_panel(
            "Fecha",
            icon = bs_icon("geo-alt"),
            shinyWidgets::airDatepickerInput(
              inputId = ns("fecha"),
              label = "Seleccione fecha:",
              inline = TRUE,
              value = c(min(solicitudes$fecha), max(solicitudes$fecha)),
              minDate = min(solicitudes$fecha),
              maxDate = max(solicitudes$fecha),
              range = c(min(solicitudes$fecha), max(solicitudes$fecha)),
              language = "es",
              view = "months"
            ),
          )
        ),
        shinyWidgets::actionBttn(ns("filtrar"), "Filtrar",
                                 color = "primary", style = "simple",
                                 icon = bs_icon("filter"))
      ),
      layout_column_wrap(
        width = 1/2,
        height = 600,
          card(
            card_header("Mapa"),
            leaflet::leafletOutput(ns("mapa")),
            full_screen = T
          ),
        card(
          card_header("Informes por unidad"),
          highcharter::highchartOutput(ns("g_1"))
        )
      ),
      layout_column_wrap(
        width = 1/2,
        height = 400,
        card(
          card_header("Historias en el tiempo"),
          highcharter::highchartOutput(ns("l_tiempo")),
        ),
        card(
          card_header("Categorías más relevantes"),
          plotOutput(ns("g_3"))
        )
      )
    )
  )
}

#' general Server Functions
#'
#' @noRd
mod_general_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_general_ui("general_1")

## To be copied in the server
# mod_general_server("general_1")
