#' general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr highcharter leaflet sf
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
          card_header("Informes por municipio"),
          highcharter::highchartOutput(ns("g_1")),
          full_screen = T
        )
      ),
      layout_column_wrap(
        width = 1/2,
        height = 600,
        card(
          card_header("Historias en el tiempo"),
          highcharter::highchartOutput(ns("l_tiempo")),
          full_screen = T
        ),
        card(
          card_header("Acciones por categoría"),
          highcharter::highchartOutput(ns("g_2")),
          full_screen = T
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

    output$mapa <- leaflet::renderLeaflet({
      aux <- solicitudes |>
        count(municipio)

      shp <- shp |>
        left_join(aux, join_by(nombre == municipio))

      paleta <- colorRampPalette(c("#124559", "white", "#f77f00"))(10)
      domain <- c(min(aux$n, na.rm = T), max(aux$n, na.rm = T))
      pal <- colorNumeric(
        palette = paleta,
        domain = domain
      )

      leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles(provider = "CartoDB.Positron") |>
        addPolygons(data = shp,
                    weight = 1.5,
                    stroke = TRUE,
                    color = "white",  # Establece el color de las líneas en gris
                    fillColor = ~pal(n),
                    opacity = 0.3,
                    label = ~ nombre,
                    # popup = ~ label,
                    fillOpacity = 0.8) |>
        addLegend('bottomright', pal = pal, values = domain,
                  title = "Solicitudes") |>
        leaflet.extras::addFullscreenControl()

    })

    output$g_1 <- renderHighchart({
      solicitudes |>
        count(municipio, sort = T) |>
        head(15) |>
        hchart('bar', hcaes(x = municipio, y = n),
               color = "#f77f00", name = "Menciones") |>
        hc_xAxis(title = list(text = "Municipios")) |>
        hc_yAxis(title = list(text = "Menciones")) |>
        hc_title(text = "Top 15 municipios")
    })

    output$l_tiempo <- renderHighchart({
      hist_tiempo <- solicitudes |>
        mutate(fecha = as.Date(fecha)) |>
        count(fecha)

      hist_tiempo |>
        hchart("line",
               hcaes(x = fecha, y = n), color = "#e76f51", name = "Menciones") |>
        hc_tooltip(pointFormat = '{point.story}',
                   shared = FALSE,
                   useHTML = TRUE,
                   positioner = JS("function(labelWidth, labelHeight, point) {
                              return { x: 1, y: 30 };
                             }")) |>
        hc_yAxis(title = list(text = "Menciones"),
                 min = 0,  # Set your minimum value
                 max = max(hist_tiempo$n) + 50,  # Set your maximum value, adjust according to your data
                 endOnTick = FALSE, # Prevents the axis from extending beyond the set max
                 startOnTick = FALSE) |>
        hc_title(text = "Solicitudes a lo largo del tiempo") |>
        hc_add_theme(hc_theme_google())
    })

    output$g_2 <- renderHighchart({
      aux <- solicitudes |>
        count(etiqueta_1, sort = T) |>
        hchart('bar', hcaes(x = etiqueta_1, y = n),
               color = "#f77f00", name = "Menciones") |>
        hc_xAxis(title = list(text = "Acciones")) |>
        hc_yAxis(title = list(text = "Menciones")) |>
        hc_title(text = "Menciones por acción")

    })

  })
}

## To be copied in the UI
# mod_general_ui("general_1")

## To be copied in the server
# mod_general_server("general_1")
