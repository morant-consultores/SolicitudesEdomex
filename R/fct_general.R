#' general
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

graficar_hchart <- function(bd, x_var, y_var, color = "#f77f00",
                            nombre_serie = "Menciones", titulo = "Gráfica",
                            titulo_x = "Eje X", titulo_y = "Eje Y") {
  bd |>
  hchart(
    type = "bar",
    hcaes(x = !!sym(x_var), y = !!sym(y_var)), # Evalúa las columnas dinámicamente
    color = color,
    name = nombre_serie
  ) |>
    hc_xAxis(title = list(text = titulo_x)) |>
    hc_yAxis(title = list(text = titulo_y)) |>
    hc_title(text = titulo)
}
