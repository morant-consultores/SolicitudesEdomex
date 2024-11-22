#' general
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

graficar_hchart <- function(bd, x, y){
  bd |>
  hchart('bar', hcaes(x = {{x}}, y = {{y}}),
         color = "#f77f00", name = "Menciones") |>
    hc_xAxis(title = list(text = "Municipios")) |>
    hc_yAxis(title = list(text = "Menciones")) |>
    hc_title(text = "Top 15 municipios")
}
