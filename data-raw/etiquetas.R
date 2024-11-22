library(dplyr)

bd <- read.csv("data-raw/etiquetas.csv") |>
  janitor::clean_names() |>
  filter(!etiqueta_3 %in% c("N/A", "Otra"))

readr::write_rds(bd, "data/etiquetas.rda")
