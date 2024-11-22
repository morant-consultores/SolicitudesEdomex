## code to prepare `shp` dataset goes here
library(sf)
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/SHP/2024/15 MEXICO/MUNICIPIO.shp"

shp <- read_sf(path) |>
  janitor::clean_names() |>
  mutate(nombre = if_else(nombre == "ACAMBAY DE RUIZ CASTAÃ\u0091EDA", "ACAMBAY", stringi::stri_trans_general(nombre, id = "latin-ascii")),
         nombre = if_else(nombre == "VALLE DE CHALCO SOLIDARIDAD", "VALLE DE CHALCO", nombre),
         nombre = if_else(nombre == "SOYANIQUILPAN DE JUAREZ", "SOYANIQUILPAN", nombre),
         municipio = sprintf("%03s", municipio)) |>
  st_transform(crs = st_crs(4326)) |>
  st_make_valid()

usethis::use_data(shp, overwrite = TRUE)
