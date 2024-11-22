## code to prepare `solicitudes` dataset goes here
library(tidyverse)

solicitudes <- readxl::read_excel("data-raw/base_solicitudes.xlsm") |>
  janitor::clean_names() |>
  filter(!clave_del_municipio %in% c("N/A", "0"), !grepl("-", municipio)) |>
  mutate(municipio = stringi::stri_trans_general(municipio, id = "latin-ascii"),
         across(contains("etiqueta"), ~toupper(gsub("_", " ", .x))))

usethis::use_data(solicitudes, overwrite = TRUE)
