## Code to prepare `iresa` datasets goes here

# Cargar librerías necesarias ----
library(tidyverse)
library(janitor)

# Leer datos raw (ajustar la ruta según la ubicación de tus datos originales) ----
iresa <- readxl::read_xlsx("data-raw\\data_iresa_review.xlsx")
# Por ahora asumimos que iresa_raw ya está cargado en el ambiente

# 1. Dataset principal: iresa ----
# Este dataset contiene todos los indicadores en formato ancho


#usethis::use_data(iresa, overwrite = TRUE)


# 2. Dataset: iresa_general ----
# Resumen de índice general y pilares (formato largo)

# Mapeo de nombres de pilares
mapa_pilares <- c(
  "CalidadAmbiental" = "Calidad ambiental",
  "GobernanzaAmbiental" = "Gobernanza Ambiental y gestión de riesgos",
  "IRESA" = "IRESA"
)

iresa_general <- iresa |>
  dplyr::select(Region, dplyr::matches("^(GobernanzaAmbiental|IRESA|CalidadAmbiental)")) |>
  tidyr::pivot_longer(
    cols = -Region,
    names_to = "name",
    values_to = "value"
  ) |>
  dplyr::mutate(
    indice_pilar = stringr::str_replace_all(name, "_", " ") |>
      stringr::word(1, 1),
    label = stringr::str_replace_all(name, "_", " ") |>
      stringr::word(2, 2),
    year = stringr::str_extract(name, "[0-9]{4}")
  ) |>
  dplyr::select(-name) |>
  # Reestructurar: una fila por índice/pilar/región/año
  tidyr::pivot_wider(
    names_from = c(label, year),
    values_from = value
  ) |>
  # Reordenar columnas
  dplyr::relocate(indice_pilar, .before = dplyr::everything()) |>
  janitor::clean_names() |>
  dplyr::mutate(
    indice_pilar = dplyr::recode(indice_pilar, !!!mapa_pilares)
  )
iresa_general
usethis::use_data(iresa_general, overwrite = TRUE)


# 3. Dataset: calidad_ambiental ----
# Indicadores del pilar Calidad Ambiental

# Indicadores del pilar
cal_amb <- c(
  "TratamientoAguasResiduales",
  "CalidadAire",
  "AccesoAguaSegura",
  "ConsumoEnergiasLimpias",
  "GeneracionResiduos",
  "DegradacionEcosistemas",
  "DisposicionResiduos"
)

# Mapeo de nombres descriptivos
mapa_indicadores_ca <- c(
  "TratamientoAguasResiduales" = "Tratamiento de aguas residuales",
  "CalidadAire" = "Calidad del aire",
  "AccesoAguaSegura" = "Acceso a agua segura",
  "ConsumoEnergiasLimpias" = "Consumo de energías limpias",
  "GeneracionResiduos" = "Generación total de residuos sólidos municipales ajustados por PBI",
  "DegradacionEcosistemas" = "Degradación de ecosistemas",
  "DisposicionResiduos" = "Disposición adecuada de residuos"
)

calidad_ambiental <- iresa |>
  dplyr::select(
    Region,
    dplyr::matches(paste0("^(", paste(cal_amb, collapse = "|"), ")"))
  ) |>
  tidyr::pivot_longer(
    cols = -Region,
    names_to = "name",
    values_to = "value"
  ) |>
  dplyr::mutate(
    indicador = stringr::str_replace_all(name, "_", " ") |>
      stringr::word(1, 1) |>
      dplyr::recode(!!!mapa_indicadores_ca),
    label = stringr::str_replace_all(name, "_", " ") |>
      stringr::word(2, 2),
    year = stringr::str_extract(name, "[0-9]{4}")
  ) |>
  dplyr::mutate(label = if_else(label == "Value", "score", label)) |>
  dplyr::select(-name) |>
  # Reestructurar
  tidyr::pivot_wider(
    names_from = c(label, year),
    values_from = value
  ) |>
  dplyr::relocate(indicador, .before = dplyr::everything()) |>
  janitor::clean_names()
calidad_ambiental
usethis::use_data(calidad_ambiental, overwrite = TRUE)


# 4. Dataset: gobernanza_gestion ----
# Indicadores del pilar Gobernanza Ambiental y gestión de riesgos

# Indicadores del pilar
gob_grd <- c(
  "ConflictosSocioambientales",
  "DelitosMineriaIlegal",
  "AreasVerdes",
  "AguaNoFacturada",
  "ViviendasInformales",
  "PerdidasIncendios",
  "EjecucionGasto"
)

# Mapeo de nombres descriptivos
mapa_indicadores_gg <- c(
  "ConflictosSocioambientales" = "Conflictos socioambientales activos",
  "DelitosMineriaIlegal" = "Delitos denunciados por minería ilegal",
  "AreasVerdes" = "Áreas verdes en espacios públicos conservadas por la municipalidad",
  "AguaNoFacturada" = "Agua no facturada",
  "ViviendasInformales" = "Viviendas informales",
  "PerdidasIncendios" = "Pérdidas por incendios forestales",
  "EjecucionGasto" = "Ejecución del gasto destinado a prevención y reducción de desastres"
)

gobernanza_gestion <- iresa |>
  dplyr::select(
    Region,
    dplyr::matches(paste0("^(", paste(gob_grd, collapse = "|"), ")"))
  ) |>
  tidyr::pivot_longer(
    cols = -Region,
    names_to = "name",
    values_to = "value"
  ) |>
  dplyr::mutate(
    indicador = stringr::str_replace_all(name, "_", " ") |>
      stringr::word(1, 1) |>
      dplyr::recode(!!!mapa_indicadores_gg),
    label = stringr::str_replace_all(name, "_", " ") |>
      stringr::word(2, 2),
    year = stringr::str_extract(name, "[0-9]{4}")
  ) |>
  dplyr::mutate(label = if_else(label == "Value", "score", label)) |>
  dplyr::select(-name) |>
  # Reestructurar
  tidyr::pivot_wider(
    names_from = c(label, year),
    values_from = value
  ) |>
  dplyr::relocate(indicador, .before = dplyr::everything()) |>
  janitor::clean_names()
gobernanza_gestion
usethis::use_data(gobernanza_gestion, overwrite = TRUE)

# 5. Dataset: mapa del peru ----
load("data\\peru_mapa.rda")
# 5. Dataset espacial: peru_mapa ----
# Mapa de las regiones del Perú para visualizaciones espaciales

# Asumiendo que peru_mapa_raw ya está cargado como sf object
# Si no, descomenta y ajusta la ruta:
# peru_mapa_raw <- sf::read_sf("data-raw/peru_regiones.shp")

peru_mapa <- peru_mapa |>
  janitor::clean_names() |>
  # Asegurar que los nombres de regiones coincidan con los datasets
  dplyr::mutate(
    region = dplyr::case_when(
      region == "Lima" & !stringr::str_detect(geometry, "Callao") ~ "Lima Provincias",
      region == "Lima" ~ "Lima*",
      TRUE ~ region
    )
  ) |>
  # Validar geometrías
  sf::st_make_valid() |>
  # Ordenar alfabéticamente
  dplyr::arrange(region)

# Verificar que todas las regiones estén presentes
stopifnot(nrow(peru_mapa) == 25)
stopifnot(all(sort(peru_mapa$region) == sort(iresa$region)))

usethis::use_data(peru_mapa, overwrite = TRUE)
