# Los datos correspondientes a los archivos .dbf, .shp y .shx por departamentos y municipios
# de Colombia (en su versión del año 2020) fue extraída del DANE mediante el siguiente enlace:
# https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-mgn-marco-geoestadistico-nacional/
library(tidyverse)
library(usethis)
library(readxl)
library(sf)
library(sp)
library(rmapshaper)

# ----------------------------------------------------------------------- #
# ---------------------------- DEPARTAMENTOS ---------------------------- #
# ----------------------------------------------------------------------- #
if (file.exists("data-raw/MGN_DPTO_POLITICO.shp")) {
  Depto <- sf::st_read("data-raw/MGN_DPTO_POLITICO.shp", quiet = TRUE)
  Temp  <- tibble("Code_Dept"    = as.numeric(Depto$DPTO_CCDGO),
                  "Departamento" = Depto$DPTO_CNMBR)

  Departamentos_SHP <- sf::as_Spatial(Depto$geom)
  print(object.size(Departamentos_SHP), units = "Mb")
  Depto_Simplified <- rmapshaper::ms_simplify(Departamentos_SHP, keep = 0.05, keep_shapes = TRUE)
  print(object.size(Depto_Simplified), units = "Mb")

  Depto_Final <- sp::SpatialPolygonsDataFrame(Depto_Simplified, Temp)
} else { print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'MGN_DPTO_POLITICO.shp'") }

# ----------------------------------------------------------------------- #
# ----------------------------- MUNICIPIOS ------------------------------ #
# ----------------------------------------------------------------------- #
if (file.exists("data-raw/MGN_MPIO_POLITICO.shp")) {
  Mpio <- sf::st_read("data-raw/MGN_MPIO_POLITICO.shp", quiet = TRUE)
  Temp  <- tibble("Code_Dept"    = as.numeric(Mpio$DPTO_CCDGO),
                  "Code_Mun"     = as.numeric(Mpio$MPIO_CCNCT),
                  "Departamento" = Mpio$DPTO_CNMBR,
                  "Municipio"    = Mpio$MPIO_CNMBR)

  Municipios_SHP <- sf::as_Spatial(Mpio$geom)
  print(object.size(Municipios_SHP), units = "Mb")
  Mpio_Simplified <- rmapshaper::ms_simplify(Municipios_SHP, keep = 0.01, keep_shapes = TRUE)
  print(object.size(Mpio_Simplified), units = "Mb")

  Mpio_Final <- sp::SpatialPolygonsDataFrame(Mpio_Simplified, Temp)
} else { print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'MGN_MPIO_POLITICO.shp'") }

# Lectura de la codificación de la División Político-administrativa de Colombia (Divipola) 2020
# https://geoportal.dane.gov.co/servicios/descarga-y-metadatos/descarga-historicos-de-codificacion-divipola/
# ----------------------------------------------------------------------- #
# ------------------------------ DIVIPOLA ------------------------------- #
# ----------------------------------------------------------------------- #
if (file.exists("data-raw/DIVIPOLA_2021.xls")) {
  DIVIPOLA <- read_excel("data-raw/DIVIPOLA_2021.xls")
  Cabeceras <- DIVIPOLA %>%
    select(Code_Dept    = `Código departamento`,
           Code_Mun     = `Código municipio`,
           Departamento = `Nombre departamento`,
           Municipio    = `Nombre municipio`,
           Tipo_Centro  = `Tipo centro poblado`,
           Longitud     = Longitud,
           Latitud      = Latitud) %>%
    mutate(Code_Dept = as.numeric(Code_Dept),
           Code_Mun = as.numeric(Code_Mun)) %>%
    filter(Tipo_Centro == "CABECERA MUNICIPAL")
} else { print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'DIVIPOLA_2021.xls'") }

# ----------------------------------------------------------------------- #
# ------------------------------- ISO3166 ------------------------------- #
# ----------------------------------------------------------------------- #
ISO3166     <- maps::iso3166
CountryCode <- giscoR::gisco_countrycode


use_data(Depto_Final, Mpio_Final, Cabeceras, ISO3166, CountryCode,
         internal = TRUE, overwrite = TRUE, compress = "xz"
         )
