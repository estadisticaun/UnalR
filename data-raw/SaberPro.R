library(usethis)
library(readr)

if (file.exists("data-raw/SaberPro.csv")) {
  SaberPro <- read_csv("data-raw/SaberPro.csv")
} else { print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'SaberPro.csv'") }

use_data(SaberPro, overwrite = TRUE, compress = "xz")
