library(usethis)
library(readr)

if (file.exists("data-raw/ejConsolidadoSaberPro2019.csv")) {
  ejConsolidadoSaberPro2019 <- read_csv("data-raw/ejConsolidadoSaberPro2019.csv")
} else {
  print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'ejConsolidadoSaberPro2019.csv'")
}

use_data(ejConsolidadoSaberPro2019, overwrite = TRUE, compress = "xz")
