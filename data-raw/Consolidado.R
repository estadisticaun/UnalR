library(usethis)
library(readr)

if (file.exists("data-raw/Consolidado.csv")) {
  Consolidado <- read_csv("data-raw/Consolidado.csv")
} else { print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'Consolidado.csv'") }

use_data(Consolidado, overwrite = TRUE, compress = "xz")
