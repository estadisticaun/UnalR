library(usethis)
library(readxl)

if (file.exists("data-raw/Graduados.xlsx")) {
  Graduados <- read_excel("data-raw/Graduados.xlsx")
} else { print("En la carpeta de datos sin procesar (data-raw) no existe el archivo 'Graduados.xlsx'") }

use_data(Graduados, overwrite = TRUE, compress = "xz")
