library(usethis)
library(dplyr)
library(stringr)
library(UnalData)

# Creación del dataset llamado "ejGraduados" -----------------------------------
ejGraduados <- UnalData::Graduados %>%
  select(!c(ID, TID)) %>% filter(YEAR %in% 2019:2021)

# Creación del dataset llamado "ejConsolidadoGrad" -----------------------------
data <- UnalData::Graduados %>% mutate(TOTAL = "TOTAL")
temp <- function(x) {
  df <- Agregar(
    formula    = x,
    frecuencia = list("Year" = 2009:2021, "Period" = 1:2),
    intervalo  = list(c(2009, 1), c(2021, 1)),
    datos      = data
  )
  return(df)
}

ejConsolidadoGrad <- bind_rows(
  temp(TIPO_NIVEL ~ YEAR + SEMESTRE), temp(NIVEL ~ YEAR + SEMESTRE),
  temp(SEDE_NOMBRE_ADM ~ YEAR + SEMESTRE), temp(NACIONALIDAD ~ YEAR + SEMESTRE),
  temp(SEXO ~ YEAR + SEMESTRE), temp(AREAC_SNIES ~ YEAR + SEMESTRE),
  temp(TOTAL ~ YEAR + SEMESTRE)
) %>% mutate(Clase = stringr::str_to_title(Clase))

# Creación del dataset llamado "ejMiniConsolidadoAsp" --------------------------
ejMiniConsolidadoAsp <- bind_rows(
  Agregar(formula = DISCAPACIDAD ~ YEAR + SEMESTRE,
          frecuencia = list("Year" = 2008:2021, "Period" = 1:2),
          ask = FALSE, datos = UnalData::Aspirantes
          ),
  Agregar(formula = TIPO_DISC ~ YEAR + SEMESTRE,
          frecuencia = list("Year" = 2008:2021, "Period" = 1:2),
          ask = FALSE, datos = UnalData::Aspirantes
          )
)

# Creación del dataset llamado "ejMiniAspirantesPre" ---------------------------
Puntaje <- UnalData::Aspirantes %>%
  filter(TIPO_NIVEL == "Pregrado", YEAR >= 2009) %>%
  mutate(Serie = factor(paste(YEAR, SEMESTRE, sep = "-"))) %>%
  select(Serie, PTOTAL, TIPO_INS, INS_SEDE_NOMBRE, ADM_SEDE_NOMBRE, FACULTAD) %>%
  replace_na(list(TIPO_INS = "Sin Información"))

Split       <- split(Puntaje, Puntaje$TIPO_INS)
GroupSizes  <- vapply(Split, nrow, integer(1))
SampledObs  <- mapply(sample, GroupSizes, c(10000, 10000, 10000, 50))
Get_Rows    <- function(df, rows) df[rows, , drop = FALSE]
ejMiniAspirantesPre <- do.call(rbind, mapply(Get_Rows, Split, SampledObs, SIMPLIFY = FALSE))

# Creación del dataset llamado "ejSaberPro2020" --------------------------------
ejSaberPro2020 <- UnalData::SaberPro %>% filter(YEAR == 2020) %>% select(!c(ID, TID))


# Create package data ----------------------------------------------------------
use_data(ejGraduados         , overwrite = TRUE, compress = "bzip2")
use_data(ejConsolidadoGrad   , overwrite = TRUE, compress = "xz")
use_data(ejMiniConsolidadoAsp, overwrite = TRUE, compress = "xz")
use_data(ejMiniAspirantesPre , overwrite = TRUE, compress = "xz")
use_data(ejSaberPro2020      , overwrite = TRUE, compress = "bzip2")
