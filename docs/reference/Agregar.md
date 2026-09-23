# Cree el consolidado a partir de los microdatos con los que dispone, agrupando por las variables temporales que considere necesarias

Esta función permite transformar desde la base de microdatos para
obtener el consolidado final con el cual trabaja la mayoría de las
funciones gráficas disponibles en `UnalR`.

## Uso

``` r
Agregar(
  datos,
  formula,
  frecuencia,
  intervalo,
  textNA = "Sin Información",
  ask = FALSE
)
```

## Argumentos

- datos:

  Un data frame con los microdatos.

- formula:

  Fórmula en la que el primer componente especificado (*antes del
  \\\sim\\*) hace referencia a la(s) variable(s) de interés, y el
  segundo componente (*luego del \\\sim\\*) a la(s) variable(s)
  temporales por las cuales se quiere agrupar (*separadas por cualquier
  operador como `+`, `*`, `:`, etc.*).

- frecuencia:

  Vector o lista (*dependiendo de la cantidad de variables temporales
  especificadas*) numérica con los periodos que debería tener cada una
  de éstas.

- intervalo:

  Vector o lista (*dependiendo de la cantidad de variables temporales
  especificadas*) numérica que contiene los períodos de inicio y fin con
  los cuales se quiere realizar el filtro.

- textNA:

  Cadena de caracteres indicando el nombre que se dará a los registros
  cuando éstos presenten algún dato faltante (*en la variable
  seleccionada más no en las variables temporales especificadas*). El
  valor por defecto es "Sin Información".

- ask:

  Si es `TRUE` (*valor predeterminado*) mostrará un mensaje por consola
  preguntándole al usuario cuál periodo desea que sea el último por
  considerar (*esperando una respuesta por consola*). Si previamente se
  ha introducido el argumento `intervalo` éste quedará inhabilitado y no
  se ejecutará.

## Valor

Un data frame o tibble perteneciente a las clases "tbl_df", "tbl" y
"data.frame".

## Ejemplos

``` r
if (FALSE) { # all(FALSE)
# library(readxl)
df1 <- read_excel(read_example("TestConsolidado1.xlsx"))
df2 <- read_excel(read_example("TestConsolidado2.xlsx"))

# Seleccione para cada dataset (A, B, C y D) una de las opciones que se le muestran
# en consola (1, 2, 3 y 4) respectivamente.
Agregar(
  formula = TIPO_NIVEL ~ ANO + PERIODO,
  frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
) -> A
Agregar(
  formula = TIPO_NIVEL ~ ANO + PERIODO,
  frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
) -> B
Agregar(
  formula = TIPO_NIVEL ~ ANO + PERIODO,
  frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
) -> C
Agregar(
  formula = TIPO_NIVEL ~ ANO + PERIODO,
  frecuencia = list("Year" = 2009:2013, "Period" = 1:3), df1
) -> D
all.equal(C, D)

G <- Agregar(
  formula    = TIPO_NIVEL ~ ANO + PERIODO,
  frecuencia = list("Year" = 2009:2013, "Period" = 1:2),
  intervalo  = list(c(2009, 1), c(2013, 1)),
  datos      = df1
)
H <- Agregar(
  formula    = TIPO_NIVEL ~ ANO + PERIODO,
  frecuencia = list("Year" = 2009:2013, "Period" = 1:2),
  datos      = df1,
  ask        = FALSE
)
all.equal(G, H)

# Observe las diferentes opciones que se le muestran por consola.
Agregar(formula = SEXO ~ YEAR + PERIODO, frecuencia = list("Year" = 2016:2021, "Period" = 1:3), df2)

Agregar(
  formula    = SEXO ~ YEAR + PERIODO,
  frecuencia = list("Year" = 2016:2021, "Period" = 1:3),
  intervalo  = list(c(2018, 1), c(2020, 3)),
  datos      = df2
)
Agregar(
  formula    = SEXO ~ YEAR,
  frecuencia = 2016:2021,
  intervalo  = c(2018, 2020),
  datos      = df2
)

# Observe que puede especificar más de una variable para realizar el agregado
# Se harán agregados simples y se unirán las filas en un único df
df1$SEXO <- c("Hombre", NA, NA, NA, NA, "Hombre", "Hombre",
              "Mujer", NA, NA, "Hombre", "Mujer", "Hombre", "Mujer"
              )
Agregar(
  formula    = TIPO_NIVEL + SEXO ~ ANO + PERIODO,
  frecuencia = list("Year" = 2010:2013, "Period" = 1:2),
  intervalo  = list(c(2010, 1), c(2013, 2)),
  datos      = df1
) -> A
Agregar(
  formula    = TIPO_NIVEL + SEXO ~ ANO + PERIODO,
  frecuencia = list("Year" = 2010:2013, "Period" = 1:2),
  datos      = df1
) -> B
}
```
