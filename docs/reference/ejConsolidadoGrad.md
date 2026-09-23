# Consolidado de Graduados

Consolidado de la población de graduados de los programas académicos en
la Universidad Nacional de Colombia, cuenta con la información histórica
desde el 2009-I al 2021-I.

## Uso

``` r
ejConsolidadoGrad
```

## Formato

Un data frame (*data.frame, tbl_df o tbl*) con 775 filas y 5 columnas:
'Variable', 'YEAR', 'SEMESTRE', 'Clase', 'Total'.

## Fuente

Para obtener más detalle de los metadatos consulte
[aquí](https://estadisticaun.github.io/DabiertosUNAL/).

## Ejemplos

``` r
# library(dplyr)
head(ejConsolidadoGrad)
#> # A tibble: 6 × 5
#>   Variable    YEAR SEMESTRE Clase     Total
#>   <chr>      <dbl>    <dbl> <chr>     <int>
#> 1 TIPO_NIVEL  2009        1 Postgrado  1498
#> 2 TIPO_NIVEL  2009        1 Pregrado   3458
#> 3 TIPO_NIVEL  2009        2 Postgrado   908
#> 4 TIPO_NIVEL  2009        2 Pregrado   2200
#> 5 TIPO_NIVEL  2010        1 Postgrado  1478
#> 6 TIPO_NIVEL  2010        1 Pregrado   3795
```
