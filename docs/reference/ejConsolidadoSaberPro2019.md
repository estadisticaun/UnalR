# Consolidado Saber Pro 2019

Consolidado de ejemplo de la prueba Saber Pro (*antes llamada ECAES*)
del año 2019. El cual se utiliza para los ejemplos de la función
[`Tabla.SaberPro()`](https://estadisticaun.github.io/UnalR/reference/Tabla.SaberPro.md).

## Uso

``` r
ejConsolidadoSaberPro2019
```

## Formato

Un data frame (*data.frame, tbl_df o tbl*) con 42 filas y 7 columnas:
'Variable', 'YEAR', 'Clase', 'n', 'Componente', 'Total', 'desv'.

## Fuente

Para obtener más detalle de los metadatos consulte
[aquí](https://estadisticaun.github.io/DabiertosUNAL/).

## Ejemplos

``` r
# library(dplyr)
head(ejConsolidadoSaberPro2019)
#> # A tibble: 6 × 7
#>   Variable  YEAR Clase      n Componente   Total  desv
#>   <chr>    <dbl> <chr>  <dbl> <chr>        <dbl> <dbl>
#> 1 sede      2019 Bogotá  3146 Cuantitativo  189.  27.8
#> 2 sede      2019 Bogotá  3146 Lectura       188.  25.6
#> 3 sede      2019 Bogotá  3146 Competencias  183.  29.9
#> 4 sede      2019 Bogotá  3146 Inglés        193.  26.8
#> 5 sede      2019 Bogotá  3146 Comunicación  163.  33.1
#> 6 sede      2019 Bogotá  3146 Global        183.  18.0
```
