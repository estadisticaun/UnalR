# Mini Consolidado de Aspirantes

Mini consolidado de la población de aspirantes a cursar estudios de
pregrado o postgrado en la Universidad Nacional de Colombia inscritos a
través de convocatoria pública de manera regular o por medio de los
programas de admisión especial existentes -*sólo pregrado*-. Dicho
dataset será usado en los ejemplos de la función
[`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md).

## Uso

``` r
ejMiniConsolidadoAsp
```

## Formato

Un data frame (*data.frame, tbl_df o tbl*) con 308 filas y 5 columnas:
'Variable', 'YEAR', 'SEMESTRE', 'Clase', 'Total'.

## Fuente

Para obtener más detalle de los metadatos consulte
[aquí](https://estadisticaun.github.io/DabiertosUNAL/).

## Ejemplos

``` r
# library(dplyr)
head(ejMiniConsolidadoAsp)
#> # A tibble: 6 × 5
#>   Variable      YEAR SEMESTRE Clase Total
#>   <chr>        <dbl>    <dbl> <fct> <int>
#> 1 DISCAPACIDAD  2008        1 No    62646
#> 2 DISCAPACIDAD  2008        1 Sí        0
#> 3 DISCAPACIDAD  2008        2 No    40040
#> 4 DISCAPACIDAD  2008        2 Sí        0
#> 5 DISCAPACIDAD  2009        1 No    62035
#> 6 DISCAPACIDAD  2009        1 Sí       36
```
