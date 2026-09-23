# Mini Microdatos de Aspirantes a Pregrado

Muestra de los microdatos (*únicamente estudiantes de pregrado*) de la
población de aspirantes a cursar estudios de pregrado o postgrado en la
Universidad Nacional de Colombia inscritos a través de convocatoria
pública de manera regular o por medio de los programas de admisión
especial existentes. Dicho dataset será usado en los ejemplos de la
función
[`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md).

## Uso

``` r
ejMiniAspirantesPre
```

## Formato

Un data frame (*data.frame, tbl_df o tbl*) con 30.050 filas y 6
columnas: 'Serie', 'PTOTAL', 'TIPO_INS', 'INS_SEDE_NOMBRE',
'ADM_SEDE_NOMBRE', 'FACULTAD'.

## Fuente

Para obtener más detalle de los metadatos consulte
[aquí](https://estadisticaun.github.io/DabiertosUNAL/).

## Ejemplos

``` r
# library(dplyr)
head(ejMiniAspirantesPre)
#> # A tibble: 6 × 6
#>   Serie  PTOTAL TIPO_INS INS_SEDE_NOMBRE ADM_SEDE_NOMBRE FACULTAD               
#>   <fct>   <dbl> <chr>    <chr>           <chr>           <chr>                  
#> 1 2013-1   519. PAES     NA              NA              NA                     
#> 2 2023-1   415. PAES     Bogotá          NA              NA                     
#> 3 2020-2   448. PAES     Palmira         Palmira         Ingeniería y administr…
#> 4 2022-1   329. PAES     NA              NA              NA                     
#> 5 2014-2   716. PAES     NA              NA              NA                     
#> 6 2021-1   461. PAES     Medellín        NA              NA                     
```
