# Microdatos Saber Pro 2020

Microdatos de los resultados de la prueba Saber Pro del año 2020,
obtenidos desde la página del ICFES, los cuales serán usados para los
ejemplos de las funciones
[`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md)
y
[`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md).

## Uso

``` r
ejSaberPro2020
```

## Formato

Un data frame (*data.frame, tbl_df o tbl*) con 4.896 filas y 59
columnas: 'YEAR', 'SEMESTRE', 'YEAR_MAT', 'SEMESTRE_MAT', 'TIPO_NIVEL',
'NIVEL', 'DEP_NAC', 'COD_DEP_NAC', 'CIU_NAC', 'COD_CIU_NAC',
'LON_CIU_NAC', 'LAT_CIU_NAC', 'DEP_PROC', 'COD_DEP_PROC', 'CIU_PROC',
'COD_CIU_PROC', 'LON_CIU_PROC', 'LAT_CIU_PROC', 'CODS_NAC', 'CODN_NAC',
'NACIONALIDAD', 'EDAD_MOD', 'CAT_EDAD', 'SEXO', 'ESTRATO_ORIG',
'ESTRATO', 'TIPO_COL', 'PBM_ORIG', 'PBM', 'SNIES_SEDE_ADM',
'SEDE_NOMBRE_ADM', 'SNIES_SEDE_MAT', 'SEDE_NOMBRE_MAT',
'ADM_PEAMA_ANDINA', 'MOD_ADM', 'TIPO_ADM', 'PAES', 'PEAMA', 'FACULTAD',
'FACULTAD_S', 'SNIES_PROGRA', 'PROGRAMA', 'PROGRAMA_S', 'AREAC_SNIES',
'CA_CINE', 'CD_CINE', 'AREA_CINE', 'SNP', 'PUNTAJE_GLOBAL',
'PUNT_COMP_CIUD', 'NIVEL_COMP_CIUD', 'PUNT_COMU_ESCR',
'NIVEL_COMU_ESCR', 'PUNT_INGLES', 'NIVEL_INGLES', 'PUNT_LECT_CRIT',
'NIVEL_LECT_CRIT', 'PUNT_RAZO_CUANT', 'NIVEL_RAZO_CUANT'.

## Fuente

Para obtener más detalle de los metadatos consulte
[aquí](https://estadisticaun.github.io/DabiertosUNAL/).

## Ejemplos

``` r
# library(dplyr)
ejSaberPro2020[1:5, 1:10]
#> # A tibble: 5 × 10
#>    YEAR SEMESTRE YEAR_MAT SEMESTRE_MAT TIPO_NIVEL NIVEL    DEP_NAC COD_DEP_NAC
#>   <dbl>    <dbl>    <dbl>        <dbl> <chr>      <chr>    <chr>         <dbl>
#> 1  2020       NA     2020            1 Pregrado   Pregrado NA               NA
#> 2  2020       NA     2020            1 Pregrado   Pregrado NA               NA
#> 3  2020       NA     2020            1 Pregrado   Pregrado NA               NA
#> 4  2020       NA     2020            1 Pregrado   Pregrado NA               NA
#> 5  2020       NA     2020            1 Pregrado   Pregrado NA               NA
#> # ℹ 2 more variables: CIU_NAC <chr>, COD_CIU_NAC <dbl>
```
