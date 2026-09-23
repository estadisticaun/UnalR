# Muestra de Microdatos de Graduados

Muestra de microdatos de la población de graduados de los programas
académicos en la Universidad Nacional de Colombia, se cuenta con una
muestra de la información disponible (*del 2019-I al 2021-I*). Dicho
dataset será usado en los ejemplos de las funciones
[`Tabla.General()`](https://estadisticaun.github.io/UnalR/reference/Tabla.General.md)
y
[`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md).

## Uso

``` r
ejGraduados
```

## Formato

Un data frame (*data.frame, tbl_df o tbl*) con 27.830 filas y 53
columnas: 'YEAR', 'SEMESTRE', 'TIPO_NIVEL', 'NIVEL', 'DEP_NAC',
'COD_DEP_NAC', 'CIU_NAC', 'COD_CIU_NAC', 'LON_CIU_NAC', 'LAT_CIU_NAC',
'DEP_PROC', 'COD_DEP_PROC', 'CIU_PROC', 'COD_CIU_PROC', 'LON_CIU_PROC',
'LAT_CIU_PROC', 'CODS_NAC', 'CODN_NAC', 'NACIONALIDAD', 'EDAD_MOD',
'CAT_EDAD', 'SEXO', 'ESTRATO_ORIG', 'ESTRATO', 'TIPO_COL', 'PBM_ORIG',
'PBM', 'MAT_PVEZ', 'DISCAPACIDAD', 'TIPO_DISC', 'SNIES_SEDE_ADM',
'SEDE_NOMBRE_ADM', 'SNIES_SEDE_MAT', 'SEDE_NOMBRE_MAT',
'ADM_PEAMA_ANDINA', 'MOD_ADM', 'TIPO_ADM', 'PAES', 'PEAMA', 'MOV_PEAMA',
'CONVENIO', 'TIP_CONVENIO', 'SNIESU_CONVENIO', 'U_CONVENIO', 'FACULTAD',
'FACULTAD_S', 'SNIES_PROGRA', 'PROGRAMA', 'PROGRAMA_S', 'AREAC_SNIES',
'CA_CINE', 'CD_CINE', 'AREA_CINE'.

## Fuente

Para obtener más detalle de los metadatos consulte
[aquí](https://estadisticaun.github.io/DabiertosUNAL/).

## Ejemplos

``` r
# library(dplyr)
ejGraduados[1:5, 1:10]
#> # A tibble: 5 × 10
#>    YEAR SEMESTRE TIPO_NIVEL NIVEL    DEP_NAC     COD_DEP_NAC CIU_NAC COD_CIU_NAC
#>   <dbl>    <dbl> <chr>      <chr>    <chr>             <dbl> <chr>         <dbl>
#> 1  2019        1 Pregrado   Pregrado SANTANDER            68 BUCARA…       68001
#> 2  2019        1 Pregrado   Pregrado VALLE DEL …          76 BUENAV…       76109
#> 3  2019        1 Pregrado   Pregrado CESAR                20 PELAYA        20550
#> 4  2019        1 Pregrado   Pregrado CAUCA                19 SANTAN…       19698
#> 5  2019        1 Pregrado   Pregrado ANTIOQUIA             5 CAREPA         5147
#> # ℹ 2 more variables: LON_CIU_NAC <dbl>, LAT_CIU_NAC <dbl>
```
