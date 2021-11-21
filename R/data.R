#' Consolidado de Graduados
#'
#' Consolidado de la población de graduados de los programas académicos en la
#' Universidad Nacional de Colombia, cuenta con la información histórica desde
#' el 2009-I al 2021-I.
#' @format Un data frame (*data.frame, tbl_df o tbl*) con `r nrow(ejConsolidadoGrad)`
#' filas y `r ncol(ejConsolidadoGrad)` columnas:
#' `r paste0("``", colnames(ejConsolidadoGrad), "``", collapse = ", ")`.
#' @source Para obtener más detalle de los metadatos consulte \href{https://n9.cl/8qk3y}{aquí}.
#' @examples
#' if (require("dplyr")) {
#'   head(ejConsolidadoGrad)
#' }
"ejConsolidadoGrad"


#' Muestra de Microdatos de Graduados
#'
#' Muestra de microdatos de la población de graduados de los programas académicos
#' en la Universidad Nacional de Colombia, se cuenta con una muestra de la información
#' disponible (*del 2019-I al 2021-I*). Dicho dataset será usado en los ejemplos
#' de las funciones `Tabla.General()` y `Plot.Treemap()`.
#' @format Un data frame (*data.frame, tbl_df o tbl*) con `r nrow(ejGraduados)`
#' filas y `r ncol(ejGraduados)` columnas:
#' `r paste0("``", colnames(ejGraduados), "``", collapse = ", ")`.
#' @source Para obtener más detalle de los metadatos consulte \href{https://n9.cl/8qk3y}{aquí}.
#' @examples
#' ejGraduados[1:5, 1:10]
"ejGraduados"


#' Consolidado Saber Pro 2019
#'
#' Consolidado de ejemplo de la prueba Saber Pro (*antes llamada ECAES*) del año
#' 2019. El cual se utiliza para los ejemplos de la función `Tabla.SaberPro()`.
#' @format Un data frame (*data.frame, tbl_df o tbl*) con `r nrow(ejConsolidadoSaberPro2019)`
#' filas y `r ncol(ejConsolidadoSaberPro2019)` columnas:
#' `r paste0("``", colnames(ejConsolidadoSaberPro2019), "``", collapse = ", ")`.
#' @source Para obtener más detalle de los metadatos consulte \href{https://n9.cl/c08al}{aquí}.
#' @examples
#' if (require("dplyr")) {
#'   head(ejConsolidadoSaberPro2019)
#' }
"ejConsolidadoSaberPro2019"


#' Microdatos Saber Pro 2020
#'
#' Microdatos de los resultados de la prueba Saber Pro del año 2020, obtenidos
#' desde la página del ICFES, los cuales serán usados para los ejemplos de las
#' funciones `Plot.Boxplot()` y `Plot.Radar()`.
#' @format Un data frame (*data.frame, tbl_df o tbl*) con `r nrow(ejSaberPro2020)`
#' filas y `r ncol(ejSaberPro2020)` columnas:
#' `r paste0("``", colnames(ejSaberPro2020), "``", collapse = ", ")`.
#' @source Para obtener más detalle de los metadatos consulte \href{https://n9.cl/c08al}{aquí}.
#' @examples
#' ejSaberPro2020[1:5, 1:10]
"ejSaberPro2020"


#' Mini Consolidado de Aspirantes
#'
#' Mini consolidado de la población de aspirantes a cursar estudios de pregrado
#' o postgrado en la universidad nacional de Colombia inscritos a través de
#' convocatoria pública de manera regular o por medio de los programas de admisión
#' especial existentes -*sólo pregrado*-. Dicho dataset será usado en los ejemplos
#' de la función `Plot.Drilldown()`.
#' @format Un data frame (*data.frame, tbl_df o tbl*) con `r nrow(ejMiniConsolidadoAsp)`
#' filas y `r ncol(ejMiniConsolidadoAsp)` columnas:
#' `r paste0("``", colnames(ejMiniConsolidadoAsp), "``", collapse = ", ")`.
#' @source Para obtener más detalle de los metadatos consulte \href{https://n9.cl/4hoq}{aquí}.
#' @examples
#' if (require("dplyr")) {
#'   head(ejMiniConsolidadoAsp)
#' }
"ejMiniConsolidadoAsp"


#' Mini Microdatos de Aspirantes a Pregrado
#'
#' Muestra de los microdatos (*únicamente estudiantes de pregrado*) de la población
#' de aspirantes a cursar estudios de pregrado o postgrado en la universidad
#' nacional de Colombia inscritos a través de convocatoria pública de manera regular
#' o por medio de los programas de admisión especial existentes. Dicho dataset
#' será usado en los ejemplos de la función `Plot.Boxplot()`.
#' @format Un data frame (*data.frame, tbl_df o tbl*) con `r nrow(ejMiniAspirantesPre)`
#' filas y `r ncol(ejMiniAspirantesPre)` columnas:
#' `r paste0("``", colnames(ejMiniAspirantesPre), "``", collapse = ", ")`.
#' @source Para obtener más detalle de los metadatos consulte \href{https://n9.cl/4hoq}{aquí}.
#' @examples
#' if (require("dplyr")) {
#'   head(ejMiniAspirantesPre)
#' }
"ejMiniAspirantesPre"
