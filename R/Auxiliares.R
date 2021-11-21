#' @import highcharter
#' @importFrom rlang quo_name
'%NotIN%' <- Negate('%in%')
PocentRelativo <- function(x) {
  TotPercent <- function(m){ m*100/sum(m, na.rm = TRUE) }
  RowPorcent <- round( t(apply(x, MARGIN = 1, FUN = TotPercent)), 1 )
  return(as.data.frame(RowPorcent))
}
cv <- function(x, na.rm = TRUE) {
  return( sd(x, na.rm = na.rm)/abs(mean(x, na.rm = na.rm)) )
}
vars2vec <- function(quosure) {
  Abc <- NULL
  for (i in 1:length(quosure)) { Abc <- c(Abc, rlang::quo_name(quosure[[i]])) }
  return(Abc)
}
Spanish.Highcharter <- function() {

  # https://api.highcharts.com/highcharts/lang
  # https://stackoverflow.com/questions/25266392/how-to-set-highchart-global-options-in-r
  # https://es.stackoverflow.com/questions/318416/cómo-cambiar-el-idioma-del-menú-de-opciones-desplegable-en-highcharts
  lang <- getOption("highcharter.lang")

  lang$contextButtonTitle <- "Men\u00fa Contextual del Gr\u00e1fico"
  lang$viewFullscreen     <- "Ver en pantalla completa"
  lang$printChart   <- "Imprimir gr\u00e1fico"
  lang$downloadPNG  <- "Descargar imagen PNG"
  lang$downloadJPEG <- "Descargar imagen JPEG"
  lang$downloadPDF  <- "Descargar documento PDF"
  lang$downloadSVG  <- "Descargar imagen vectorial SVG"
  lang$downloadCSV  <- "Descargar CSV"
  lang$downloadXLS  <- "Descargar XLS"
  lang$viewData     <- "Ver tabla de datos"
  lang$loading      <- "Cargando..."
  lang$noData       <- "No hay informaci\u00f3n para mostrar"
  lang$drillUpText  <- "<< Volver a {series.name}"

  options(highcharter.lang = lang)
}
