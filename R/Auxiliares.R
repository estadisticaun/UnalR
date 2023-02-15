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
br2addline <- function(x) { gsub("<br>", "\n", x) }
theme_DNPE <- function() {
  font <- "Ancizar Sans Light"
  # hrbrthemes::theme_ipsum()
  theme_minimal() %+replace%
    theme(
      # Elementos de la Grilla (grid)
      # @ El tema padre ya elimina las líneas del eje (no es necesario hacerlo de nuevo)
      # panel.grid.major = element_blank(), # Quita las líneas de cuadrícula principal
      # panel.grid.minor = element_blank(), # Quita las líneas de cuadrícula menores
      # axis.ticks = element_blank(),       # Quita las líneas del axis ticks

      # Elementos de Texto
      plot.title = element_text(
        family = font   , # Familia de la fuente
        face   = "bold" , # Tipografía en negrita
        colour = "black", # Color de la fuente
        size   = 20     , # Tamaño de la fuente
        hjust  = 0.5    , # Centrando horizontalmente
        vjust  = 2        # Ligeramente elevado
      ),
      plot.subtitle = element_text(
        family = font     ,
        face   = "bold.italic",
        colour = "#94B43B",
        size   = 14       ,
        hjust  = 0.5
      ),
      plot.caption = element_text(
        family = font     ,
        face   = "italic" ,
        colour = "#94B43B",
        size   = 9        ,
        hjust  = 1
      ),
      plot.tag = element_text(
        family = font     ,
        face   = "plain"  ,
        colour = "#94B43B",
        size   = 9        ,
        vjust  = 2
      ),
      axis.title = element_text(
        family = font   ,
        colour = "black",
        size   = 12
      )
    )
}