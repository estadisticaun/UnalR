#' Guarde y presente un widget HTML renderizado como una imagen estática
#'
#' Esta función permite representar un widget HTML como un objeto ráster
#' (*imagen de mapa de bits*), útil para reproducir gráficos interactivos en
#' archivos estáticos como un `.pdf` generado por `R Markdown`. Esta función usa
#' internamente los paquetes `webshot`, `htmlwidgets`, `png` y `grid` para poder
#' llevar a cabo su propósito.
#'
#' @param HTML_Widget Widget HTML a ser mostrado de forma estática.
#' @param Height Altura de la imagen estática a retornar.
#' @param PrimeraVez Si es `FALSE` (valor predeterminado) no se instalará `PhantomJS`.
#'   Éste es necesario instalarlo una única vez, por lo cual si es la primera vez
#'   que corre la función deberá indicar el argumento con el valor `TRUE`, después
#'   de esto omita este argumento y deje su valor por defecto.
#' @param ... Otros parámetros concernientes a la función [webshot()][webshot::webshot()],
#'   sin considerar los ya usados dentro de la función (`url`, `file`, `delay`, `zoom` y `vheight`).
#'
#' @details
#' No es necesario especificar el número del factor de zoom. El factor de zoom por
#' defecto es 5, el cual dará como resultado cinco veces más de píxeles vertical y
#' horizontalmente. Este valor fue seleccionado debido a que es el óptimo, un valor
#' mayor ocasiona un tiempo de ejecución excesivamente alto y poca ganancia en cuanto
#' a calidad, con un valor menor se tiene una pérdida considerable de calidad.
#'
#' Si se especifican tanto el ancho como el alto, es probable que la imagen se distorsione.
#' Por lo tanto, el único argumento variable será la altura de la imagen, dejando el
#' ancho como un argumento adaptativo dependiendo del ancho disponible.
#'
#' El tiempo de espera antes de tomar una captura de pantalla, en segundos, es de
#' \eqn{1s}. Este valor debido a que se necesita un retraso mayor para que los grafico
#' generados por `Highcharter` se muestren correctamente.
#'
#' @return
#' Una imagen estática.
#'
#' @examples
#' \dontrun{
#' col <- c("#29ABE2", "#8CC63F", "#CC241D",
#'          "#FF3673", "#0071BC", "#F15A24",
#'          "#FBB03B", "#93278F", "#8A381A")
#' A <- Plot.Series(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col)
#' StaticPlot(A)
#'
#' ano <- 2020; semestre <- 2
#' col <- c("#116BEE", "#E62272")
#' B <- Plot.Torta(datos     = Consolidado,
#'                 categoria = "SEXO",
#'                 ano       = ano,
#'                 periodo   = semestre,
#'                 colores   = col,
#'                 titulo    = "DISTRIBUCI\u00d3N DE GRADUADOS POR SEXO",
#'                 libreria  = "highcharter")
#' StaticPlot(B)
#' }
#'
#' @export
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom png readPNG
#' @importFrom grid grid.raster
StaticPlot <- function(HTML_Widget, Height = 500, PrimeraVez = FALSE, ...) {

  # https://stackoverflow.com/questions/57581268/embed-plotly-into-pdf-rmarkdown
  if (PrimeraVez) { webshot::install_phantomjs() }

  htmlwidgets::saveWidget(widget = HTML_Widget, file = "Temp_Plot.html")
  webshot::webshot(url = "Temp_Plot.html", file = "Temp_Plot.png", delay = 1, zoom = 5, vheight = Height, ...)

  StaticImage <- png::readPNG(source = "Temp_Plot.png")
  file.remove("Temp_Plot.html"); file.remove("Temp_Plot.png")

  return(grid::grid.raster(StaticImage))
}
