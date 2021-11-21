#' Obtenga la ruta de los archivos de Excel para la función `Agregar()`
#'
#' UnalR viene con algunos archivos de ejemplo en su directorio `inst/extdata`.
#' Esta función facilita el acceso a ellos.
#'
#' @param ruta Nombre del archivo. Si es `NULL`, se enumerarán los archivos de
#'   ejemplo.
#' @export
read_example <- function(ruta = NULL) {
  if (is.null(ruta)) {
    dir(system.file("extdata", package = "UnalR"))
  } else {
    system.file("extdata", ruta, package = "UnalR", mustWork = TRUE)
  }
}
