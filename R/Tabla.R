#' Cree fácilmente un widget para visualización de tablas HTML usando el paquete `DT`
#'
#' Esta función está diseñada para facilitar la creación de tablas para informes y
#' publicaciones produciendo un widget HTML para visualizar un data frame utilizando
#' el paquete `DT`. La forma en que esta función maneja las cosas por usted significa
#' que a menudo no tiene que preocuparse por los pequeños detalles para obtener un
#' resultado impresionante y listo para usar.
#'
#' @param datos Un data frame.
#' @param categoria Una variable categórica dentro del data frame ingresado en `datos`.
#' @param encabezado Cadena de caracteres que describe los distintos niveles de la
#'   variable `categoria`.
#' @param leyenda Cadena de caracteres que describe información adicional de la tabla,
#'   ésta se sitúa en la parte inferior de la tabla de manera centrada, dicho texto
#'   se visualizará en todas las opciones de descarga. Su valor por defecto es `NULL`.
#' @param tituloPdf Cadena de caracteres que proporciona un título a la tabla al
#'   momento de generar el `.pdf` como al darle al botón de imprimir. Su valor por
#'   defecto es el introducido en el argumento `encabezado`.
#' @param mensajePdf Cadena de caracteres que proporciona un mensaje situado entre
#'   el título y la tabla. Se visualizará tanto al generar el `.pdf` como al darle
#'   al botón de imprimir.
#' @param ajustarNiveles Si es `TRUE` (valor predeterminado) se buscará optimizar
#'   el espacio entre las columnas, colocando todos los nombres de las columnas de
#'   forma horizontal y eliminado al máximo el espacio entre éstas.
#' @param colorHead Cadena de caracteres que indica el color de fondo de la cabecera
#'   de la tabla. Puede indicar el color con el nombre (`"red"`), código hexadecimal
#'   (`"#FF0000"`) o RGB (`rgb(1, 0, 0)`). El valor por defecto es "blanco" (`"#FFFFFF"`).
#' @param colorear Si es `TRUE` indica si desea poner color a la tabla de forma
#'   automática, aplicando color a la fuente del periodo y añadiendo un color de
#'   fondo para los años (*de acuerdo con una paleta predefinida*). El valor por
#'   defecto es `FALSE`.
#' @param estilo Una lista compuesta por dos parámetros:
#'   * `PaletaYear`: Vector de caracteres que específica los colores de fondo para
#'     los años.
#'   * `PaletaSemestre`: Vector de caracteres que específica los colores de fuente
#'     para los periodos (\emph{semestres}).
#'
#' @details
#' Esta función se basa enteramente del paquete `DT`, el cual proporciona una interfaz
#' para `R` a la biblioteca `DataTables` de `JavaScript`. Los data frames de `R` se
#' pueden mostrar como tablas en páginas HTML, proporcionando opciones de filtrado,
#' paginación, clasificación y muchas otras características en las tablas.
#'
#' @return
#' Retorna la tabla creada mediante `DT` la cual pertenece a la clase "datatables" y "htmlwidget".
#'
#' @examples
#' Colorido <- c("#F9CA00", "#AEF133", "#19C89B", "#FF7F50", "#D99588", "#FF4C94",
#'               "#19EE9F", "#9F61F6", "#F24467", "#F68118", "#F9CA00", "#AEF133")
#' Tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM",
#'       encabezado = "TOTAL DE ESTUDIANTES POR SEDE DE GRADUACI\u00d3N",
#'       titulo  = "ESTUDIANTES GRADUADOS POR SEDE",
#'       leyenda = "Distribuci\u00f3n de estudiantes admitidos (desde el 2009-I al 2020-I) por sede.",
#'       colorHead = "#8CC63F",
#'       estilo    = list(PaletaYear = Colorido, PaletaSemestre = c("#7E1BC3", "#C31576"))
#'       )
#'
#' @export
#'
#' @import DT
#' @import dplyr
#' @importFrom htmltools withTags tag
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#' @importFrom methods missingArg
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
Tabla <- function(datos, categoria, encabezado = "Encabezados de los Niveles de la Categor\u00eda",
                  leyenda = NULL, tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE,
                  colorHead = "#FFFFFF", colorear = FALSE, estilo) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("\u00a1Por favor introduzca una categor\u00eda que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!is.logical(ajustarNiveles)) {
    stop("\u00a1El argumento 'ajustarNiveles' debe ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.character(colorHead)) {
    stop("\u00a1El argumento 'colorHead' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
  }
  if (!is.logical(colorear)) {
    stop("\u00a1El argumento 'colorear' debe ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (is.null(leyenda) == FALSE) {
    leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                       "Tabla : ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")

  thead <- function(...) { htmltools::tag("thead", ...) }
  th <- function(...) { htmltools::tag("th", ...) }
  tr <- function(...) { htmltools::tag("tr", ...) }

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  DataFrame <- datos %>%
    # Convertir a columnas las observaciones dispersas en múltiples filas
    filter(Variable == categoria) %>%
    pivot_wider(names_from = Clase, values_from = Total) %>%
    select(-Variable) %>%
    # Creación de la columna Total Global (Total por Fila/Semestre)
    left_join(
      datos %>%
        filter(Variable == categoria) %>%
        group_by(YEAR, SEMESTRE) %>%
        summarise(TotalGlobal = sum(Total, na.rm = TRUE))
    ) %>%
    mutate(
      YEAR = factor(YEAR),
      SEMESTRE = factor(SEMESTRE)
    )
  Categorias <- datos %>%
    filter(Variable == categoria) %>%
    group_by(Clase) %>% distinct(Clase)
  # Custom Table Container (Nombre de los Encabezados)
  sketch = htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, "A\u00f1o"),
        th(rowspan = 2, "Periodo"),
        th(colspan = n_groups(Categorias), encabezado),
        th(rowspan = 2, "Total")
      ),
      tr( lapply(Categorias %>% pull(), th) )
    )
  ))

  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class      = AjusteNiveles,
    rownames   = FALSE,
    container  = sketch,
    caption    = leyenda,
    filter     = list(position = "top", clear = TRUE, plain = FALSE),
    extensions = c("Buttons", "KeyTable"),
    options    = list(autoWidth  = TRUE,
                      columnDefs = list(list(className = "dt-center", targets = 0:(n_groups(Categorias)+2)),
                                        list(width = "65px", targets = 0)),
                      pageLength = 8,
                      order = list(list(0, "desc"), list(1, "desc")),
                      dom   = "Bfrtip",
                      keys  = TRUE,
                      searchHighlight = TRUE,
                      scrollX = TRUE,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color':", paste0("'", colorHead, "'"), ", 'color': '#000000'});","}"),
                      language = list(
                        processing     = "Procesando...",
                        lengthMenu     = "Mostrar _MENU_ registros",
                        zeroRecords    = "No se encontraron resultados",
                        emptyTable     = "Ning\u00fan dato disponible en esta tabla",
                        info           = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                        infoEmpty      = "Mostrando registros del 0 al 0 de un total de 0 registros",
                        infoFiltered   = "(filtrado de un total de _MAX_ registros)",
                        infoPostFix    = "",
                        search         = "Buscar:",
                        url            = "",
                        infoThousands  = ",",
                        loadingRecords = "Cargando...",
                        paginate = list(
                          first    = "Primero",
                          last     = "\u00daltimo",
                          `next`   = "Siguiente",
                          previous = "Anterior"
                        ),
                        aria = list(
                          sortAscending  = "Activar para ordenar la columna de manera ascendente",
                          sortDescending = "Activar para ordenar la columna de manera descendente"
                        )
                      ),
                      buttons = list(list(extend = "copy", text = "Copiar"), "csv", "excel",
                                     list(extend = "pdf", pageSize = "A4", filename = "pdf",
                                          message = mensajePdf, title = tituloPdf),
                                     list(extend = "print", text = "Imprimir", pageSize = "A4",
                                          message = mensajePdf, title = tituloPdf))
    )
  )

  if (colorear && missingArg(estilo)) {
    TablaFinal <- TablaFinal %>%
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), colorRampPalette(brewer.pal(12, "Set3"))(nlevels(DataFrame$YEAR)) )
      ) %>%
      formatStyle(
        "SEMESTRE", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$SEMESTRE), c("#E72837", "#0A7DBF") )
      )
  } else if (!missingArg(estilo)) {
    TablaFinal <- TablaFinal %>%
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), estilo$PaletaYear )
      ) %>%
      formatStyle(
        "SEMESTRE", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$SEMESTRE), estilo$PaletaSemestre )
      )
  }

  return(TablaFinal)
}
