#' Cree fácilmente un widget para visualización de tablas HTML usando el paquete `DT`
#'
#' Esta función está diseñada para facilitar/simplificar la creación/producción de tablas para informes, presentaciones y
#' publicaciones, produciendo un widget HTML para visualizar un data frame
#' utilizando el paquete `DT`. La forma en que esta función maneja las cosas por
#' usted significa que a menudo no tiene que preocuparse por los pequeños detalles
#' para obtener un resultado impresionante y listo para usar.
#'
#' @param df Un data frame.
#' @param rows Una variable categórica dentro del data frame ingresado en `datos`.
#' @param pivotCat X.
#' @param pivotVar X.
#' @param columnNames Vector de caracteres que especifica los nombres de las columnas
#'   de la tabla a retornar. Si no se introduce algún valor se tomará el mismo
#'   nombre de las columnas presentes en `datos`.
#' @param filtros Si es `FALSE` (*valor predeterminado*) no se habilitará/aplicará
#'   los filtros por columna. Establézcalo en `TRUE` si desea generar filtros de
#'   columna automáticamente.
#' @param colFilters Vector numérico que especifica las columnas a las cuales les
#'   desea agregar la opción de poder filtrar. Si no se introduce algún valor todas
#'   las columnas tendrán habilitada la opción de poder filtrar.
#' @param estadistico X.
#' @param encabezado Cadena de caracteres que describe los distintos niveles de
#'   la variable `categoria`.
#' @param leyenda Cadena de caracteres que describe información adicional de la
#'   tabla, ésta se sitúa en la parte inferior de la tabla de manera centrada,
#'   dicho texto se visualizará en todas las opciones de descarga. Su valor por
#'   defecto es `NULL`.
#' @param tituloPdf Cadena de caracteres que proporciona un título a la tabla al
#'   momento de generar el `.pdf` como al hacer clic al botón de imprimir. Su valor
#'   por defecto es el introducido en el argumento `encabezado`.
#' @param mensajePdf Cadena de caracteres que proporciona un mensaje situado entre
#'   el título y la tabla. Se visualizará tanto al generar el `.pdf` como al
#'   hacer clic al botón de imprimir.
#' @param ajustarNiveles Si es `TRUE` (*valor predeterminado*) se buscará optimizar
#'   el espacio entre las columnas, colocando todos los nombres de las columnas de
#'   forma horizontal y eliminando al máximo el espacio entre éstas.
#' @param scrollX Si es `TRUE` (*valor predeterminado*) se habilitará la propiedad
#'   Scroller para el eje X. Tenga presente que cuando su df contiene muchas columnas
#'   es de utilidad (*pues no permite que se salga la tabla por ancho*), sin embargo,
#'   asegúrese de desactivarlo cuando presente pocas columnas, pues se verá un
#'   desplazamiento de los encabezados debido a un conflicto interno.
#' @param colorHead Cadena de caracteres que indica el color de fondo de la cabecera
#'   de la tabla. Puede indicar el color con el nombre (`"red"`), código hexadecimal
#'   (`"#FF0000"`) o RGB (`rgb(1, 0, 0)`). El valor por defecto es "blanco" (`"#FFFFFF"`).
#' @param estilo Una lista compuesta por listas las cuales en su interior contiene
#'   argumentos válidos de la función [formatStyle()][DT:: formatStyle()], esto
#'   con la finalidad de que pueda aplicar estilos CSS a la tabla, tales como color
#'   de la fuente, color de fondo, tamaño de fuente, etc. Puede encontrar mayor
#'   información de los argumentos disponibles \href{https://rstudio.github.io/DT/functions.html}{aquí}.
#'
#' @details
#' Esta función se basa enteramente del paquete `DT`, el cual proporciona una
#' interfaz para `R` a la biblioteca `DataTables` de `JavaScript`. Los data frames
#' de `R` se pueden mostrar como tablas en páginas HTML, proporcionando opciones
#' de filtrado, paginación, clasificación y muchas otras características en las
#' tablas.
#'
#' Esta función se basa enteramente del paquete `DT`, el cual proporciona una interfaz
#' para `R` a la biblioteca `DataTables` de `JavaScript`. Los data frames de `R`
#' se pueden mostrar como tablas en páginas HTML, proporcionando opciones de
#' filtrado, paginación, clasificación y muchas otras características en las tablas.
#'
#' Al establecer `filtros = FALSE` no elimina ni modifica el filtro global
#' (*cuadro de búsqueda en la parte superior derecha*).
#'
#' Para el argumento `colFilters` recuerde que la numeración inicia en 0, es decir,
#' la primera columna tiene asociado el índice 0, la segunda el 1, y así sucesivamente.
#'
#' @returns
#' Retorna la tabla creada mediante `DT` la cual pertenece a la clase "datatables"
#' y "htmlwidget".
#'
#' @examplesIf all(require("DT"), require("dplyr"), require("tidyr"))
#' # library(DT); library(dplyr); library(tidyr)
#' # Example of R Combinations with Dot (".") and Pipe (%>%) Operator
#' # UnalR::Agregar(
#' #   formula    = SEDE_NOMBRE_ADM ~ YEAR + SEMESTRE,
#' #   frecuencia = list("Year" = 2009:2022, "Period" = 1:2),
#' #   datos      = UnalData::Graduados, ask = FALSE
#' #   ) |>
#' #   select(-Variable) |>
#' #   rename(Year = YEAR, Semester = SEMESTRE, Cat = Clase) %>%
#' #   Tabla(
#' #     ., rows = vars(Year, Semester), pivotCat = Cat, pivotVar = Total
#' #   )
#' Tabla(
#'   df = ejConsolidadoGrad |> dplyr::filter(Variable == "SEDE_NOMBRE_ADM") |> dplyr::select(-Variable),
#'   rows     = vars(YEAR, SEMESTRE),
#'   pivotCat = Clase,
#'   pivotVar = Total,
#'   columnNames = c("Año", "Semestre", "Total"),
#'   encabezado  = "TOTAL DE ESTUDIANTES \u00d7 SEDE DE GRADUACI\u00d3N",
#'   leyenda     = "Distribuci\u00f3n de estudiantes graduados (desde el 2009-I al 2021-I) por sede.",
#'   tituloPdf   = "ESTUDIANTES GRADUADOS POR SEDE",
#'   colorHead   = "#8CC63F",
#'   estilo      = list(
#'     list(
#'       columns = "YEAR", target = "cell", fontWeight = "normal",
#'       backgroundColor = styleEqual(unique(ejConsolidadoGrad$YEAR), rainbow(13, alpha = 0.5, rev = TRUE))
#'     ),
#'     list(
#'       columns = "SEMESTRE", target = "cell", fontWeight = "bold",
#'       color = styleEqual(unique(ejConsolidadoGrad$SEMESTRE), c("#EB0095", "#9D45FD"))
#'     )
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' VariosYears <- ejConsolidadoSaberPro2019         |>
#'   mutate(YEAR = replace(YEAR, YEAR==2019, 2020)) |>
#'   bind_rows(ejConsolidadoSaberPro2019)           |>
#'   filter(Variable == "sede") |> select(-Variable, -desv)
#'
#' Msj <- "\u00c9sta es una descripci\u00f3n de la tabla diferente al valor por default."
#' Tabla(
#'   df          = VariosYears,
#'   rows        = vars(YEAR, Clase, n),
#'   pivotCat    = Componente,
#'   pivotVar    = Total,
#'   columnNames = c("Año", "Sede", "n", "M\u00e1ximo"),
#'   estadistico = "Max",
#'   encabezado  = "PUNTAJES \u00d7 SEDE",
#'   leyenda     = Msj,
#'   colorHead   = "#F9CA00",
#'   estilo      = list(
#'     list(
#'       columns = "YEAR", target = "cell", fontWeight = "normal",
#'       backgroundColor = styleEqual(unique(VariosYears$YEAR), c("#AEF133", "#19EE9F"))
#'     ),
#'     list(
#'       columns = "Clase", target = "cell", fontWeight = "bold",
#'       color = styleEqual(unique(VariosYears$Clase), c("#42C501", "#7E10DE", "#FF6700", "#0096F2"))
#'     )
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' Tabla(df = datasets::mtcars)
#'
#' df <- ejGraduados |>
#'   filter(TIPO_NIVEL == "Pregrado") |>
#'   group_by(YEAR, SEMESTRE, DEP_NAC, CIU_NAC, SEXO, CAT_EDAD, ESTRATO, PROGRAMA) |>
#'   summarise(Total = n(), .groups = "drop") |>
#'   mutate(across(where(is.character), \(x) replace_na(x, replace = "SIN INFO")))
#'
#' Nombres <- c("<em>A\u00f1o</em>", "Semestre", "Departamento", "Municipio", "Sexo", "Edad", "Estrato", "Carrera", "Total")
#' Titulo  <- "<b>HIST\u00d3RICO DEL TOTAL DE GRADUADOS DE PREGRADO DEPENDIENDO DE LAS VARIABLES SELECCIONADAS</b>"
#' Tabla(
#'   df             = df,
#'   columnNames    = Nombres,
#'   filtros        = TRUE,
#'   colFilters     = 0:3,
#'   encabezado     = Titulo,
#'   leyenda        = "N\u00famero de graduados de pregrado por lugar de procedencia.",
#'   tituloPdf      = "Este es un t\u00edtulo provisional para el PDF",
#'   mensajePdf     = "Este es un mensaje provisional para el PDF",
#'   ajustarNiveles = TRUE,
#'   colorHead      = "#4CFF49",
#'   estilo         = list(
#'     list(
#'       columns = "YEAR", target = "cell", fontWeight = "bold",
#'       backgroundColor = styleEqual(unique(df$YEAR), c("#FF6400", "#01CDFE", "#FF0532"))
#'     ),
#'     list(
#'       columns = "SEMESTRE", target = "cell", fontWeight = "bold",
#'       color = styleEqual(unique(df$SEMESTRE), c("#3D3397", "#AE0421"))
#'     ),
#'     list(columns = "DEP_NAC", color = "#FFFFFF", backgroundColor = "#4D1B7B"),
#'     list(columns = "CIU_NAC", color = "#FFFFFF", backgroundColor = "#F59E11")
#'   )
#' )
#'
#' @export
#'
#' @import DT
#' @import dplyr
#' @importFrom htmltools withTags tag
#' @importFrom tidyr pivot_wider
#' @importFrom methods missingArg
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
Tabla <- function(
    df, rows, pivotCat, pivotVar, columnNames, filtros = FALSE, colFilters,
    estadistico = c("Suma", "Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
    encabezado = "Encabezados de los Niveles de la Categor\u00eda",
    leyenda, tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE,
    scrollX = TRUE, colorHead = "#FFFFFF", estilo) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (!all(is.logical(filtros), is.logical(ajustarNiveles), is.logical(scrollX))) {
    stop("\u00a1Los argumentos 'filtros', 'ajustarNiveles' y 'scrollX' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.character(colorHead)) {
    stop("\u00a1El argumento 'colorHead' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (missingArg(leyenda)) {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                       "Nota: ", htmltools::em("Los valores presentados entre par\u00e9ntesis hacen referencia a la desviaci\u00f3n est\u00e1ndar."),
                                       htmltools::br(htmltools::em("(*) hace referencia al total de estudiantes evaluados.")))
  } else {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', "Nota: ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")

  thead <- function(...) { htmltools::tag("thead", ...) }
  th <- function(...) { htmltools::tag("th", ...) }
  tr <- function(...) { htmltools::tag("tr", ...) }

  # ----------------------------------------------------------------------------
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  if (all(missingArg(rows), missingArg(pivotCat), missingArg(pivotVar))) {
    DataFrame <- df %>% mutate_all(., as.factor)
    colNames  <- colnames(df)

    if (filtros) {
      Filtros <- list(position = "top", clear = TRUE, plain = FALSE)

      if (missingArg(colFilters)) {
        dots <- list()
      } else {
        if (max(colFilters) >= length(colNames) || min(colFilters) < 0) {
          stop("\u00a1El vector ingresado para seleccionar las columnas con filtro debe estar entre [0, n-1] donde n representa el total de columnas!", call. = FALSE)
        } else {
          U    <- 0:(length(colNames) - 1)
          dots <- list(targets = setdiff(U, colFilters), searchable = FALSE)
        }
      }
    } else {
      if (!missingArg(colFilters)) {
        warning("\u00a1El valor para el argumento 'colFilters' que ha ingresado queda deshabilitado debido a que 'filtros = FALSE'!", call. = FALSE)
      }
      Filtros <- "none"; dots <- list()
    }

    colsDefs <- list(
      list(className = "dt-center", targets = 0:(length(colNames) - 1)),
      dots, list(width = "65px", targets = 0)
    )

    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(colspan = length(colNames), encabezado)
        ),
        tr(lapply(colNames, th))
      )
    ))
  } else {
    # Creación de la Tabla Pivoteada de Acuerdo con los Parámetros Ingresados
    DataFrame <- df |> pivot_wider(names_from = {{ pivotCat }}, values_from = {{ pivotVar }})
    nCat      <- df |> group_by({{ pivotCat }}) |> distinct({{ pivotCat }})
    Statistic <- match.arg(estadistico)
    Groups    <- df |> group_by(!!!rows, .drop = FALSE)
    addGlobal <- switch(
      Statistic,
      Suma     = Groups |> summarise("Statistic" = sum({{ pivotVar }}   , na.rm = TRUE), .groups = "drop"),
      Promedio = Groups |> summarise("Statistic" = mean({{ pivotVar }}  , na.rm = TRUE), .groups = "drop"),
      Mediana  = Groups |> summarise("Statistic" = median({{ pivotVar }}, na.rm = TRUE), .groups = "drop"),
      Varianza = Groups |> summarise("Statistic" = var({{ pivotVar }}   , na.rm = TRUE), .groups = "drop"),
      SD       = Groups |> summarise("Statistic" = sd({{ pivotVar }}    , na.rm = TRUE), .groups = "drop"),
      CV       = Groups |> summarise("Statistic" = cv({{ pivotVar }}    , na.rm = TRUE), .groups = "drop"),
      Min      = Groups |> summarise("Statistic" = min({{ pivotVar }}   , na.rm = TRUE), .groups = "drop"),
      Max      = Groups |> summarise("Statistic" = max({{ pivotVar }}   , na.rm = TRUE), .groups = "drop")
    )
    # Creación de la Columna Total Global (Total x Fila)
    DataFrame <- DataFrame |> left_join(addGlobal)
    colsDefs <- list(
      list(className = "dt-center", targets = 0:(n_groups(nCat)+2)),
      list(width = "65px", targets = 0)
    )
    DataFrame <- DataFrame |> mutate_at(rows, factor)
    # Custom Table Container (Nombre de los Encabezados)
    Txt <- ""
    if (!missingArg(columnNames)) {
      for (i in 1:(length(columnNames)-1)) { Txt <- paste0(Txt, paste0('th(rowspan = 2, "', columnNames[i], '"), ')) }
      lastCol <- tail(columnNames, n = 1)
    } else {
      for (i in 1:length(rows)) { Txt <- paste0(Txt, paste0('th(rowspan = 2, "Col', i, '"), ')) }
      lastCol <- "Total"
    }
    TxtFinal <- paste0(
      'htmltools::withTags(table(class = "display",
        thead(
          tr(',
      Txt,
      'th(colspan = n_groups(nCat), encabezado),
            th(rowspan = 2, "', lastCol , '")
          ),
          tr( lapply(nCat |> pull(), th) )
        )
       ))'
    )
    sketch  <- eval(parse(text = TxtFinal))
    Filtros <- "none"
  }
  # print(sketch)
  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class      = AjusteNiveles,
    rownames   = FALSE,
    container  = sketch,
    caption    = Leyenda,
    escape     = FALSE,
    filter     = Filtros,
    extensions = c("Buttons", "KeyTable"),
    options    = list(
      autoWidth  = TRUE,
      columnDefs = colsDefs,
      pageLength = 8,
      order = list(list(0, "desc"), list(1, "asc")),
      dom   = "Bfrtip",
      keys  = TRUE,
      searchHighlight = TRUE,
      scrollX = scrollX,
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
      buttons = list(
        list(extend = "copy", text = "Copiar"), "csv", "excel",
        list(extend = "pdf", pageSize = "A4", filename = "pdf",
             message = mensajePdf, title = tituloPdf
        ),
        list(extend = "print", text = "Imprimir", pageSize = "A4",
             message = mensajePdf, title = tituloPdf
        )
      )
    )
  )

  if (!missingArg(estilo)) {
    for (i in 1:length(estilo)) {
      Temp <- do.call(formatStyle, append(list(table = TablaFinal), estilo[[i]]))
      TablaFinal <- Temp
    }
  }
  # if (!missingArg(estilo)) {
  #   formatCols <- function(table, varTXT, list) {
  #     if (is.null(list$color)) { colorFinal <- NULL } else {
  #       colorFinal <- styleEqual(unique(DataFrame[[varTXT]]), list$color)
  #     }
  #     if (is.null(list$background)) { backgFinal <- NULL } else {
  #       backgFinal <- styleEqual(unique(DataFrame[[varTXT]]), list$background)
  #     }
  #     return(
  #       formatStyle(
  #         table = table, varTXT, target = "cell", fontWeight = list$font,
  #         color = colorFinal, backgroundColor = backgFinal
  #       )
  #     )
  #   }
  #   listVars <- as.character(substitute(rows))[-1]
  #   for (i in 1:length(listVars)) {
  #     TablaFinal <- TablaFinal |> formatCols(varTXT = listVars[i], list = estilo[[i]])
  #   }
  # }
  return(TablaFinal)
}
