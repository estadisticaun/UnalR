#' Cree una serie de tiempo dinámica y flexible con tres diferentes paquetes
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de series de tiempo dinámicas con el objetivo de estudiar la evolución de una o
#' varias variables a lo largo del tiempo. Dicha serie interactiva se puede representar
#' usando tres diferentes librerías que son `Highcharter`, `Plotly` y `Dygraph`, las
#' cuales usan internamente `JavaScript`.
#'
#' @param datos Un data frame, no un objeto clase serie de tiempo o vector numérico.
#' @param categoria Una variable categórica dentro del data frame ingresado en `datos`.
#' @param colores Cadena de caracteres indicando los colores con los cuales se deben
#'   colorear cada una de las series correspondiente a cada nivel del parámetro
#'   `categoria`. Si no se introduce algún vector se usará la paleta `rainbow` por
#'   defecto.
#' @param titulo Cadena de caracteres indicando el título principal del plot.
#' @param labelX Cadena de caracteres indicando la etiqueta del eje `X`. Por defecto
#'   se emplea el rótulo "Periodo".
#' @param labelY Cadena de caracteres indicando la etiqueta del eje `Y`.
#' @param libreria Cadena de caracteres que indica el paquete con el cual se realizará
#'   la serie. Los valores permitidos son `"highcharter"` (valor predeterminado),
#'   `"plotly"` o `"dygraphs"`. Los valores se emparejarán parcialmente.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar la serie y cuyo objetivo
#'   es personalizar pequeños detalles de ésta.
#'   * `LegendTitle`: Cadena de caracteres indicado un título para la legenda
#'     (\emph{diferentes niveles del parámetro `categorias`}). Se utilizará tanto
#'     en el paquete `Highcharter` como en `Plotly`.
#'   * `hc.Tema`: Modifica el tema con el cual se creará la serie. Los posibles
#'     valores son un número entero entre \eqn{[1, 10]} (\emph{el tema por defecto
#'     es el número 5, `hc_theme_flat()`}), el cual hace referencia a diferentes
#'     temas disponibles en dicha librería.
#'   * `hc.Slider`: Si es `TRUE` agrega un deslizador/navegador dinámico en la parte
#'     inferior de la serie. Proporciona herramientas para acercar y alejar partes
#'     de la serie, así como para desplazarse por el conjunto de datos. El valor por
#'     defecto es `FALSE`.
#'   * `hc.BoxInfo`: Si es `TRUE` (valor predeterminado) la información concerniente
#'     a cada punto se visualiza conjuntamente en un cuadro, o de forma individual
#'     (`FALSE`) al pasar el cursor.
#'   * `hc.Credits`: Cadena de caracteres indicando un subtítulo o etiqueta de
#'     créditos debajo del título principal.
#'   * `ply.LegendPosition`: Lista que específica la posición y orientación de la
#'     legenda. Los valores por defecto la ubican centrada verticalmente a la
#'     derecha del plot, es decir, `c(x = 1, y = 0.5, orientation = "v")`.
#'   * `ply.Interaction`: Cadena de caracteres que determina el modo de las
#'     interacciones de desplazamiento. Los valores permitidos son "x unified"
#'     (valor predeterminado), "y unified", "closest", "x", "y" y FALSE.
#'   * `ply.Credits`: Lista que específica la posición y texto para añadir un
#'     subtítulo o etiqueta de créditos a la serie principal, por ejemplo,
#'     `c(x = 0.2, y = 1, text = "https://...")`.
#'   * `dyg.LegendWidth`: Un número que indica el ancho (\emph{en píxeles}) que ocupará
#'     la leyenda. El valor por defecto es 250.
#'   * `dyg.Resaltar`: Si es `FALSE` (valor predeterminado) no se resaltará la serie
#'     en que se sitúa el cursor.
#'
#' @details
#' Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre del
#' archivo descargado será la concatenación del plot graficado y la categoría usada,
#' así, por ejemplo, si se graficó la serie de tiempo para la categoría "Sede" el
#' nombre será `PlotSeries_Sede.png`.
#'
#' @return
#' Retorna la serie (*objeto widget de HTML*) creada. La clase del objeto retornado
#' será un "htmlwidget" y dependiendo de la librería usada pertenecerá adicionalmente
#' a la clase "highchart", "plotly" o "dygraphs".
#'
#' @examples
#' col <- c("#29ABE2", # AZUL CLARO  | Amazonia
#'          "#8CC63F", # VERDE       | Bogota
#'          "#CC241D", # ROJO        | Caribe
#'          "#FF3673", # MORADO CLARO| De la paz
#'          "#0071BC", # AZUL VIVO   | Manizales
#'          "#F15A24", # NARANJA     | Medellin
#'          "#FBB03B", # AMARILLO    | Orinoquia
#'          "#93278F", # MORADO      | Palmira
#'          "#8A381A") # GRIS        | Tumaco
#' Msj <- "Distribuci\u00f3n de estudiantes admitidos (desde el 2009-I al 2020-II) por sede."
#' Plot.Series(datos     = Consolidado,
#'             categoria = "SEDE_NOMBRE_ADM",
#'             colores   = col,
#'             titulo    = "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEDE",
#'             labelY    = "N\u00famero de graduados (k: miles)",
#'             libreria  = "highcharter",
#'             estilo    = list(LegendTitle = "SEDE:", hc.Tema = 4, hc.Slider = TRUE, hc.Credits = Msj)
#'             )
#' Plot.Series(datos     = Consolidado,
#'             categoria = "SEDE_NOMBRE_ADM",
#'             colores   = col,
#'             titulo    = "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEDE",
#'             labelY    = "N\u00famero de graduados (k: miles)",
#'             libreria  = "plotly",
#'             estilo    = list(LegendTitle = "SEDE:", ply.Interaction = "closest",
#'                              ply.LegendPosition = list(x = 0.16, y = -0.25, orientation = "h"),
#'                              ply.Credits = list(x = 0.5, y = 0.95, text = Msj))
#'             )
#' Plot.Series(datos     = Consolidado,
#'             categoria = "SEDE_NOMBRE_ADM",
#'             colores   = col,
#'             titulo    = "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEDE",
#'             labelY    = "N\u00famero de graduados (k: miles)",
#'             libreria  = "dygraphs",
#'             estilo    = list(dyg.LegendWidth = 810, dyg.Resaltar = TRUE)
#'             )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dygraphs
#' @import dplyr
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom xts xts
#' @importFrom zoo as.Date as.yearmon
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Series <- function(datos, categoria, colores, titulo = "", labelX = "Periodo", labelY = "",
                        libreria = c("highcharter", "plotly", "dygraphs"), estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  # categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("\u00a1Por favor introduzca una categor\u00eda que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelX) && is.character(labelY))) {
    stop("\u00a1El argumento 'titulo', 'labelX' y 'labelY' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly", "dygraphs")) {
      stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
    }
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame <- datos %>%
    filter(Variable == categoria) %>%
    mutate(Fecha = paste(YEAR, SEMESTRE, sep = "-")) %>%
    select(-Variable, -YEAR, -SEMESTRE) %>%
    relocate(Fecha)

  TablaHorizontal <- DataFrame %>% pivot_wider(names_from = Clase, values_from = Total)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()

  if (length(categorias)==1L) {
    Relativo <- DataFrame %>%
      mutate(Relativo = Total/Total*100) %>%
      select(-Total)
  } else {
    Relativo <- TablaHorizontal %>% select(-Fecha) %>%
      PocentRelativo() %>% as_tibble() %>%
      mutate(Fecha = TablaHorizontal$Fecha) %>%
      pivot_longer(cols = categorias, names_to = "Clase", values_to = "Relativo")
  }
  TablaFinal <- DataFrame %>% inner_join(Relativo)

  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {

    Spanish.Highcharter()
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1"  = hc_theme_ffx(),
                        "2"  = hc_theme_google(),
                        "3"  = hc_theme_538(),
                        "4"  = hc_theme_ggplot2(),
                        "5"  = hc_theme_economist(),
                        "6"  = hc_theme_sandsignika(),
                        "7"  = hc_theme_ft(),
                        "8"  = hc_theme_superheroes(),
                        "9"  = hc_theme_flatdark(),
                        "10" = hc_theme_flat()
                        )
    } else { ThemeHC <- hc_theme_flat() }
    BoxInfo <- ifelse(!(missingArg(estilo) || is.null(estilo$hc.BoxInfo)), estilo$hc.BoxInfo, TRUE)

    PlotSeries <- TablaFinal %>%
      hchart(type = "line", hcaes(x = Fecha, y = Total, group = Clase), color = colores,
             zoomType = list(enabled = FALSE),  resetZoomButton = TRUE) %>%
      hc_chart(type = "datetime", zoomType = "x") %>%
      hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "square", radius = 1))) %>%

      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
      ) %>%
      hc_xAxis(title = list(text   = labelX,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      align = "center", lineColor = "#787878", opposite  = FALSE,
      labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_yAxis(title = list(text   = labelY,
                            offset = 70,
                            style  = list(fontWeight = "bold",
                                          fontSize   = "18px",
                                          color      = "black")
      ),
      lineColor = "#787878", opposite  = FALSE, lineWidth = 1, min = 0,
      labels    = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      # https://github.com/jbkunst/highcharter/issues/331
      hc_exporting(enabled = TRUE, filename = paste0("PlotSeries_", categoria)) %>%
      hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom", layout = "horizontal",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                x = 42, y = 0, itemStyle = list(fontWeight = "bold",
                                                color      = "black",
                                                fontSize   = "18px")) %>%
      hc_tooltip(crosshairs = TRUE, shared = BoxInfo,
                 pointFormat = '<span style="color:{series.color}">\u25CF </span><b>{series.name}: {point.y}</b> ({point.Relativo}%)<br/>',
                 backgroundColor = hex_to_rgba("#BAAEAE", 0.7),
                 borderColor = "#6D6666", borderWidth = 5, useHTML = TRUE) %>%
      hc_add_theme(ThemeHC)
    
    if (!missingArg(estilo) && "hc.Slider"%in%names(estilo) && estilo$hc.Slider) {
      PlotSeries <- PlotSeries %>%
        hc_navigator(height = 15, margin = 5, maskFill = "rgba(255,16,46,0.6)",
                     enabled = TRUE, series = list(color     = "#999999",
                                                   lineWidth = 30,
                                                   type      = "areaspline",
                                                   fillColor = "#999999")
        ) %>%
        hc_rangeSelector(enabled = TRUE, inputEnabled = FALSE, labelStyle = list(display = "none"),
                         buttonPosition = list(align = "left"), floating = FALSE,
                         buttons = list(list(type = "all", text = "Restaurar")))
    }
    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotSeries <- PlotSeries %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }

  } else if (libreria == "plotly") {

    if (!(missingArg(estilo) || is.null(estilo$ply.LegendPosition))) {
      ParmsLegend <- estilo$ply.LegendPosition
    } else {
      ParmsLegend <- list(x = 1, y = 0.5, orientation = "v")
    }
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }
    Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)), estilo$ply.Interaction, "x unified")

    FreqRelativa <- Relativo %>% pivot_wider(names_from = Clase, values_from = Relativo)
    PlotSeries <- plot_ly(data = TablaHorizontal)
    for (i in 1:length(categorias)) {
      df_Temp    <- data.frame(X = TablaHorizontal$Fecha, Y = TablaHorizontal[[categorias[i]]], Text = FreqRelativa[[categorias[i]]])
      PlotSeries <- add_trace(PlotSeries, x = ~X, y = ~Y, data = df_Temp, text = ~ Text,
                              name = categorias[i], type = "scatter", mode = "markers+lines",
                              line = list(color = colores[i], width = 3),
                              marker =  list(color = colores[i], size = 6, line = list(width = 1.2, color = "#787878")),
                              hovertemplate = paste('%{y}', '(%{text:.2s}%)'),
                              textposition = "outside")
    }
    # Arial | Open Sans | Courier New, monospace
    FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")

    Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
    Xaxis <- list(title = labelX,
                  zeroline = FALSE,
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 2.5,
                  autotick  = FALSE,
                  ticks     = "outside",
                  tickwidth = 2.5,
                  ticklen   = 10,
                  tickcolor = "#CCCCCC",
                  tickangle = -45,
                  tickfont  = FamilyAxis)
    Yaxis <- list(title = labelY,
                  zeroline = TRUE,
                  showline = TRUE,
                  showgrid = TRUE,
                  showticklabels = TRUE,
                  linecolor = "#787878",
                  linewidth = 3,
                  separatethousands = TRUE,
                  tickfont  = FamilyAxis)

    PlotSeries <- PlotSeries %>%
      layout(title = Title, xaxis = Xaxis, yaxis = Yaxis,
             autosize = TRUE, showlegend = TRUE,
             legend = append(ParmsLegend, list(traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>")))),
             hovermode = Hovermode,
             annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#CCCCCC")))
      ) %>% config(locale = "es")

  } else if (libreria == "dygraphs") {

    LegendWidth <- ifelse(!(missingArg(estilo) || is.null(estilo$dyg.LegendWidth)), estilo$dyg.LegendWidth, 250)

    Periodos <- TablaHorizontal %>% select(Fecha) %>% distinct() %>% pull()
    Periodos <- gsub("-2", "-7", Periodos)
    TableHorizontal <- TablaHorizontal
    TableHorizontal$Fecha <- as.Date(as.yearmon(Periodos, "%Y-%m"))
    TableHorizontal <- xts(x = TableHorizontal[,-1], order.by = TableHorizontal$Fecha)

    getSemestre <- 'function(d) {
                      var monthNames = ["I", "", "", "", "", "","II", "", "", "", "", ""];
                      date = new Date(d);
                      if (date.getMonth() == 0 || date.getMonth() == 6) {
                        return date.getFullYear() + "-" + monthNames[date.getMonth()];
                      } else {
                        return "";
                      }
                   }'
    dyUnzoom <- function(dygraph) {
      dyPlugin(
        dygraph = dygraph,
        name = "Unzoom",
        path = system.file("plugins/unzoom.js", package = "dygraphs")
      )
    }

    PlotSeries <- dygraph(TableHorizontal, main = paste0("<span style='color:", "#333333", ";'>", titulo, "</span>")) %>%
      dyOptions(drawPoints = TRUE, pointSize = 2,
                strokeWidth = 2, colors = colores, includeZero = TRUE,
                axisTickSize = 3, axisLineColor = "#787878",
                axisLabelColor = "#525252", axisLabelFontSize = 16,
                drawGrid = TRUE, gridLineColor = "lightblue") %>%
      dyLegend(show = "always", width = LegendWidth, hideOnMouseOut = TRUE) %>%
      dyAxis("x", label = labelX, axisLabelFormatter = JS(getSemestre), axisLineWidth = 4) %>%
      dyAxis("y", label = labelY, axisLineWidth = 4) %>%
      dyRangeSelector(height = 30, strokeColor = "") %>%
      dyUnzoom()

    if (!(missingArg(estilo) || is.null(estilo$dyg.Resaltar))) {
      if (estilo$dyg.Resaltar) {
        PlotSeries <- PlotSeries %>%
          dyHighlight(highlightCircleSize = 5,
                      highlightSeriesBackgroundAlpha = 0.5,
                      highlightSeriesOpts = list(strokeWidth = 2.5),
                      hideOnMouseOut = TRUE)
      }
    }
  }

  return(PlotSeries)
}
