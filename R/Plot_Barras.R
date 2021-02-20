#' Cree un gráfico de barras que muestra la información en columnas horizontales o
#' verticales y para variables nominales u ordinales con dos diferentes paquetes
#'
#' Esta función permite mostrar de forma interactiva un gráfico de barras verticales
#' u horizontales cuya longitud/altura es proporcional al valor de la variable
#' (*categorías de una variable cualitativa*), lo anterior para ayudar a la creación
#' de informes descriptivos y analíticos. Dicho diagrama se puede representar usando
#' dos diferentes librerías que son `Highcharter` y `Plotly`, las cuales usan
#' internamente `JavaScript`.
#'
#' @param datos Un data frame.
#' @param categoria Una variable categórica dentro del data frame ingresado en `datos`.
#' @param ano Igual uso que en [Plot.Torta()]
#' @param periodo Igual uso que en [Plot.Torta()]
#' @param vertical Si es `TRUE` (valor predeterminado) indicará que la orientación
#'   del gráfico será vertical.
#' @param ordinal Si es `TRUE` indicará que las categorías de la variable ingresada
#'   son ordinales (*no nominales*), esto con el fin de ordenar la disposición en el
#'   que se presentan en el eje del gráfico, el valor por defecto es `FALSE`.
#' @param colores Igual uso que en [Plot.Torta()]
#' @param libreria Igual uso que en [Plot.Torta()]
#' @param titulo Igual uso que en [Plot.Torta()]
#' @param labelEje Cadena de caracteres indicando la etiqueta del eje `X` o `Y`
#'   (*dependiendo de la orientación del gráfico*). Por defecto se emplea el rótulo
#'   "Número de Graduados".
#' @param addPeriodo Igual uso que en [Plot.Torta()]
#' @param textInfo Cadena de caracteres que especifica el texto que se escribe dentro
#'   de la caja de información al posar el cursor en alguna barra en el gráfico,
#'   producido por `Highcharter`, el valor por defecto es igual al de `labelEje`.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería específica para graficar el plot y cuyo objetivo es
#'   personalizar pequeños detalles de ésta.
#'   * `hc.Tema` y `hc.Credits`: Igual uso que en [Plot.Torta()]
#'   * `ply.Credits` y `ply.Legend`: Igual uso que en [Plot.Torta()]
#'
#' @details
#' Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre del
#' archivo descargado será la concatenación del plot graficado y la categoría usada,
#' así, por ejemplo, si se graficó el diagrama de barras para la categoría "Nacionalidad"
#' el nombre será `PlotBarras_Nacionalidad.png`.
#'
#' @return
#' Retorna el diagrama de barras (*objeto widget de HTML*) creado. La clase del objeto
#' retornado será un "htmlwidget" y dependiendo de la librería usada pertenecerá
#' adicionalmente a la clase "highchart" o "plotly".
#'
#' @examples
#' ano <- 2020; semestre <- 2
#' Msj <- "Ac\u00e1 puede ir m\u00e1s informaci\u00f3n acerca del gr\u00e1fico."
#' Plot.Barras(datos      = Consolidado,
#'             categoria  = "NIVEL",
#'             ano        = ano,
#'             periodo    = semestre,
#'             vertical   = TRUE,
#'             ordinal    = TRUE,
#'             colores    = heat.colors(5),
#'             libreria   = "highcharter",
#'             titulo     = "\u00c9STE ES UN T\u00cdTULO PERSONALIZADO",
#'             labelEje   = "\u00c9ste es el nombre del eje personalizado",
#'             addPeriodo = FALSE,
#'             textInfo   = "Este es el texto dentro del InfoBox",
#'             estilo     = list(hc.Tema = 1, hc.Credits = Msj)
#'             )
#' Plot.Barras(datos      = Consolidado,
#'             categoria  = "NIVEL",
#'             ano        = ano,
#'             periodo    = semestre,
#'             vertical   = FALSE,
#'             ordinal    = FALSE,
#'             libreria   = "plotly",
#'             titulo     = "\u00c9STE ES UN T\u00cdTULO PERSONALIZADO",
#'             labelEje   = "\u00c9ste es el nombre del eje personalizado",
#'             addPeriodo = FALSE,
#'             estilo     = list(ply.Credits = list(x = 0.35, y = 1.1, text = Msj),
#'                               ply.Legend = FALSE)
#'             )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dplyr
#' @importFrom scales percent
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Barras <- function(datos, categoria, ano, periodo, vertical = TRUE, ordinal = FALSE, colores, libreria = c("highcharter", "plotly"),
                        titulo = "", labelEje = "N\u00famero de Graduados", addPeriodo = TRUE, textInfo = labelEje, estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("\u00a1Por favor introduzca una categor\u00eda que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelEje) && is.character(textInfo))) {
    stop("\u00a1El argumento 'titulo', 'labelEje' y 'textInfo' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria  <- tolower(libreria)
    '%NotIN%' <- Negate('%in%')
    if (libreria %NotIN% c("highcharter", "plotly")) {
      stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
    }
  }

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame  <- ungroup(datos) %>% filter(Variable == categoria) %>% select(-Variable) %>% filter(is.na(Clase)!=TRUE)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()

  if (missingArg(ano)) { TablaFinal <- DataFrame %>% select(-YEAR) %>% filter(SEMESTRE == periodo) }
  else if (missingArg(periodo)) { TablaFinal <- DataFrame %>% select(-SEMESTRE) %>% filter(YEAR == ano) }
  else {
    titulo     <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (PERIODO ", ano, "-", periodo, ")"), titulo)
    TablaFinal <- DataFrame %>% filter(YEAR == ano, SEMESTRE == periodo)
  }

  if (!(missingArg(colores) || length(colores)==length(categorias))) {
    stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
                "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  if (!ordinal) {
    TablaFinal <- bind_cols(TablaFinal, "Colores" = colores)
    TablaFinal <- TablaFinal %>% arrange(desc(Total))
    MyColors   <- TablaFinal$Colores
  } else { MyColors <- colores }

  # CREACIÓN DEL PLOT RETORNAR
  if(libreria == "highcharter") {

    Spanish.Highcharter()
    if(!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
                        "1" = hc_theme_538(),
                        "2" = hc_theme_alone(),
                        "3" = hc_theme_economist(),
                        "4" = hc_theme_ffx(),
                        "5" = hc_theme_flat(),
                        "6" = hc_theme_ggplot2(),
                        "7" = hc_theme_google(),
                        "8" = hc_theme_monokai(),
                        "9" = hc_theme_darkunica(),
                        "10" = hc_theme_gridlight()
      )
    } else { ThemeHC <- hc_theme_elementary() }

    Orientacion <- ifelse(vertical, "column", "bar")
    PlotOptions <- list(colorByPoint = TRUE, colors = MyColors, dataLabels = list(enabled = TRUE, style = list(fontWeight = "bold",
                                                                                                               color      = "black",
                                                                                                               fontSize   = "18px"))
    )

    PlotBarras <- highchart() %>%
      hc_add_series(TablaFinal, type = Orientacion, hcaes(x = paste(Clase, "-", round(Total*100/sum(Total),1), "%"), y = Total),
                    name = textInfo, showInLegend = FALSE) %>%
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
      ) %>%
      hc_plotOptions(bar = PlotOptions, column = PlotOptions) %>%
      hc_xAxis(categories = TablaFinal$Clase,
               labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_yAxis(title  = list(text = labelEje, style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
               labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%

      hc_exporting(enabled = TRUE, filename = paste0("PlotBarras_", categoria)) %>%
      hc_add_theme(ThemeHC)

    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotBarras <- PlotBarras %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }

  } else if (libreria == "plotly") {

    # PocentRelativo <- function(x) { return(as.vector(round(x*100/sum(x, na.rm = TRUE), 2))) }
    # FreqRelativo   <- PocentRelativo(TablaFinal$Total)
    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.11, y = 1.1, text = "")
    }
    ShowLeyenda <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Legend)), estilo$ply.Legend, TRUE)

    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
    if (titulo == "") { Margen <- NULL } else { Margen <- list(l = 50, r = 50, t = 110, b = 0) }

    if (vertical) {
      if (ordinal) { EjeX <- "Clase"; EjeY <- "Total" } else { EjeX <- "reorder(Clase, Total)"; EjeY <- "Total" }
      PlotBarras <- plot_ly(TablaFinal, x = ~eval(parse(text = EjeX)), y = ~eval(parse(text = EjeY)),
                            type = "bar", color = ~Clase, orientation = "v",
                            hovertemplate = ~paste0(Total, " (", scales::percent(Total/sum(Total)), ")"),
                            marker = list(color = colores, line = list(color = "#3A4750", width = 1.5))) %>%
        layout(title = Title, xaxis = list(title = ""), yaxis = list(title = labelEje),
               showlegend = ShowLeyenda, autosize = TRUE, margin = Margen)
    } else {
      if (ordinal) { EjeX <- "Total"; EjeY <- "Clase" } else { EjeX <- "Total"; EjeY <- "reorder(Clase, Total)" }
      PlotBarras <- plot_ly(TablaFinal, x = ~eval(parse(text = EjeX)), y = ~eval(parse(text = EjeY)),
                            type = "bar", color = ~Clase, orientation = "h",
                            hovertemplate = ~paste0(Total, " (", scales::percent(Total/sum(Total)), ")"),
                            marker = list(color = colores, line = list(color = "#3A4750", width = 1.5))) %>%
        layout(title = Title, xaxis = list(title = labelEje), yaxis = list(title = ""),
               showlegend = ShowLeyenda, autosize = TRUE, margin = Margen)
    }

    PlotBarras <- PlotBarras %>%
      layout(annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#2B908F")))
      ) %>% config(locale = "es")
  }

  return(PlotBarras)
}
