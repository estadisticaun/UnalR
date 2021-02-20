#' Cree un gráfico circular/torta/pie dinámico y flexible con dos diferentes paquetes
#'
#' Esta función permite mostrar de forma interactiva una descripción compacta y general
#' de una variable con sus respectivas categorías. Dicho diagrama se puede representar
#' usando dos diferentes librerías que son `Highcharter` y `Plotly`, las cuales usan
#' internamente `JavaScript`.
#'
#' @param datos Un data frame.
#' @param categoria Una variable categórica dentro del data frame ingresado en `datos`.
#' @param ano Valor numérico que indica el año de interés para realizar el gráfico.
#'   Si solo se introduce el año, pero no el período, se calculará independiente del
#'   semestre (*tomando ambos*).
#' @param periodo Valor numérico que indica el período o semestre de interés para
#'   realizar el gráfico. Si solo se introduce el período, pero no el año, se calculará
#'   en base a todos los años disponibles en `datos`.
#' @param colores Igual uso que en [Plot.Series()]
#' @param titulo Igual uso que en [Plot.Series()]
#' @param addPeriodo Si es `TRUE` (valor predeterminado ssi se introduce los argumentos
#'   `ano` y `periodo` simultáneamente) concatenará al título introducido una cadena
#'   de caracteres indicando entre paréntesis el "año-periodo" insertado.
#' @param libreria Igual uso que en [Plot.Series()]
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería específica para graficar la torta y cuyo objetivo es
#'   personalizar pequeños detalles de ésta.
#'   * `LegendTitle`, `hc.Tema`, `hc.Credits` y `ply.Credits`: Igual uso que en [Plot.Series()]
#'   * `ply.Leyenda`: Por defecto la gráfica muestra la legenda fuera del grafico de
#'     pie, si se introduce la cadena de texto "inside" se resumirá toda la información
#'     dentro del pie.
#'
#' @details
#' Al usar el paquete `Highcharter` y usar las opciones de descarga, el nombre del
#' archivo descargado será la concatenación del plot graficado y la categoría usada,
#' así, por ejemplo, si se graficó el diagrama de pie para la categoría "Sexo" el
#' nombre será `PlotTorta_Sexo.png`.
#'
#' @note
#' Los gráficos circulares son una forma muy mala de mostrar información. El ojo es
#' bueno para juzgar medidas lineales y malo para juzgar áreas relativas. Un gráfico
#' de barras o un gráfico de puntos es una forma preferible de mostrar este tipo de datos.
#'
#' @return
#' Retorna el diagrama circular (*objeto widget de HTML*) creado. La clase del objeto
#' retornado será un "htmlwidget" y dependiendo de la librería usada pertenecerá
#' adicionalmente a la clase "highchart" o "plotly".
#'
#' @examples
#' ano <- 2020; semestre <- 2
#' col <- c("#F15A24", "#8CC63F")
#' Msj <- "Distribuci\u00f3n de estudiantes admitidos en el segundo periodo acad\u00e9mico del 2020."
#' Plot.Torta(datos      = Consolidado,
#'            categoria  = "TIPO_NIVEL",
#'            ano        = ano,
#'            periodo    = semestre,
#'            colores    = col,
#'            titulo     = "DISTRIBUCI\u00d3N DE GRADUADOS POR MODALIDAD DE FORMACI\u00d3N",
#'            addPeriodo = TRUE,
#'            libreria   = "highcharter",
#'            estilo     = list(LegendTitle = "\u00c9sta es una descripci\u00f3n para la leyenda:",
#'                              hc.Tema = 4, hc.Credits = Msj)
#'            )
#' Plot.Torta(datos      = Consolidado,
#'            categoria  = "TIPO_NIVEL",
#'            ano        = ano,
#'            periodo    = semestre,
#'            colores    = col,
#'            titulo     = "DISTRIBUCI\u00d3N DE GRADUADOS POR MODALIDAD DE FORMACI\u00d3N",
#'            addPeriodo = FALSE,
#'            libreria   = "plotly",
#'            estilo     = list(ply.Leyenda = "inside",
#'                              ply.Credits = list(x = 0.62, y = 1.1,
#'                                                 text = paste0("<b>", Msj, "</b>")))
#'            )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dplyr
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Torta <- function(datos, categoria, ano, periodo, colores, titulo = "", addPeriodo = TRUE,
                       libreria = c("highcharter", "plotly"), estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("\u00a1Por favor introduzca una categor\u00eda que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!is.character(titulo)) {
    stop("\u00a1El argumento 'titulo' debe ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(ano) && missingArg(periodo)) {
    stop("\u00a1Por favor introduzca al menos uno de los dos argumentos (o ambos), sea 'ano' o 'periodo'!", call. = FALSE)
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
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Etiqueta    <- ifelse(is.null(estilo$hc.Etiqueta), "N\u00famero de graduados", estilo$hc.Etiqueta)

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame  <- ungroup(datos) %>% filter(Variable == categoria) %>% select(-Variable)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()

  if (missingArg(ano)) {
    TablaFinal <- DataFrame %>% select(-YEAR) %>% filter(SEMESTRE == periodo) %>% group_by(Clase) %>% summarise(Total = sum(Total))
  } else if (missingArg(periodo)) {
    TablaFinal <- DataFrame %>% select(-SEMESTRE) %>% filter(YEAR == ano) %>% group_by(Clase) %>% summarise(Total = sum(Total))
  } else {
    titulo     <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (PERIODO ", ano, "-", periodo, ")"), titulo)
    TablaFinal <- DataFrame %>% filter(YEAR == ano, SEMESTRE == periodo)
  }
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
    } else { ThemeHC <- hc_theme_flat() }

    PlotTorta <- TablaFinal %>%
      hchart(type = "pie", hcaes(x = Clase, y = Total), name = Etiqueta, showInLegend = TRUE) %>%
      hc_title(text = titulo, style = list(fontWeight = "bold",
                                           fontSize   = "22px",
                                           color      = "#333333",
                                           useHTML    = TRUE)
      ) %>%
      hc_plotOptions(pie = list(allowPointSelect = TRUE,
                                colorByPoint = TRUE,
                                colors = colores,
                                dataLabels = list(enabled = TRUE,
                                                  format = "<b>{point.name}</b>: {point.percentage:.1f} %",
                                                  style = list(fontWeight = "bold",
                                                               color      = "black",
                                                               fontSize   = "18px")
                                )
      )
      ) %>%
      hc_exporting(enabled = TRUE, filename = paste0("PlotTorta_", categoria)) %>%
      hc_legend(enabled = TRUE, align = "center", verticalAlign = "bottom",
                title = list(text = LegendTitle, style = list(textDecoration = "underline")),
                itemStyle = list(fontWeight = "bold",
                                 color      = "black",
                                 fontSize   = "18px")) %>%
      hc_add_theme(ThemeHC)

    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotTorta <- PlotTorta %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }

  } else if (libreria == "plotly") {

    if (!(missingArg(estilo) || is.null(estilo$ply.Credits))) {
      ParmsCredits <- estilo$ply.Credits
    } else {
      ParmsCredits <- list(x = 0.2, y = 1, text = "")
    }

    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")
    Title  <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.95)
    Margen <- list(l = 50, r = 50, t = 110, b = 0)                                      # l = left; r = right; t = top; b = bottom

    if (!missingArg(estilo) && estilo$ply.Leyenda=="inside") {
      PlotTorta <- plot_ly(TablaFinal, labels = ~Clase, values = ~Total, type = "pie",
                           textposition = "inside", textinfo = "label+value+percent",   # "label+percent"
                           insidetextfont = list(color = "#FFFFFF", size = 20), hoverinfo = "label+value",
                           insidetextorientation = "radial",                            # "horizontal", "radial", "tangential", "auto"
                           marker = list(colors = colores, line = list(color = "#000000", width = 1.5))) %>%
        layout(title = Title, showlegend = FALSE,  autosize = TRUE, margin = Margen)
    } else {
      PlotTorta <- plot_ly(TablaFinal, labels = ~Clase, values = ~Total, type = "pie",
                           textinfo = "label+percent",
                           marker = list(colors = colores, line = list(color = "#FFFFFF", width = 1.5))) %>%
        layout(title = Title, showlegend = TRUE,  autosize = TRUE, margin = Margen)
    }
    PlotTorta <- PlotTorta %>%
      layout(annotations = append(ParmsCredits, list(showarrow = FALSE, xref = "paper", yref = "paper",
                                                     xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
                                                     font = list(size = 12, color = "#2B908F")))
      ) %>% config(locale = "es")
  }

  return(PlotTorta)
}
