#' Cree un gráfico circular/torta/pie dinámico y flexible con dos diferentes paquetes
#'
#' Esta función permite mostrar de forma interactiva una descripción compacta y
#' general de una variable con sus respectivas categorías. Dicho diagrama se puede
#' representar usando dos diferentes librerías que son `Highcharter` y `Plotly`,
#' las cuales usan internamente `JavaScript`.
#'
#' @param datos Un data frame.
#' @param categoria Igual uso que en [Plot.Series()]
#' @param ano Valor numérico que indica el año de interés para realizar el gráfico.
#'   Si solo se introduce el año, pero no el período, se calculará independiente
#'   del semestre (*tomando ambos*).
#' @param periodo Valor numérico que indica el período o semestre de interés para
#'   realizar el gráfico. Si solo se introduce el período, pero no el año, se
#'   calculará en base a todos los años disponibles en `datos`.
#' @param colores Igual uso que en [Plot.Series()]
#' @param titulo Igual uso que en [Plot.Series()]
#' @param label Cadena de caracteres indicando la etiqueta a la que hace referencia
#'   el plot. Por defecto se emplea el rótulo "Número de ".
#' @param addPeriodo Si es `TRUE` (*valor predeterminado ssi se introduce los
#'   argumentos `ano` y `periodo` simultáneamente*) concatenará al título introducido
#'   una cadena de caracteres indicando entre paréntesis el "año-periodo" insertado.
#' @param libreria Cadena de caracteres que indica el paquete con el cual se realizará
#'   el plot. Los valores permitidos son `"highcharter"` (*valor predeterminado*)
#'   y `"plotly"`. Los valores se emparejarán parcialmente.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería específica para graficar la torta y cuyo objetivo
#'   es personalizar pequeños detalles de ésta.
#'   * `LegendTitle`, `hc.Tema`, `hc.Credits` y `ply.Credits`: Igual uso que en
#'     [Plot.Series()]
#'   * `ply.Legend`: Por defecto la gráfica muestra la leyenda fuera del gráfico
#'     de pie, si se introduce la cadena de texto "inside" se resumirá toda la
#'     información dentro del pie.
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
#' col <- c("#F15A24", "#8CC63F")
#' Msj <- "Distribuci\u00f3n de estudiantes gradados en el primer periodo acad\u00e9mico del 2021."
#' Txt <- "DISTRIBUCI\u00d3N DE GRADUADOS POR MODALIDAD DE FORMACI\u00d3N"
#' Plot.Torta(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "TIPO_NIVEL",
#'   ano       = 2021,
#'   periodo   = 1,
#'   colores   = col,
#'   titulo    = Txt,
#'   label     = "Graduados",
#'   libreria  = "highcharter",
#'   estilo    = list(
#'     LegendTitle = "\u00c9sta es una descripci\u00f3n para la leyenda:",
#'     hc.Tema = 7, hc.Credits = Msj
#'   )
#' )
#'
#' Msj <- "Distribuci\u00f3n hist\u00f3rica de estudiantes gradados (desde el 2009-I al 2021-I)."
#' Plot.Torta(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "TIPO_NIVEL",
#'   colores   = col,
#'   titulo    = Txt,
#'   libreria  = "plotly",
#'   estilo    = list(
#'     ply.Legend = "inside", ply.Credits = list(
#'       x = 0.66, y = 1.1, text = paste0("<b>", Msj, "</b>")
#'     )
#'   )
#' )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dplyr
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Torta <- function(datos, categoria, ano, periodo, colores,
                       titulo = "", label = "", addPeriodo = TRUE,
                       libreria = c("highcharter", "plotly"), estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos) || missingArg(categoria)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una categor\u00eda dentro de la columna 'Variable'!", call. = FALSE)
  }
  categoria <- toupper(categoria)
  if (!(categoria %in% datos$Variable)) {
    stop("\u00a1Por favor introduzca una categor\u00eda que se encuentra dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(label))) {
    stop("\u00a1Los argumentos 'titulo' y 'label' deben ser una cadena de texto!", call. = FALSE)
  }
  if (missingArg(libreria)) {
    warning("\u00a1Se usar\u00e1 la librer\u00eda 'highcharter' por defecto para realizar el plot!", call. = FALSE)
    libreria <- "highcharter"
  } else {
    libreria <- tolower(libreria)
    if (libreria %NotIN% c("highcharter", "plotly")) {
      stop("\u00a1Por favor introduzca el nombre de una librer\u00eda valida (paquete usado para realizar la gr\u00e1fica)!", call. = FALSE)
    }
  }
  Etiqueta    <- paste("N\u00famero de", label)
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame <- ungroup(datos) %>%
    filter(Variable == categoria) %>% select(-Variable)
  categorias <- DataFrame %>% select(Clase) %>% distinct() %>% pull()

  if (!(missingArg(ano) || missingArg(periodo))) {
    titulo <- ifelse(!missingArg(titulo) && addPeriodo, paste0(titulo, " (PERIODO ", ano, "-", periodo, ")"), titulo)
    TablaFinal <- DataFrame %>% filter(YEAR == ano, SEMESTRE == periodo)
  } else {
    if (missingArg(ano) && missingArg(periodo)) {
      TablaFinal <- DataFrame %>% group_by(Clase) %>% summarise(Total = sum(Total))
    } else if (missingArg(ano)) {
      TablaFinal <- DataFrame %>% filter(SEMESTRE == periodo) %>%
        group_by(Clase) %>% summarise(Total = sum(Total))
    } else {
      TablaFinal <- DataFrame %>% filter(YEAR == ano) %>%
        group_by(Clase) %>% summarise(Total = sum(Total))
    }
  }

  if (!(missingArg(colores) || length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
    ), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

  # CREACIÓN DEL PLOT RETORNAR
  if (libreria == "highcharter") {
    Spanish.Highcharter()
    if (!(missingArg(estilo) || is.null(estilo$hc.Tema))) {
      ThemeHC <- switch(estilo$hc.Tema,
        "1"  = hc_theme_ffx(),
        "2"  = hc_theme_google(),
        "3"  = hc_theme_tufte(),
        "4"  = hc_theme_538(),
        "5"  = hc_theme_ggplot2(),
        "6"  = hc_theme_economist(),
        "7"  = hc_theme_sandsignika(),
        "8"  = hc_theme_ft(),
        "9"  = hc_theme_superheroes(),
        "10" = hc_theme_flatdark()
      )
    } else { ThemeHC <- hc_theme_flat() }

    PlotTorta <- TablaFinal %>%
      hchart(type = "pie", hcaes(x = Clase, y = Total), name = Etiqueta, showInLegend = TRUE) %>%
      hc_title(text = titulo, style = list(fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE)) %>%
      hc_plotOptions(pie = list(
        allowPointSelect = TRUE, colorByPoint = TRUE, colors = colores,
        dataLabels = list(
          enabled = TRUE, format = "<b>{point.name}</b>: {point.percentage:.1f} %",
          style = list(fontWeight = "bold", color = "black", fontSize = "18px")
          )
        )
      ) %>%
      hc_exporting(enabled = TRUE, filename = paste0("PlotTorta_", categoria)) %>%
      hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") %>%
      hc_legend(
        enabled = TRUE, align = "center", verticalAlign = "bottom",
        title = list(text = LegendTitle, style = list(textDecoration = "underline")),
        itemStyle = list(fontWeight = "bold", color = "black", fontSize = "18px")
      ) %>%
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
    Margen <- list(l = 50, r = 50, t = 110, b = 0) # l = left; r = right; t = top; b = bottom

    if (!missingArg(estilo) && estilo$ply.Legend == "inside") {
      PlotTorta <- plot_ly(TablaFinal,
        labels = ~Clase, values = ~Total, type = "pie",
        textposition = "inside", textinfo = "label+value+percent", # "label+percent"
        insidetextfont = list(color = "#FFFFFF", size = 20), hoverinfo = "label+value",
        insidetextorientation = "radial", # "horizontal", "radial", "tangential", "auto"
        marker = list(colors = colores, line = list(color = "#000000", width = 1.5))
        ) %>%
        layout(title = Title, showlegend = FALSE, autosize = TRUE, margin = Margen)
    } else {
      PlotTorta <- plot_ly(TablaFinal,
        labels = ~Clase, values = ~Total, type = "pie", textinfo = "label+percent",
        marker = list(colors = colores, line = list(color = "#FFFFFF", width = 1.5))
        ) %>%
        layout(title = Title, showlegend = TRUE, autosize = TRUE, margin = Margen)
    }

    PlotTorta <- PlotTorta %>%
      layout(annotations = append(ParmsCredits, list(
        showarrow = FALSE, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
        font = list(size = 12, color = "#2B908F")
        ))
      ) %>%
      config(locale = "es")
  }

  return(PlotTorta)
}
