#' Cree un gráfico de barras apiladas dinámico y flexible
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un gráfico de barras apiladas con el objetivo de mostrar el tamaño relativo
#' (*como porcentaje*) de una variable de categorías, subdivididas por colores en
#' función de un subgrupo. Dicha gráfica se va a representar usando la librería
#' `Highcharter`, la cual usa internamente `JavaScript`.
#'
#' @param datos Un data frame, no un vector numérico.
#' @param categoria Igual uso que en [Plot.Series()]
#' @param anos Igual uso que en [Plot.Torta()]
#' @param periodo Igual uso que en [Plot.Torta()]
#' @param colores Igual uso que en [Plot.Series()]
#' @param titulo Igual uso que en [Plot.Series()]
#' @param addPeriodo Igual uso que en [Plot.Torta()]
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   para graficar las barras apiladas y cuyo objetivo es personalizar pequeños
#'   detalles de éste.
#'   * `LegendTitle`: Cadena de caracteres indicado un título para la leyenda
#'     (\emph{diferentes niveles del argumento `varPrincipal`}).
#'   * `hc.Tema` y `hc.Credits`: Igual uso que en [Plot.Series()]
#'
#' @return
#' Retorna el diagrama de barras apiladas (*objeto widget de HTML*) creado. La
#' clase del objeto retornado será un "htmlwidget" y adicionalmente pertenecerá
#' a la clase "highchart".
#'
#' @examples
#' Msj <- "Se considera \u00fanicamente los valores obtenidos en el primer periodo acad\u00e9mico de cada a\u00f1o."
#' Plot.Apiladas(
#'   datos     = ejConsolidadoGrad,
#'   categoria = "NIVEL",      # Pruebe también con -> unique(ejConsolidadoGrad$Variable)
#'   anos      = c(2018:2020),
#'   periodo   = 1,
#'   colores   = c("#FFA700", "#C10AA1", "#01CDFE", "#00FF44", "#FF0040"),
#'   titulo    = "BARRAS APILADAS EN FUNCI\u00d3N DEL NIVEL ACAD\u00c9MICO Y EL A\u00d1O",
#'   estilo    = list(LegendTitle = "NIVEL ACAD\u00c9MICO:", hc.Tema = 4, hc.Credits = Msj)
#' )
#'
#' @export
#'
#' @import highcharter
#' @import dplyr
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Apiladas <- function(datos, categoria, anos, periodo, colores,
                          titulo = "", addPeriodo = TRUE, estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos)) {
    stop("\u00a1Por favor introduzca un conjunto de datos!", call. = FALSE)
  }
  if (!is.character(titulo)) {
    stop("\u00a1El argumento 'titulo' deben ser una cadena de texto!", call. = FALSE)
  }
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)

  # GENERACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA GRÁFICA
  DataFrame <- ungroup(datos) %>% filter(Variable == categoria, is.na(Clase) != TRUE)

  if (!(missingArg(anos) || missingArg(periodo))) {
    TablaFinal <- DataFrame %>% filter(YEAR %in% anos, SEMESTRE %in% periodo)
  } else {
    if (missingArg(anos) && missingArg(periodo)) {
      TablaFinal <- DataFrame
    } else if (missingArg(anos)) {
      TablaFinal <- DataFrame %>% filter(SEMESTRE %in% periodo)
    } else {
      TablaFinal <- DataFrame %>% filter(YEAR %in% anos)
    }
  }

  TablaFinal <- TablaFinal %>% select(-Variable, -SEMESTRE)
  categorias <- TablaFinal %>% select(Clase) %>% distinct() %>% pull()

  df <- TablaFinal %>%
    left_join(
      TablaFinal %>% group_by(YEAR) %>% summarise(sumYear = sum(Total)),
      by = "YEAR"
    ) %>%
    mutate(Valor = round(Total / sumYear * 100, 4)) %>% select(-Total, -sumYear)

  # CREACIÓN DEL PLOT RETORNAR
  if (!(missingArg(colores) || length(colores) == length(categorias))) {
    stop(paste0(
      "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
      "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(categorias)
    ), call. = FALSE)
  }
  if (missingArg(colores)) { colores <- rainbow(length(categorias), alpha = 0.7) }

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

  PlotApiladas <- df %>%
    hchart("column", hcaes(x = "YEAR", y = "Valor", group = "Clase")) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_title(text = titulo, style = list(
      fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE
      )
    ) %>%
    hc_xAxis(type = "category", labels = list(
      style = list(fontWeight = "bold", color = "black", fontSize = "18px")
      )
    ) %>%
    hc_yAxis(
      title = list(text = "Porcentaje", style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
      labels = list(format = "{value}%", style = list(fontWeight = "bold", color = "black", fontSize = "18px")),
      min = 0, max = 100
    ) %>%
    hc_colors(colores) %>%
    hc_tooltip(pointFormat = '<span style="color:{series.color}">\u25CF </span><b>{series.name}:</b> {point.Valor:.2f}%<br/>') %>%
    hc_exporting(enabled = TRUE, filename = paste0("PlotApiladas_", str_to_title(categoria))) %>%
    hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") %>%
    hc_legend(
      enabled = TRUE, align = "center", verticalAlign = "bottom",
      title = list(text = LegendTitle, style = list(textDecoration = "underline")),
      itemStyle = list(fontWeight = "bold", color = "black", fontSize = "18px")
    ) %>%
    hc_add_theme(ThemeHC)

  if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
    PlotApiladas <- PlotApiladas %>%
      hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
  }

  return(PlotApiladas)
}
