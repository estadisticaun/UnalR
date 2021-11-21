#' Cree un diagrama de caja/boxplot dinámico y flexible con dos diferentes paquetes
#'
#' Esta función proporciona excelentes herramientas y opciones para la visualización
#' de un diagrama de caja y bigotes (*también conocido como boxplot*) dinámico
#' con el objetivo de representar gráficamente una serie numérica a través de sus
#' cuantiles. Dicho boxplot se puede representar usando dos diferentes librerías
#' que son `Highcharter` y `Plotly`, las cuales usan internamente `JavaScript`.
#'
#' @param datos Un data frame, no un vector numérico.
#' @param variable Una variable numérica dentro del data frame ingresado en `datos`.
#' @param grupo1 Una variable categórica dentro del data frame ingresado en `datos`.
#' @param grupo2 Otra variable categórica dentro del data frame ingresado en `datos`
#'   por si se desea segregar por otra clase el grupo principal.
#' @param outliers Si es `TRUE` (*valor predeterminado*) se mostrarán los puntos
#'   correspondientes a los datos atípicos, defínalo en `FALSE` si desea ocultar
#'   dichos puntos.
#' @param jitter Si es `TRUE` se agregará las observaciones de cada grupo con un
#'   poco de ruido aleatorio encima de las cajas, útil para mostrar la distribución
#'   subyacente de los datos. El valor por defecto es `FALSE`.
#' @param violin Si es `TRUE` se acompañará el boxplot con su diagrama de densidad,
#'   logrando ser más informativo al mostrar la distribución completa de los datos.
#'   Solo aplica para la librería `"plotly"`.
#' @param numericalVars Una lista (*ya sea creada con la sintaxis `base` o `tidy`*)
#'   con las variables numéricas dentro del data frame ingresado en `datos` con
#'   las que se desea crear un botón dinámico para desplazarse entre ellas fijando
#'   el grupo ingresado en `grupo1`.
#' @param ylim Vector numérico que especifica el límite inferior y superior,
#'   respectivamente, del eje `Y`. Si no se introduce algún valor se mostrará todo
#'   el rango disponible para dicho eje.
#' @param colores Cadena de caracteres indicando los colores con los cuales se
#'   deben colorear cada una de las trazas correspondiente a cada nivel del argumento
#'   `grupo1`. Si no se introduce algún vector se usará la paleta `rainbow` por defecto.
#' @param sizeOutlier Valor numérico que indica el tamaño de los puntos considerados
#'   como atípicos, por defecto se tiene un valor específico al que se le sumará
#'   el ingresado acá.
#' @param colOutlier Cadena de caracteres indicando el color de los puntos considerados
#'   como atípicos, por defecto se pintarán de un azul rey.
#' @param titulo Igual uso que en [Plot.Series()]
#' @param labelX Igual uso que en [Plot.Series()]
#' @param labelY Igual uso que en [Plot.Series()]
#' @param textBox Cadena de caracteres indicando el nombre de la serie numérica
#'   con la que se construye las cajas, necesario únicamente si se especifica
#'   solamente el `grupo1`, para el caso en que se tenga dos grupos no tendrá
#'   ningún efecto en el plot.
#' @param libreria Igual uso que en [Plot.Torta()]
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   de acuerdo con la librería especificada para graficar el boxplot y cuyo objetivo
#'   es personalizar pequeños detalles de éste.
#'   * `LegendTitle`, `hc.Tema` y `hc.Credits`: Igual uso que en [Plot.Series()]
#'   * `ply.Interaction`, `ply.LegendPosition` y `ply.Credits`: Igual uso que en
#'     [Plot.Series()]
#'
#' @details
#' El argumento `numericalVars` funciona solamente con la librería `"plotly"`,
#' pues la función de crear los botones dinámicos es procedente de dicha librería.
#'
#' @return
#' Retorna el boxplot (*objeto widget de HTML*) creado. La clase del objeto retornado
#' será un "htmlwidget" y dependiendo de la librería usada pertenecerá adicionalmente
#' a la clase "highchart" o "plotly".
#'
#' @examples
#' Txt <- "EVOLUCI\u00d3N DEL PUNTAJE EN EL EXAMEN DE ADMISI\u00d3N"
#' Msj <- "Aspirantes a pregrado (<i>no se incluye los datos at\u00edpicos</i>)"
#' if (require("pals")) {
#'   Plot.Boxplot(
#'     datos       = ejMiniAspirantesPre,
#'     variable    = PTOTAL,
#'     grupo1      = Serie,
#'     outliers    = FALSE,
#'     ylim        = c(0, 1000),
#'     colores     = jet(25),
#'     sizeOutlier = 1,
#'     colOutlier  = "#FF3366",
#'     titulo      = Txt,
#'     labelY      = "Puntaje",
#'     textBox     = "Score",
#'     libreria    = "highcharter",
#'     estilo      = list(hc.Tema = 2, hc.Credits = Msj)
#'   )
#' }
#'
#' Msj <- "Aspirantes a pregrado (<i>cada periodo se encuentra segregado por el tipo de admisi\u00f3n</i>)"
#' Plot.Boxplot(
#'   datos    = ejMiniAspirantesPre,
#'   variable = PTOTAL,
#'   grupo1   = Serie,
#'   grupo2   = TIPO_INS,
#'   outliers = TRUE,
#'   ylim     = c(0, 1000),
#'   colores  = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
#'   titulo   = Txt,
#'   labelY   = "Puntaje",
#'   libreria = "highcharter",
#'   estilo   = list(LegendTitle = "Programa:", hc.Tema = 6, hc.Credits = Msj)
#' )
#'
#' Plot.Boxplot(
#'   datos    = iris,
#'   variable = Sepal.Length,
#'   grupo1   = Species,
#'   violin   = TRUE,
#'   colores  = c("#FF1D58", "#FDB911", "#00E527"),
#'   titulo   = "BOXPLOT DE LA LONGITUD DEL S\u00c9PALO | IRIS DATASET",
#'   labelX   = "Especie",
#'   labelY   = "Longitud del S\u00e9palo",
#'   libreria = "plotly"
#' )
#'
#' Plot.Boxplot(
#'   datos       = ejMiniAspirantesPre,
#'   variable    = PTOTAL,
#'   grupo1      = Serie,
#'   grupo2      = TIPO_INS,
#'   jitter      = TRUE,
#'   ylim        = c(0, 1000),
#'   colores     = c("#00ABFF", "#F3224B", "#FCD116", "#29DF2C"),
#'   sizeOutlier = 0,
#'   colOutlier  = "#D3D3D3",
#'   titulo      = Txt,
#'   labelY      = "Puntaje",
#'   libreria    = "plotly",
#'   estilo      = list(
#'     LegendTitle = "Programa:", ply.Interaction = "closest",
#'     ply.LegendPosition = list(x = 0.16, y = -0.25, orientation = "h"),
#'     ply.Credits = list(x = 0.4, y = 0.95, text = Msj)
#'   )
#' ) -> Advertencia
#' suppressWarnings(print(Advertencia))
#'
#' if (require("dplyr")) {
#'   df <- ejSaberPro2020 %>%
#'     select(SEDE_NOMBRE_ADM, PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
#'            PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
#'            )
#'   Numericas <- vars(PUNT_RAZO_CUANT, PUNT_INGLES, PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR)
#' # Numericas <- c("PUNT_RAZO_CUANT", "PUNT_INGLES", "PUNT_LECT_CRIT", "PUNT_COMP_CIUD", "PUNT_COMU_ESCR")
#' }
#' misColores <- c(
#'   "#29ABE2", # AZUL CLARO  | Amazonia
#'   "#8CC63F", # VERDE       | Bogota
#'   "#CC241D", # ROJO        | Caribe
#'   "#0071BC", # AZUL VIVO   | Manizales
#'   "#F15A24", # NARANJA     | Medellin
#'   "#FBB03B", # AMARILLO    | Orinoquia
#'   "#93278F", # MORADO      | Palmira
#'   "#8A381A"  # GRIS        | Tumaco
#' )
#'
#' Plot.Boxplot(
#'   datos         = df,
#'   variable      = PUNTAJE_GLOBAL,
#'   grupo1        = SEDE_NOMBRE_ADM,
#'   numericalVars = Numericas,
#'   colores       = misColores,
#'   libreria      = "plotly"
#' )
#'
#' @export
#'
#' @import highcharter
#' @import plotly
#' @import dplyr
#' @importFrom methods missingArg
#' @importFrom grDevices rainbow
Plot.Boxplot <- function(datos, variable, grupo1, grupo2, outliers = TRUE,
                         jitter = FALSE, violin = FALSE, numericalVars,
                         ylim, colores, sizeOutlier = 0, colOutlier = "#08306B",
                         titulo = "", labelX = "Periodo", labelY = "", textBox = "",
                         libreria = c("highcharter", "plotly"), estilo = NULL) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos) || missingArg(variable) || missingArg(grupo1)) {
    stop("\u00a1Por favor introduzca un conjunto de datos, una variable num\u00e9rica y un grupo con los cuales se graficar\u00e1!", call. = FALSE)
  }
  if (!(is.character(titulo) && is.character(labelX) && is.character(labelY) && is.character(textBox))) {
    stop("\u00a1Los argumentos 'titulo', 'labelX', 'labelY' y 'textBox' deben ser una cadena de texto!", call. = FALSE)
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
  if (!missingArg(ylim)) {
    if (!(is.numeric(ylim) && length(ylim) == 2)) {
      stop("\u00a1Por favor introduzca un vector de longitud dos que definen los l\u00edmites del eje Y!", call. = FALSE)
    }
    yLim <- ylim
  } else { yLim <- NULL }

  # AJUSTES Y CONDICIONALES PRELIMINARES POR CONSIDERAR
  LegendTitle <- ifelse(is.null(estilo$LegendTitle), "", estilo$LegendTitle)
  Puntos      <- ifelse(jitter, "all", ifelse(outliers, "outliers", FALSE))

  if (!missingArg(grupo2)) {
    Levels <- datos %>% select({{ grupo2 }}) %>% distinct() %>% pull()
    if (!(missingArg(colores) || length(colores) == length(Levels))) {
      stop(paste0(
        "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
        "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(Levels)
      ), call. = FALSE)
    }
    if (missingArg(colores)) { colores <- rainbow(length(Levels), alpha = 0.7) }

    ColxPunto  <- FALSE
    ShowLegend <- TRUE
    Intento <- try(data_to_boxplot(
      data = datos, variable = {{ variable }},
      group_var = {{ grupo1 }}, group_var2 = {{ grupo2 }},
      add_outliers = outliers, color = colores
    ),
    silent = TRUE
    )
    if (any(class(Intento) == "try-error")) {
      dfBoxPlot <- data_to_boxplot(
        data = datos, variable = {{ variable }},
        group_var = {{ grupo1 }}, group_var2 = {{ grupo2 }},
        add_outliers = FALSE, color = colores
      )
    } else { dfBoxPlot <- Intento }
  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    TypeGroup <- "group"
    if (!violin) {
      Dots <- list(
        data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable), type = "box",
        color = rlang::enquo(grupo2), colors = colores, boxpoints = Puntos, pointpos = 0,
        jitter = 0.4, marker = list(color = colOutlier, size = 2 + sizeOutlier)
      )
    } else {
      Dots <- list(
        data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
        type = "violin", color = rlang::enquo(grupo2), colors = colores,
        box = list(visible = FALSE), meanline = list(visible = TRUE)
      )
    }
  } else {
    Levels <- datos %>% select({{ grupo1 }}) %>% distinct() %>% pull()
    if (!(missingArg(colores) || length(colores) == length(Levels))) {
      stop(paste0(
        "\u00a1El n\u00famero de colores ingresados en el vector 'colores' no corresponde con el n\u00famero de categor\u00edas a colorear!",
        "\n\tNo. colores ingresados = ", length(colores), " != ", "No. de categor\u00edas = ", length(Levels)
      ), call. = FALSE)
    }
    if (missingArg(colores)) { colores <- rainbow(length(Levels), alpha = 0.7) }

    ColxPunto  <- TRUE
    ShowLegend <- FALSE
    dfBoxPlot  <- data_to_boxplot(
      data = datos, variable = {{ variable }}, group_var = {{ grupo1 }},
      add_outliers = outliers, name = textBox
    )
  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    TypeGroup <- NULL
    if (!violin) {
      Dots <- list(
        data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
        type = "box", color = rlang::enquo(grupo1), colors = colores,
        name = textBox, showlegend = FALSE, boxpoints = Puntos, pointpos = 0, jitter = 0.4,
        marker = list(color = colOutlier, size = 2 + sizeOutlier)
      )
    } else {
      Dots <- list(
        data = datos, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
        type = "violin", color = rlang::enquo(grupo1), colors = colores, name = textBox,
        showlegend = FALSE, box = list(visible = FALSE), meanline = list(visible = TRUE)
      )
    }
  }

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

    PlotBoxPlot <- highchart()   %>%
      hc_chart(type = "boxplot") %>%
      hc_xAxis(
        type = "category", title = list(
          text = labelX, offset = 70, style = list(
            fontWeight = "bold", fontSize = "18px", color = "black"
          )
        ),
        align = "center", lineColor = "#787878", opposite = FALSE,
        labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_add_series_list(dfBoxPlot) %>%
      hc_plotOptions(
        boxplot = list(colorByPoint = ColxPunto, colors = colores),
        series = list(marker = list(fillColor = colOutlier, radius = 1.5 + sizeOutlier))
      ) %>%
      hc_title(text = titulo, style = list(
        fontWeight = "bold", fontSize = "22px", color = "#333333", useHTML = TRUE
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = labelY, offset = 70, style = list(
            fontWeight = "bold", fontSize = "18px", color = "black"
          )
        ),
        lineColor = "#787878", opposite = FALSE, lineWidth = 1, min = yLim[1], max = yLim[2],
        labels = list(style = list(fontWeight = "bold", color = "black", fontSize = "18px"))
      ) %>%
      hc_credits(enabled = TRUE, text = "DNPE", href = "http://estadisticas.unal.edu.co/home/") %>%
      # https://github.com/jbkunst/highcharter/issues/331
      hc_exporting(enabled = TRUE, filename = paste0("PlotBoxPlot_", as_label(enquo(grupo1)))) %>%
      hc_legend(
        enabled = ShowLegend, align = "center", verticalAlign = "bottom", layout = "horizontal",
        title = list(text = LegendTitle, style = list(textDecoration = "underline")),
        x = 42, y = 0, itemStyle = list(
          fontWeight = "bold", color = "black", fontSize = "18px"
        )
      ) %>%
      hc_add_theme(ThemeHC)

    if (!(missingArg(estilo) || is.null(estilo$hc.Credits))) {
      PlotBoxPlot <- PlotBoxPlot %>%
        hc_subtitle(text = estilo$hc.Credits, align = "left", style = list(color = "#2B908F", fontWeight = "bold"))
    }
  } else if (libreria == "plotly") {
    if (!missingArg(numericalVars)) {
      df <- as.data.frame(datos)
      CrearBotones <- function(df, listVars) {
        lapply(listVars,
          FUN = function(varName, df) {
            boton <- list(
              method = "restyle", args = list("y", list(df[, varName])),
              label = sprintf("Mostrar: %s", varName)
            )
          },
          df
        )
      }
      Dots <- list(
        data = df, x = rlang::enquo(grupo1), y = rlang::enquo(variable),
        type = "violin", color = rlang::enquo(grupo1), colors = colores,
        box = list(visible = TRUE), meanline = list(visible = TRUE)
      )
    }
  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
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

    Hovermode <- ifelse(!(missingArg(estilo) || is.null(estilo$ply.Interaction)),
      estilo$ply.Interaction, ifelse(missingArg(grupo2), "x", "closest")
    )

    # Arial | Open Sans | Courier New, monospace
    FamilyAxis  <- list(family = "Old Standard TT, serif", size = 16, color = "#525252")
    FamilyTitle <- list(family = "Open Sans", size = 24, color = "#333333")

    Title <- list(text = paste0("<b>", titulo, "</b>"), font = FamilyTitle, y = 0.96)
    Xaxis <- list(
      title = paste0("<i>", labelX, "</i>"),
      zeroline  = FALSE,
      showline  = TRUE,
      showgrid  = FALSE,
      linecolor = "#787878",
      linewidth = 2.5,
      autotick  = FALSE,
      ticks     = "outside",
      tickwidth = 2.5,
      ticklen   = 10,
      tickcolor = "#CCCCCC",
      tickangle = -45,
      tickfont  = FamilyAxis,
      showticklabels = TRUE
    )
    Yaxis <- list(
      title     = labelY,
      zeroline  = TRUE,
      showline  = TRUE,
      showgrid  = TRUE,
      linecolor = "#787878",
      linewidth = 3,
      range     = yLim,
      tickfont  = FamilyAxis,
      showticklabels = TRUE,
      separatethousands = TRUE
    )

    PlotBoxPlot <- do.call(plot_ly, Dots) %>%
      layout(
        boxmode = TypeGroup, violinmode = TypeGroup, title = Title,
        xaxis = Xaxis, yaxis = Yaxis, autosize = TRUE, showlegend = TRUE,
        legend = append(ParmsLegend, list(traceorder = "normal", title = list(text = paste0("<b>", LegendTitle, "</b>")))),
        hovermode = Hovermode, annotations = append(ParmsCredits, list(
          showarrow = FALSE, xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
          font = list(size = 12, color = "#CCCCCC")
        ))
      ) %>%
      config(locale = "es")

    if (!missingArg(numericalVars)) {
      PlotBoxPlot <- PlotBoxPlot %>%
        layout(updatemenus = list(list(buttons = CrearBotones(df, vars2vec(numericalVars)))))
    }
  }

  return(PlotBoxPlot)
}
