#' Cree un widget para visualizar datos geográficos en un mapa interactivo usando
#' el paquete `Leaflet`
#'
#' Esta función está planeada para facilitar la creación de mapas interactivos
#' compatible con plataformas móviles y de escritorio, además de estar diseñada
#' pensando en la simplicidad y el rendimiento. Esta utilidad produce mapas que
#' tienen controles para hacer zoom, desplazarse y alternar capas y puntos entre
#' mostrar y ocultar. Igualmente permite incrustar mapas en webs, documentos
#' `R Markdown` y aplicaciones `Shiny`. Todo lo anterior basado enteramente en la
#' librería `Leaflet`, la cual es la biblioteca `JavaScript` de código abierto más
#' popular para mapas interactivos.
#'
#' @param depto Vector numérico que contiene los códigos de los departamentos, de
#'   acuerdo con la codificación de la División Político Administrativa de Colombia
#'   (*DIVIPOLA*) dispuesta por el `DANE`.
#' @param mpio Vector numérico que contiene los códigos de los municipios, de acuerdo
#'   con la codificación de la División Político Administrativa de Colombia (*DIVIPOLA*)
#'   dispuesta por el `DANE`.
#' @param estadistico Cadena de caracteres que indica el estadístico a graficar en
#'   el mapa. Los valores permitidos son "Conteo" (valor predeterminado), "Promedio",
#'   "Mediana", "Varianza", "SD", "Min" y "Max". Para el caso en que la estadística
#'   a calcular sea el conteo no es necesario (*no se usará*) especificar dicho vector.
#' @param variable Vector numérico cuya función es servir como variable auxiliar
#'   con la cual se calculará el estadístico previamente seleccionado.
#' @param tipo Cadena de caracteres que indica el tipo de mapa a graficar. Los valores
#'   permitidos son "Deptos", "SiNoMpios", "Mpios" y "DeptoMpio" (valor predeterminado).
#'   Se emparejará parcialmente.
#' @param naTo0 Si es `TRUE` (valor predeterminado) los valores introducidos como
#'   `NA` (*not available*) se cambiarán por el valor de 0. Ajústelo a `FALSE` para
#'   que no se realice tal cambio y mostrar en la caja de información la leyenda
#'   "Sin Información".
#' @param centroideMapa Cadena de caracteres indicando el departamento que servirá
#'   de centroide al momento de graficar el mapa. El valor por defecto es "CUNDINAMARCA".
#'   Se emparejará parcialmente.
#' @param zoomMapa Valor numérico que indica el nivel de zoom del mapa (usado por
#'   la función [setView()][leaflet::setView()]). El valor por defecto es 6, entre
#'   mayor sea su valor más zoom se aplicará al mapa.
#' @param titulo Cadena de caracteres indicando la segregación que presenta el mapa
#'   y el periodo al que hace referencia éste, separados por un espacio, por ejemplo,
#'   "Admitidos 2021-I".
#' @param baldosas Vector de caracteres indicando los mapas base con los que se
#'   realizará el mapa, sean los popularizados por Google Maps o por terceros. Los
#'   valores aceptados son los admitidos por la función [addProviderTiles()][leaflet::addProviderTiles()],
#'   asimismo los valores por defecto son `c("CartoDB.Positron", "Esri.WorldStreetMap", "Esri.NatGeoWorldMap")`,
#'   algunos otros valores pueden ser:
#'
#'   * "Stamen.Toner"
#'   * "Stamen.TonerLite"
#'   * "Stamen.TonerLines"
#'   * "Stamen.Watercolor"
#'   * "Stamen.TonerHybrid"
#'   * "Esri.DeLorme"
#'   * "Esri.WorldTerrain"
#'   * "Esri.WorldShadedRelief"
#'   * "Esri.WorldPhysical"
#'   * "Esri.OceanBasemap"
#'   * "Esri.WorldGrayCanvas"
#'
#'   La lista completa la puede consultar [aquí](http://leaflet-extras.github.io/leaflet-providers/preview/index.html)
#' @param cortes Vector numérico indicando los cortes con los cuales se crearán los
#'   intervalos. No aplica para el tipo de mapa "SiNoMpios", pues este es binario.
#'   Para el tipo de mapa "DeptoMpio" se debe pasar una lista de la siguiente manera
#'   `list(Deptos = c(), Mpios = c())`, pues requiere dos cortes, uno para departamentos
#'   y otro para municipios.
#' @param colores Vector de caracteres indicando los colores para cada uno de los
#'   intervalos con los que cuenta el mapa. Si no se introduce algún vector se usará
#'   una paleta predetermina dependiendo del tipo de mapa.
#' @param showSedes Si es `TRUE` (valor predeterminado) en el control de capas
#'   (usado en la función [addLayersControl()][leaflet::addLayersControl()]) aparecerá
#'   el grupo destinado a mostrar o no la ubicación de las distintas sedes de la
#'   Universidad Nacional de Colombia. Ajústelo a `FALSE` para que en el control de
#'   capas no aparezca dicha opción.
#' @param colSedes Vector de caracteres (*de longitud 9*) indicando los colores del
#'   icono de ubicación de las distintas sedes de la Universidad Nacional de Colombia.
#'   Los colores permitidos son los que acepta la función [makeAwesomeIcon()][leaflet::makeAwesomeIcon()],
#'   es decir, "red", "darkred", "lightred", "orange", "beige", "green", "darkgreen",
#'   "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink",
#'   "cadetblue", "white", "gray", "lightgray", "black".
#' @param opacidad Un número entre \eqn{[0, 1]} que indica la opacidad de las capas.
#' @param colBorde Cadena de caracteres indicando el color del borde de los polígonos
#'   al momento de pasar el cursos sobre él.
#' @param compacto Si es `TRUE` (valor predeterminado) el control de capas se
#'   representará como un icono que se expande cuando se coloca el cursor sobre él.
#'   Ajústelo a `FALSE` para que el control de capas siempre aparezca en su estado
#'   expandido.
#' @param textSize Valor numérico que indica el tamaño del texto de las etiquetas
#'   de los municipios. El valor para los departamentos será \eqn{+2px}.
#' @param limpio Si es `FALSE` (valor predeterminado) se mostrará el MiniMapa, la
#'   barra de escala y los botones para ver en pantalla completa, retornar zoom y
#'   localización. Ajústelo a `TRUE` si desea omitir dichas herramientas adicionales
#'   al mapa.
#'
#' @details
#' Los vectores `depto` y `mpio` introducidos hacen referencia a atributos atómicos,
#' es decir, la pareja formada por `(depto, mpio)` debe corresponder a un individuo.
#' En los argumentos no se acepta la entrada de objetos espaciales o polígonos.
#'
#' @return
#' Retorna el mapa (objeto widget de HTML) creado mediante `Leaflet`, el cual pertenece
#' a la clase "leaflet" y "htmlwidget".
#'
#' @examples
#' if (require("dplyr")) {
#'   Graduados <- Graduados %>%
#'     select(Code_Dept    = COD_DEP_NAC,
#'            Code_Mun     = COD_CIU_NAC,
#'            Departamento = DEP_NAC,
#'            Municipio    = CIU_NAC,
#'            Longitud     = LON_CIU_NAC,
#'            Latitud      = LAT_CIU_NAC)
#' }
#'
#' Plot.Mapa(depto    = Graduados$Code_Dept,
#'           mpio     = Graduados$Code_Mun,
#'           tipo     = "SiNoMpios",
#'           titulo   = "Graduados 2020-II",
#'           baldosas = c("Esri.WorldPhysical", "Esri.DeLorme", "Esri.WorldShadedRelief", "Esri.WorldTerrain",
#'                        "Stamen.Toner", "Stamen.TonerLite", "Stamen.TonerLines", "Esri.OceanBasemap"),
#'           colores  = c("#FF0071", "#00BCB5"),
#'           colSedes = rep("green", 9),
#'           opacidad = 0.4,
#'           compacto = FALSE,
#'           textSize = 16,
#'           limpio   = TRUE
#'           )
#'
#' Plot.Mapa(depto   = Graduados$Code_Dept,
#'           mpio    = Graduados$Code_Mun,
#'           tipo    = "DeptoMpio",
#'           titulo  = "Graduados 2020-II",
#'           cortes  = list(Deptos = c(0, 10, 20, 50, 500, Inf),
#'                          Mpios  = c(0, 1, 5, 10, 100, Inf)),
#'           colores = list(Deptos = c("#6812F2", "#5769F6", "#F6ED0D", "#EE6115", "#EC2525"),
#'                          Mpios  = c("#E7F15D", "#ACBD37", "#E15E32", "#A82743", "#5C323E"))
#'           )
#'
#' if (require("UnalData") && require("dplyr") && require("magrittr")) {
#'   UnalData::SaberPro %>%
#'     select(Code_Dept     = COD_DEP_NAC,
#'            Code_Mun      = COD_CIU_NAC,
#'            Edad          = EDAD_MOD,
#'            PBM           = PBM_ORIG,
#'            ScoreGlobal   = PUNTAJE_GLOBAL,
#'            ScoreCompCiud = PUNT_COMP_CIUD,
#'            ScoreComuEscr = PUNT_COMU_ESCR,
#'            ScoreIngles   = PUNT_INGLES,
#'            ScoreLectCrit = PUNT_LECT_CRIT,
#'            ScoreRazCuant = PUNT_RAZO_CUANT
#'            ) %$%
#'     Plot.Mapa(depto         = Code_Dept,
#'               mpio          = Code_Mun,
#'               estadistico   = "Promedio",
#'               variable      = ScoreGlobal,
#'               tipo          = "DeptoMpio",
#'               titulo        = "P.Global 2020-2",
#'               naTo0         = FALSE,
#'               centroideMapa = "ANTIOQUIA",
#'               zoomMapa      = 8,
#'               cortes        = list(Deptos = c(0, 155, 170, 180, 185, Inf),
#'                                    Mpios  = c(0, 50, 178, 200, 250, Inf)),
#'               colores       = list(Deptos = c("#6812F2", "#5769F6", "#F6ED0D", "#EE6115", "#EC2525"),
#'                                    Mpios  = c("#E7F15D", "#ACBD37", "#E15E32", "#A82743", "#5C323E")),
#'               showSedes     = FALSE
#'               )
#' }
#'
#' @export
#'
#' @import leaflet
#' @import dplyr
#' @importFrom leaflet.extras addFullscreenControl addSearchFeatures searchFeaturesOptions
#' @importFrom tidyr replace_na
#' @importFrom stringr str_to_title
#' @importFrom htmltools HTML
#' @importFrom methods missingArg
Plot.Mapa <- function(depto, mpio, estadistico = c("Conteo", "Promedio", "Mediana", "Varianza", "SD", "Min", "Max"),
                      variable = NA, tipo = c("Deptos", "SiNoMpios", "Mpios", "DeptoMpio"), titulo, naTo0 = TRUE,
                      centroideMapa = "CUNDINAMARCA", zoomMapa = 6, baldosas, cortes, colores, showSedes = TRUE,
                      colSedes, opacidad = 0.7, colBorde, compacto = TRUE, textSize = 10, limpio = FALSE) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(depto) || missingArg(mpio)) {
    stop('\u00a1Por favor introduzca tanto el vector "depto" como el "mpio"!', call. = FALSE)
  }
  if (length(depto) != length(mpio)) {
    stop('\u00a1Las longitudes de los vectores ingresados ("depto" y "mpio") no coinciden, no se puede reciclar los valores por la naturaleza de estos!', call. = FALSE)
  }

  Statistic <- match.arg(estadistico)
  if (Statistic == "Conteo") {
    Datos <- tibble("Code_Dept" = depto, "Code_Mun" = mpio) %>% filter(!is.na(Code_Dept))
    if (!missingArg(variable)) {
      warning(paste0("\u00a1Para el estad\u00edstico conteo no es necesario especificar una variable con la cual calcularse. Se omitir\u00e1 la variable",
                     ' "', deparse(substitute(variable)), '" ingresada!'), call. = FALSE)
    }
  } else {
    if (missingArg(variable)) {
      stop(paste0("\u00a1Por favor ingrese una variable auxiliar con la cual se calcular\u00e1 el/la ", tolower(Statistic), "!"), call. = FALSE)
    } else if (length(variable) != length(depto)) {
      stop('\u00a1Las longitudes de los vectores ingresados ("depto" y "mpio") con la variable auxiliar no coinciden!', call. = FALSE)
    } else if (!is.numeric(variable)) {
      stop("\u00a1El vector ingresado como variable contiene valores no num\u00e9ricos!", call. = FALSE)
    }
    Datos <- tibble("Code_Dept" = depto, "Code_Mun" = mpio, "Variable" = variable) %>% filter(!is.na(Code_Dept))
  }

  '%NotIN%' <- Negate('%in%')
  tipo <- tolower(tipo); centroideMapa <- toupper(centroideMapa)
  if (tipo %NotIN% c("deptos", "sinompios", "mpios", "deptompio")) {
    stop('\u00a1Por favor introduzca un tipo de mapa correcto! Las opciones son: "Deptos", "SiNoMpios", "Mpios" y "DeptoMpio"', call. = FALSE)
  }
  if (!missingArg(titulo)) {
    if (!is.character(titulo)) {
      stop("\u00a1El argumento 'titulo' debe ser una cadena de texto!", call. = FALSE)
    }
  } else { titulo <- "\u00bf ?" }
  if (missingArg(baldosas)) {
    Baldosas        <- c("CartoDB.Positron", "Esri.WorldStreetMap", "Esri.NatGeoWorldMap")
    Baldosas.names  <- c("Ligero", "Street", "Sat\u00e9lite<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;NatGeo")
  } else { Baldosas <- Baldosas.names <- baldosas }
  if (!missingArg(colSedes)) {
    if (length(colSedes) != 9L) {
      stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colSedes' no corresponde con el n\u00famero de sedes presentes!",
                  "\n\t - Ingrese un vector con 9 colores correspondientes a las sedes:",
                  "\n\t -- Medell\u00edn, Bogot\u00e1, Manizales, De La Paz, Tumaco, Palmira, Orinoqu\u00eda, Caribe y Amazonas, respectivamente."), call. = FALSE)
    } else { Icons_Col <- colSedes }
  } else { Icons_Col <- c("orange", "green", "darkblue", "purple", "gray", "darkpurple", "lightred", "red", "blue") }
  if (!(is.numeric(zoomMapa) && is.numeric(opacidad) && is.numeric(textSize))) {
    stop("\u00a1Los argumentos 'zoomMapa', 'opacidad'y 'textSize' deben ser un valor num\u00e9rico!", call. = FALSE)
  }
  opacidadClic <- ifelse(opacidad!=1, opacidad+0.2, opacidad)
  textDeptos   <- paste0(textSize+2, "px"); textMpios <- paste0(textSize, "px")
  if (!missing(colBorde)) {
    if (!is.character(colBorde)) {
      stop("\u00a1El argumento 'colBorde' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(255, 0, 0))!", call. = FALSE)
    }
  }
  if (!(is.logical(naTo0) && is.logical(showSedes) && is.logical(compacto) && is.logical(limpio))) {
    stop("\u00a1Los argumentos 'naTo0', 'showSedes', 'compacto' y 'limpio' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }

  Mensaje <- paste0("\u00a1El argumento 'colores' no tiene la longitud correcta respecto al argumento 'cortes' ingresado, o viceversa!",
                    "\n\t - Recuerde que si realiza (n) cortes necesita (n-1) colores para cada intervalo creado.",
                    "\n\t -- Por ejemplo, si los cortes son c(0, 50, Inf) necesitar\u00e1 2 colores para dichos intervalos.")

  # Creación de una función auxiliar que retorne los municipios ingresados que presentan NA's en la variable auxiliar.
  Aux <- function(data, group, var) {
    Temp <- data %>% group_by(eval(parse(text = group))) %>% summarise("n" = sum(is.na(eval(parse(text = var)))))
    NAs  <- Temp %>% select(n) %>% sum()
    ValuesNAs <- Temp %>% filter(n > 0) %>% select(-n) %>% pull()
    return(list("NAs" = NAs, "ValuesNAs" = paste(ValuesNAs, collapse = ", ")))
  }

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  Msj <- 'Map created by <a href="https://github.com/JeisonAlarcon">Jeison Alarc\u00f3n</a> &mdash; Data source: &copy; <a href="http://estadisticas.unal.edu.co/home/">DNPE</a>'
  # Code_Mun != 25001 -> Cundinamarca no tiene capital, pues Bogotá D.C. está en otro departamento.
  Capitales <- Cabeceras %>% filter(Code_Mun %% 1000 == 1, Code_Mun != 25001)

  # Hace referencia al n* con el cual se calculará la estadística, no al n total que puede incluir NA's.
  #   ■ Para el caso del conteo es diferente, pues para calcular este no se requiere de ninguna variable auxiliar.
  if (naTo0) {
    GroupBy_Dept  <- Datos %>% group_by(Code_Dept) %>% replace(., is.na(.), 0)
    GroupBy_Mpio  <- Datos %>% group_by(Code_Mun)  %>% replace(., is.na(.), 0)

    Conteo_ByDept <- GroupBy_Dept %>% summarise("n" = n())
    Conteo_ByMun  <- GroupBy_Mpio %>% summarise("n" = n())
  } else {
    GroupBy_Dept  <- Datos %>% group_by(Code_Dept)
    GroupBy_Mpio  <- Datos %>% group_by(Code_Mun)
    if (Statistic != "Conteo") {
      Conteo_ByDept <- GroupBy_Dept %>% summarise("n" = sum(!is.na(Variable)))
      Conteo_ByMun  <- GroupBy_Mpio %>% summarise("n" = sum(!is.na(Variable)))
    } else {
      Conteo_ByDept <- GroupBy_Dept %>% summarise("n" = n())
      Conteo_ByMun  <- GroupBy_Mpio %>% summarise("n" = n())
    }
  }

  Datos_ByDept <- switch(Statistic,
                         Conteo   = GroupBy_Dept %>% summarise("Statistic" = n()),
                         Promedio = GroupBy_Dept %>% summarise("Statistic" = mean(Variable,   na.rm = TRUE)),
                         Mediana  = GroupBy_Dept %>% summarise("Statistic" = median(Variable, na.rm = TRUE)),
                         Varianza = GroupBy_Dept %>% summarise("Statistic" = var(Variable,    na.rm = TRUE)),
                         SD       = GroupBy_Dept %>% summarise("Statistic" = sd(Variable,     na.rm = TRUE)),
                         Min      = GroupBy_Dept %>% summarise("Statistic" = min(Variable,    na.rm = TRUE)),
                         Max      = GroupBy_Dept %>% summarise("Statistic" = max(Variable,    na.rm = TRUE))
                         )
  Datos_ByMun  <- switch(Statistic,
                         Conteo   = GroupBy_Mpio %>% summarise("Statistic" = n()),
                         Promedio = GroupBy_Mpio %>% summarise("Statistic" = mean(Variable,   na.rm = TRUE)),
                         Mediana  = GroupBy_Mpio %>% summarise("Statistic" = median(Variable, na.rm = TRUE)),
                         Varianza = GroupBy_Mpio %>% summarise("Statistic" = var(Variable,    na.rm = TRUE)),
                         SD       = Conteo_ByMun %>% summarise("Statistic" = sd(Variable,     na.rm = TRUE)),
                         Min      = GroupBy_Mpio %>% summarise("Statistic" = min(Variable,    na.rm = TRUE)),
                         Max      = GroupBy_Mpio %>% summarise("Statistic" = max(Variable,    na.rm = TRUE))
                         )

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  # Carga del Departamentos
  Polygons_Depto <- Depto_Final
  Polygons_Depto@data$Statistic <- Polygons_Depto@data %>% left_join(Datos_ByDept,  by = "Code_Dept") %>% select(Statistic) %>% pull()
  Polygons_Depto@data$n         <- Polygons_Depto@data %>% left_join(Conteo_ByDept, by = "Code_Dept") %>% select(n) %>% pull()

  # Carga de Municipios
  Polygons_Mpio <- Mpio_Final
  Polygons_Mpio@data$Statistic <- Polygons_Mpio@data %>% left_join(Datos_ByMun,  by = "Code_Mun") %>% select(Statistic) %>% pull()
  Polygons_Mpio@data$n         <- Polygons_Mpio@data %>% left_join(Conteo_ByMun, by = "Code_Mun") %>% select(n) %>% pull()

  if (naTo0) {
    Polygons_Depto@data$Statistic <- Poly_Dept_ShowStatistic <- Polygons_Depto@data$Statistic %>% replace(., is.na(.), 0)
    Polygons_Depto@data$n         <- Poly_Dept_ShowN         <- Polygons_Depto@data$n %>% replace(., is.na(.), 0)

    Polygons_Mpio@data$Statistic <- Poly_Mpio_ShowStatistic <- Polygons_Mpio@data$Statistic %>% replace(., is.na(.), 0)
    Polygons_Mpio@data$n         <- Poly_Mpio_ShowN         <- Polygons_Mpio@data$n %>% replace(., is.na(.), 0)

    Comodin <- "%g"
  } else {
    Poly_Dept_ShowStatistic <- Polygons_Depto@data$Statistic %>% replace(., is.na(.), "Sin Información")
    Poly_Dept_ShowN         <- Polygons_Depto@data$n %>% replace(., is.na(.), "Sin Información")

    Poly_Mpio_ShowStatistic <- Polygons_Mpio@data$Statistic %>% replace(., is.na(.), "Sin Información")
    Poly_Mpio_ShowN         <- Polygons_Mpio@data$n %>% replace(., is.na(.), "Sin Información")

    Comodin <- "%s"
  }

  # Corrección de algunos municipios cuyo nombre es hominino de otro.
  Homonimos <- Polygons_Mpio@data %>% group_by(Municipio) %>%
    summarise(veces = n()) %>% filter(veces > 1)

  Polygons_Mpio@data <- Polygons_Mpio@data %>%
    mutate(Municipio = if_else(condition = Municipio %in% Homonimos$Municipio,
                               true  = paste(Municipio, substring(Departamento, 1, 3)),
                               false = Municipio
                               )
           )

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  # Ubicación de los centroides correspondientes a cada departamento (extraídos del objeto 'Polygons_Depto' y no de 'Capitales')
  #   ■ El centroide de Colombia corresponde al centroide del departamento de Cundinamarca
  NumDept <- length(Polygons_Depto); Coordenadas <- sp::coordinates(Polygons_Depto)
  Centroides_Deptos <- data.frame("Departamento" = Polygons_Depto@data$Departamento, "Lon" = Coordenadas[,1], "Lat" = Coordenadas[,2])

  NumMpios    <- length(Polygons_Mpio)
  #   ■ Corrección del centroide de los municipios, pues en algunos casos no éste se sale del polígono
  Coordenadas <- suppressWarnings(sf::st_point_on_surface(sf::st_as_sf(Polygons_Mpio)))
  Coordenadas <- data.frame(sf::st_coordinates(Coordenadas))
  Centroides_Mpios <- tibble("Municipio" = Polygons_Mpio@data$Municipio, "Lon" = Coordenadas$X, "Lat" = Coordenadas$Y)

  Centroide_Col <- Centroides_Deptos %>% filter(Departamento == centroideMapa)

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  # Filtrar sedes de la Universidad Nacional de Colombia
  Sedes <- Cabeceras %>%
    filter(Code_Mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>%
    mutate(Sede = c("Medell\u00edn", "Bogot\u00e1", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoqu\u00eda", "Caribe", "Amazonas"))
  Icons_Sedes <- makeAwesomeIcon(markerColor = Icons_Col, iconColor = "white", fontFamily = "Leonardian", text = "un")

  # Extracción de la segregación y el periodo en cuestión del título ingresado
  TitleSplit  <- strsplit(titulo, "\\s+"); Segregacion <- TitleSplit[[1]][1]; Periodo <- TitleSplit[[1]][2]
  TitleDepto  <- paste0("<center>", Segregacion, " por<br/>departamento<br/>", Periodo, "</center>")
  TitleMpio   <- paste0("<center>", Segregacion, " por<br/>municipio<br/>", Periodo, "</center>")

  # Labels
  Labels_Sedes <- sprintf("<strong>%s %s</strong>", "Sede", Sedes$Sede) %>% lapply(htmltools::HTML)
  if (Statistic != "Conteo") {
    Labels_Deptos <- sprintf(paste0("<strong> %s </strong> <br>", Statistic, ": ", Comodin, " <br> n: ", Comodin),
                             Polygons_Depto@data$Departamento, Poly_Dept_ShowStatistic, Poly_Dept_ShowN) %>%
      lapply(htmltools::HTML)
    Labels_Mpios  <- sprintf(paste0("<strong> %s </strong> (%s) <br>", Statistic, ": ", Comodin, " <br> n: ", Comodin),
                             Polygons_Mpio@data$Municipio, Polygons_Mpio@data$Departamento,
                             Poly_Mpio_ShowStatistic, Poly_Mpio_ShowN) %>%
      lapply(htmltools::HTML)
  } else {
    Labels_Deptos <- sprintf(paste0("<strong> %s </strong> <br>", Comodin, " ", tolower(Segregacion)),
                             Polygons_Depto@data$Departamento, Poly_Dept_ShowStatistic) %>%
      lapply(htmltools::HTML)
    Labels_Mpios  <- sprintf(paste0("<strong> %s </strong> (%s) <br>", Comodin, " ", tolower(Segregacion)),
                             Polygons_Mpio@data$Municipio, Polygons_Mpio@data$Departamento, Poly_Mpio_ShowStatistic) %>%
      lapply(htmltools::HTML)
  }

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  if (tipo == "deptos") {

    # MAPA POR DEPARTAMENTOS
    if (missingArg(colBorde)) { colBorde <- "#5A0028" }
    if (!(missingArg(cortes) && missingArg(colores))) {
      if (length(colores) != length(cortes)-1L) {
        stop(Mensaje, call. = FALSE)
      } else { Colors <- colores; Cortes <- cortes }
    } else {
      Colors <- c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50")
      Cortes <- c(0, 20, 50, 200, 1000, Inf)
    }
    pal <- colorBin(Colors, bins = Cortes)

    Mapa <- leaflet(data = Polygons_Depto) %>% addTiles(attribution = Msj)

    for (i in 1:length(Baldosas)) {
      Mapa <- Mapa %>% addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
    }

    Mapa <- Mapa %>%
      # Definiendo el centro del mapa.
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = zoomMapa) %>%
      # Adición de grupos de control de capas.
      addLayersControl(baseGroups = Baldosas.names, options = layersControlOptions(collapsed = compacto)) %>%
      # Adición de los polígonos, su relleno y etiquetas con el nombre y número de graduados.
      addPolygons(stroke = TRUE, fillColor = ~pal(Statistic), weight = 2, opacity = opacidad, color = "#005A32", dashArray = "", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Deptos, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                     textsize = "12px", direction = "auto")) %>%
      addLegend(position = "bottomright", pal = pal, values = ~Statistic, opacity = opacidadClic, labFormat = labelFormat(prefix = "[", suffix = ")"), title = titulo) %>%
      # Adición de los textos indicando el departamento y su respectivo municipio capital.
      addLabelOnlyMarkers(lng = Centroides_Deptos$Lon, lat = Centroides_Deptos$Lat, label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, textsize = textDeptos)) %>%
      addLabelOnlyMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, label = paste0(sapply(Capitales$Municipio, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = textMpios)) %>%
      # Adición del botón de control de búsqueda (lupa).
      addSearchFeatures(targetGroups = "lupa", options = searchFeaturesOptions(zoom = 8, openPopup = TRUE,
                                                                               textErr = "Ubicaci\u00f3n No Encontrada",
                                                                               textCancel = "Cancelar",
                                                                               textPlaceholder = "Buscar...",
                                                                               position = "topleft",
                                                                               hideMarkerOnCollapse = FALSE)) %>%
      # Adición de los marcadores circulares para las capitales de los departamentos.
      addCircleMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, radius = 2, stroke = TRUE,
                       color = "#D95F02", fill = TRUE, fillColor = "orangelight", fillOpacity = 0.9)

  } else if (tipo == "sinompios") {

    # MAPA POR MUNICIPIO (SI/NO)
    if (missingArg(colBorde)) { colBorde <- "#16ABAB" }
    if (!missingArg(cortes)) {
      warning(paste0("\u00a1Usted ha ingresado alg\u00fan valor en el argumento 'cortes' y ha seleccionado el tipo de mapa 'SiNoMpios'!",
                     "\n\t Por lo cual se omitir\u00e1 el valor ingresado, pues este tipo de mapa no permite un cambio en los cortes, pues es un intervalo binario."),
              call. = FALSE)
    }
    if (!missingArg(colores)) {
      if (length(colores) != 2L) {
        stop("\u00a1La longitud del argumento 'colores' es diferente a 2! Este mapa es binario por lo cual necesita especificar solamente 2 colores.", call. = FALSE)
      } else { colBinary <- colores }
    } else { colBinary <- c("#FDAE61", "#A6D96A") }
    Pal_Binary <- colorBin(palette = colBinary, bins = c(0, 1, 35000))

    Mapa <- leaflet(data = Polygons_Mpio) %>% addTiles(attribution = Msj)

    for (i in 1:length(Baldosas)) {
      Mapa <- Mapa %>% addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
    }

    Mapa <- Mapa %>%
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = zoomMapa) %>%
      addLayersControl(baseGroups = Baldosas.names, options = layersControlOptions(collapsed = compacto)) %>%
      addPolygons(stroke = TRUE, fillColor = ~Pal_Binary(Statistic), weight = 1, opacity = opacidad, color = "gray", dashArray = "3", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Mpios, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "12px", direction = "auto")) %>%
      addPolylines(data = Polygons_Depto, stroke = TRUE, color = "white", weight = 2, smoothFactor = 0.05) %>%
      addLegend(position = "bottomright", values = ~Statistic, bins = c(0, 1, 35000), colors = colBinary, opacity = opacidadClic,
                labels = c(paste0("0 ", Segregacion), paste0("1 o m\u00e1s ", Segregacion)), title = titulo) %>%
      addLabelOnlyMarkers(lng = Centroides_Deptos$Lon, lat = Centroides_Deptos$Lat, label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, textsize = textDeptos)) %>%
      addLabelOnlyMarkers(lng = Centroides_Mpios$Lon, lat = Centroides_Mpios$Lat, label = paste0(sapply(Centroides_Mpios$Municipio, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = FALSE)) %>%
      addSearchFeatures(targetGroups = "lupa", options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                                               textErr = "Ubicaci\u00f3n No Encontrada",
                                                                               textCancel = "Cancelar",
                                                                               textPlaceholder = "Buscar...",
                                                                               position = "topleft",
                                                                               hideMarkerOnCollapse = FALSE)) %>%
      addCircleMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, radius = 2, stroke = TRUE,
                       color = "#377EB8", fill = TRUE, fillColor = "purplelight", fillOpacity = 0.9)

  } else if (tipo == "mpios") {

    # MAPA TOTAL DE GRADUADOS POR MUNICIPIO
    if (missingArg(colBorde)) { colBorde <- "#AB1616" }
    if (!(missingArg(cortes) && missingArg(colores))) {
      if (length(colores) != length(cortes)-1L) {
        stop(Mensaje, call. = FALSE)
      } else { Colors <- colores; Cortes <- cortes }
    } else {
      Colors <- c("#B9FFDC", "#CFFFAF", "#FFEA80", "#FFA652", "#ED6753")
      Cortes <- c(0, 1, 3, 10, 100, Inf)
    }
    pal  <- colorBin(Colors, domain = Polygons_Mpio@data$Statistic, bins = Cortes)

    Mapa <- leaflet(data = Polygons_Mpio) %>% addTiles(attribution = Msj)

    for (i in 1:length(Baldosas)) {
      Mapa <- Mapa %>% addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
    }

    Mapa <- Mapa %>%
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = zoomMapa) %>%
      addLayersControl(baseGroups = Baldosas.names, options = layersControlOptions(collapsed = compacto)) %>%
      addPolygons(stroke = TRUE, fillColor = ~pal(Statistic), weight = 1, opacity = opacidad, color = "gray", dashArray = "3", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Mpios, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "12px", direction = "auto")) %>%
      addPolylines(data = Polygons_Depto, stroke = TRUE, color = "#252525", weight = 2, smoothFactor = 0.05) %>%
      addLegend(position = "bottomright", pal = pal, values = ~Statistic, opacity = opacidadClic, labFormat = labelFormat(prefix = "[", suffix = ")"), title = titulo) %>%
      addLabelOnlyMarkers(lng = Centroides_Deptos$Lon, lat = Centroides_Deptos$Lat, label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, textsize = textDeptos)) %>%
      addLabelOnlyMarkers(lng = Centroides_Mpios$Lon, lat = Centroides_Mpios$Lat, label = paste0(sapply(Centroides_Mpios$Municipio, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = FALSE)) %>%
      addSearchFeatures(targetGroups = "lupa", options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                                               textErr = "Ubicaci\u00f3n No Encontrada",
                                                                               textCancel = "Cancelar",
                                                                               textPlaceholder = "Buscar...",
                                                                               position = "topleft",
                                                                               hideMarkerOnCollapse = FALSE)) %>%
      addCircleMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, radius = 2, stroke = TRUE,
                       color = "#377EB8", fill = TRUE, fillColor = "purplelight", fillOpacity = 0.9)

  } else {

    # MAPA COMBINADO DEPARTAMENTO-MUNICIPIO
    if (missingArg(colBorde)) { colBorde <- "#5A0028" }
    if (!(missingArg(cortes) && missingArg(colores))) {
      if ( (length(colores$Deptos) != length(cortes$Deptos)-1L) || (length(colores$Mpios) != length(cortes$Mpios)-1L) ) {
        stop(Mensaje, call. = FALSE)
      } else {
        Colors_Deptos <- colores$Deptos; Cortes_Deptos <- cortes$Deptos
        Colors_Mpios  <- colores$Mpios;  Cortes_Mpios  <- cortes$Mpios
      }
    } else {
      Colors_Deptos <- c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50")
      Cortes_Deptos <- c(0, 20, 50, 200, 1000, Inf)
      Colors_Mpios  <- c("#B9FFDC", "#CFFFAF", "#FFEA80", "#FFA652", "#ED6753")
      Cortes_Mpios  <- c(0, 1, 3, 10, 100, Inf)
    }
    Pal_Deptos <- colorBin(palette = Colors_Deptos, bins = Cortes_Deptos)
    Pal_Mpios  <- colorBin(palette = Colors_Mpios, domain = Polygons_Mpio@data$Statistic, bins = Cortes_Mpios)

    Mapa <- leaflet(data = Polygons_Mpio) %>% addTiles(attribution = Msj)

    for (i in 1:length(Baldosas)) {
      Mapa <- Mapa %>% addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
    }

    Mapa <- Mapa %>%
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = zoomMapa) %>%
      addLayersControl(baseGroups = Baldosas.names, overlayGroups = "Mostrar<br/>Departamentos",
                       options = layersControlOptions(collapsed = compacto, autoZindex = TRUE)) %>%
      addPolygons(stroke = TRUE, fillColor = ~Pal_Mpios(Statistic), weight = 1, opacity = opacidad, color = "gray", dashArray = "3", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Mpios, group =  "Municipios", labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                           textsize = "12px", direction = "auto")) %>%
      addPolygons(data = Polygons_Depto, stroke = TRUE, fillColor = ~Pal_Deptos(Statistic), weight = 2, opacity = opacidad, color = "#005A32", dashArray = "", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = "#5A0028", dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Deptos, group = "Mostrar<br/>Departamentos", labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                                          textsize = "12px", direction = "auto")) %>%
      showGroup("Municipios") %>% hideGroup("Mostrar<br/>Departamentos") %>%
      addPolylines(data = Polygons_Depto, stroke = TRUE, color = "#005A32", weight = 2, smoothFactor = 0.05, group = "Municipios") %>%
      addLegend(position = "bottomright", pal = Pal_Mpios, values = ~Statistic, opacity = opacidadClic, labFormat = labelFormat(prefix = "[", suffix = ")"), title = TitleMpio) %>%
      addLegend(position = "bottomright", pal = Pal_Deptos, values = ~Statistic, opacity = opacidadClic, labFormat = labelFormat(prefix = "[", suffix = ")"), title = TitleDepto) %>%
      addLabelOnlyMarkers(lng = Centroides_Deptos$Lon, lat = Centroides_Deptos$Lat, label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE, textsize = textDeptos)) %>%
      addLabelOnlyMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, label = paste0(sapply(Capitales$Municipio, str_to_title)),
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, textsize = textMpios)) %>%
      addLabelOnlyMarkers(lng = Centroides_Mpios$Lon, lat = Centroides_Mpios$Lat, label = paste0(sapply(Centroides_Mpios$Municipio, str_to_title)),
                          group = "lupa", labelOptions = labelOptions(noHide = FALSE)) %>%
      addSearchFeatures(targetGroups = "lupa", options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                                               textErr = "Ubicaci\u00f3n No Encontrada",
                                                                               textCancel = "Cancelar",
                                                                               textPlaceholder = "Buscar...",
                                                                               position = "topleft",
                                                                               hideMarkerOnCollapse = FALSE)) %>%
      addCircleMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, radius = 2, stroke = TRUE, color = "#D95F02",
                       fill = TRUE, fillColor = "orangelight", fillOpacity = 0.9, group = "Municipios") %>%
      addCircleMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, radius = 2, stroke = TRUE, color = "#D95F02",
                       fill = TRUE, fillColor = "orangelight", fillOpacity = 0.9, group = "Mostrar<br/>Departamentos")
  }

  if (showSedes) {
    if (tipo %in% c("deptos", "sinompios", "mpios")) {
      Mapa <- Mapa %>%
        # Adición de grupos de control de capas.
        addLayersControl(baseGroups = Baldosas.names, overlayGroups = "Mostrar<br/>sedes UNAL", options = layersControlOptions(collapsed = compacto)) %>%
        # Adición de los marcadores de iconos para las distintas sedes de presencia nacional de la UNAL, y deseleccionándola por defecto.
        addAwesomeMarkers(lng = Sedes$Longitud, lat = Sedes$Latitud, group = "Mostrar<br/>sedes UNAL", icon = Icons_Sedes, label = Labels_Sedes,
                          labelOptions = labelOptions(style = list("font-weight" = "large", padding = "3px 8px"),
                                                      textsize = "15px", direction = "auto")) %>%
        hideGroup("Mostrar<br/>sedes UNAL")
    } else {
      Mapa <- Mapa %>%
        addLayersControl(baseGroups = Baldosas.names, overlayGroups = c("Mostrar<br/>Departamentos", "Mostrar<br/>sedes UNAL"),
                         options = layersControlOptions(collapsed = compacto, autoZindex = TRUE)) %>%
        addPolygons(data = Polygons_Depto, stroke = TRUE, fillColor = ~Pal_Deptos(Statistic), weight = 2, opacity = opacidad, color = "#005A32", dashArray = "", fillOpacity = opacidad,
                    highlightOptions = list(weight = 4, color = "#5A0028", dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                    label = Labels_Deptos, group = "Mostrar<br/>Departamentos", labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                                            textsize = "12px", direction = "auto")) %>%
        showGroup("Municipios") %>% hideGroup("Mostrar<br/>Departamentos") %>%
        addCircleMarkers(lng = Capitales$Longitud, lat = Capitales$Latitud, radius = 2, stroke = TRUE, color = "#D95F02",
                         fill = TRUE, fillColor = "orangelight", fillOpacity = 0.9, group = "Mostrar<br/>Departamentos") %>%
        addAwesomeMarkers(lng = Sedes$Longitud, lat = Sedes$Latitud, group = "Mostrar<br/>sedes UNAL", icon = Icons_Sedes, label = Labels_Sedes,
                          labelOptions = labelOptions(style = list("font-weight" = "large", padding = "3px 8px"),
                                                      textsize = "15px", direction = "auto")) %>%
        hideGroup("Mostrar<br/>sedes UNAL")
    }
  }
  if (!limpio) {
    Mapa <- Mapa %>%
      # Adición del minimapa para ayuda en la navegación.
      addMiniMap(position = "bottomleft", zoomAnimation = TRUE, toggleDisplay = TRUE, autoToggleDisplay = TRUE) %>%
      # Adición de botones simples para ver en pantalla completa, reestablecer el zoom y localización.
      addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE) %>%
      addEasyButton(easyButton(icon = "fa-globe", title = "Retornar", onClick = JS("function(btn, map){ map.setView(L.latLng(4.823986, -74.09776), 6); }"))) %>%
      addEasyButton(easyButton(icon = "fa-crosshairs", title = "Ub\u00edcame", onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
      # Adicción de la barra de escala.
      addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
  }
  return(Mapa)
}
