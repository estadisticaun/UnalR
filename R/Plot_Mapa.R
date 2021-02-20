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
#' @param datos Un data frame con dos columnas primordiales (*que usará internamente la función*)
#'   `Code_Dept` y `Code_Mun`. No objetos espaciales o polígonos.
#' @param tipo Cadena de caracteres que indica el tipo de mapa a graficar. Los valores
#'   permitidos son "Deptos", "SiNoMpios", "Mpios" y "DeptoMpio" (valor predeterminado).
#'   Se emparejará parcialmente.
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
#' El data frame introducido en el argumento datos debe contener atributos atómicos,
#' es decir, que cada fila debe corresponder a un individuo. Además, se usará solamente
#' dos variables de éste, correspondiente a los códigos `DIVIPOLA` del departamento
#' (**Code_Dept**) y el municipio (**Code_Mpio**).
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
#'            Latitud      = LAT_CIU_NAC) %>%
#'     filter(!is.na(Departamento))
#' }
#'
#' Plot.Mapa(datos    = Graduados,
#'           tipo     = "Deptos",
#'           titulo   = "Graduados 2020-II",
#'           baldosas = c("Stamen.Toner", "Stamen.TonerLite", "Stamen.TonerLines", "Esri.DeLorme",
#'                        "Esri.WorldTerrain", "Esri.WorldShadedRelief", "Esri.WorldPhysical",
#'                        "Esri.OceanBasemap", "Esri.WorldGrayCanvas"),
#'           colSedes = rep("green", 9),
#'           compacto = FALSE,
#'           textSize = 16,
#'           limpio   = TRUE
#'           )
#'
#' Plot.Mapa(datos = Graduados, tipo = "SiNoMpios", titulo = "Graduados 2020-II",
#'           opacidad = 0.4, colores = c("#FF0071", "#00BCB5")
#'           )
#'
#' Plot.Mapa(datos   = Graduados,
#'           tipo    = "DeptoMpio",
#'           titulo   = "Graduados 2020-II",
#'           cortes  = list(Deptos = c(0, 10, 20, 50, 500, Inf),
#'                          Mpios  = c(0, 1, 5, 10, 100, Inf)),
#'           colores = list(Deptos = c("#6812F2", "#5769F6", "#F6ED0D", "#EE6115", "#EC2525"),
#'                          Mpios  = c("#E7F15D", "#ACBD37", "#E15E32", "#A82743", "#5C323E"))
#'           )
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
Plot.Mapa <- function(datos, tipo = c("Deptos", "SiNoMpios", "Mpios", "DeptoMpio"), titulo, baldosas, cortes,
                      colores, colSedes, opacidad = 0.7, colBorde, compacto = TRUE, textSize = 10, limpio = FALSE) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos)) {
    stop("\u00a1Por favor introduzca un conjunto de datos!", call. = FALSE)
  }
  if (!all(c("Code_Dept", "Code_Mun") %in% colnames(datos))) {
    stop('\u00a1El conjunto de datos ingresado en el argumento "datos" debe contener por lo menos dos columnas con los nombres "Code_Dept" y "Code_Mun"!', call. = FALSE)
  }
  tipo <- tolower(tipo); '%NotIN%' <- Negate('%in%')
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
  Mensaje <- paste0("\u00a1El argumento 'colores' no tiene la longitud correcta respecto al argumento 'cortes' ingresado, o viceversa!",
                    "\n\t - Recuerde que si realiza (n) cortes necesita (n-1) colores para cada intervalo creado.",
                    "\n\t -- Por ejemplo, si los cortes son c(0, 50, Inf) necesitar\u00e1 2 colores para dichos intervalos.")
  if (!missingArg(colSedes)) {
    if (length(colSedes) != 9L) {
      stop(paste0("\u00a1El n\u00famero de colores ingresados en el vector 'colSedes' no corresponde con el n\u00famero de sedes presentes!",
                  "\n\t - Ingrese un vector con 9 colores correspondientes a las sedes:",
                  "\n\t -- Medell\u00edn, Bogot\u00e1, Manizales, De La Paz, Tumaco, Palmira, Orinoqu\u00eda, Caribe y Amazonas, respectivamente."), call. = FALSE)
    } else { Icons_Col <- colSedes }
  } else { Icons_Col <- c("orange", "green", "darkblue", "purple", "gray", "darkpurple", "lightred", "red", "blue") }
  if (!(is.numeric(opacidad) && is.numeric(textSize))) {
    stop("\u00a1Los argumentos 'opacidad'y 'textSize' deben ser un valor num\u00e9rico!", call. = FALSE)
  }
  opacidadClic <- ifelse(opacidad!=1, opacidad+0.2, opacidad)
  textDeptos   <- paste0(textSize+2, "px"); textMpios <- paste0(textSize, "px")
  if (!missing(colBorde)) {
    if (!is.character(colBorde)) {
      stop("\u00a1El argumento 'colBorde' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
    }
  }
  if (!(is.logical(compacto) && is.logical(limpio))) {
    stop("\u00a1Los argumentos 'compacto' y 'limpio' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  Datos <- datos
  Msj <- 'Map created by <a href="https://github.com/JeisonAlarcon">Jeison Alarc\u00f3n</a> &mdash; Data source: &copy; <a href="http://estadisticas.unal.edu.co/home/">DNPE</a>'
  # Code_Mun != 25001 -> Cundinamarca no tiene capital, pues Bogotá D.C. está en otro departamento.
  Capitales <- Cabeceras %>% filter(Code_Mun %% 1000 == 1, Code_Mun != 25001)
  Datos_ByDept <- Datos %>% group_by(Code_Dept) %>% summarise(Total = n())
  # ¡OJO! se eliminan los registros con datos faltantes en municipio de nacimiento.
  Datos_ByMun <- Datos %>% group_by(Code_Mun) %>% summarise(Total = n()) %>% filter(!is.na(Code_Mun))

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  # Carga del Departamentos
  Polygons_Depto <- Depto_Final
  Polygons_Depto@data$Total <- Polygons_Depto@data %>% left_join(Datos_ByDept, by = "Code_Dept") %>% select(Total) %>% pull()

  # Carga de Municipios
  Polygons_Mpio <- Mpio_Final
  Polygons_Mpio@data$Total <- Polygons_Mpio@data %>% left_join(Datos_ByMun, by = "Code_Mun") %>%
    select(Total) %>% mutate(Total = replace_na(Total, 0)) %>% pull()

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  # Ubicación de los centroides correspondientes a cada departamento (extraídos del objeto 'Polygons_Depto' y no de 'Capitales')
  #   El centroide de Colombia corresponde al centroide del departamento de Cundinamarca
  NumDept <- length(Polygons_Depto); Coordenadas <- matrix(0, nrow = NumDept, ncol = 2)
  for (i in 1:NumDept) { Coordenadas[i,] <- Polygons_Depto@polygons[[i]]@labpt }
  Centroides_Deptos <- data.frame("Departamento" = Polygons_Depto@data$Departamento,
                                  "Lon" = Coordenadas[,1], "Lat" = Coordenadas[,2])

  NumMpios <- length(Polygons_Mpio); Coordenadas <- matrix(0, nrow = NumMpios, ncol = 2)
  for (i in 1:NumMpios) { Coordenadas[i,] <- Polygons_Mpio@polygons[[i]]@labpt }
  Centroides_Mpios <- data.frame("Municipio" = Polygons_Mpio@data$Municipio,
                                 "Lon" = Coordenadas[,1], "Lat" = Coordenadas[,2])

  Centroide_Col <- Centroides_Deptos %>% filter(Departamento == "CUNDINAMARCA")

  # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

  # Filtrar sedes de la Universidad Nacional de Colombia
  Sedes <- Cabeceras %>%
    filter(Code_Mun %in% c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)) %>%
    mutate(Sede = c("Medell\u00edn", "Bogot\u00e1", "Manizales", "De La Paz", "Tumaco", "Palmira",  "Orinoqu\u00eda", "Caribe", "Amazonas"))
  Icons_Sedes <- makeAwesomeIcon(markerColor = Icons_Col, iconColor = "white", fontFamily = "Leonardian", text = "un")

  # Labels
  Labels_Sedes  <- sprintf("<strong>%s %s</strong>", "Sede", Sedes$Sede) %>% lapply(htmltools::HTML)
  Labels_Deptos <- sprintf("<strong>%s</strong><br/>%g graduados",
                           Polygons_Depto@data$Departamento, Polygons_Depto@data$Total) %>%
    lapply(htmltools::HTML)
  Labels_Mpios  <- sprintf("<strong> %s </strong> (%s) <br/> %g  graduados",
                           Polygons_Mpio@data$Municipio, Polygons_Mpio@data$Departamento, Polygons_Mpio@data$Total) %>%
    lapply(htmltools::HTML)

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
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = 6) %>%
      # Adición de grupos de control de capas.
      addLayersControl(baseGroups = Baldosas.names, overlayGroups = "Mostrar<br/>sedes UNAL", options = layersControlOptions(collapsed = compacto)) %>%
      # Adición de los polígonos, su relleno y etiquetas con el nombre y número de graduados.
      addPolygons(stroke = TRUE, fillColor = ~pal(Total), weight = 2, opacity = opacidad, color = "#005A32", dashArray = "", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Deptos, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                     textsize = "12px", direction = "auto")) %>%
      addLegend(position = "bottomright", pal = pal, values = ~Total, opacity = opacidadClic, title = titulo) %>%
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
                       color = "#D95F02", fill = TRUE, fillColor = "orangelight", fillOpacity = 0.9) %>%
      # Adición de los marcadores de iconos para las distintas sedes de presencia nacional de la UNAL, y deseleccionándola por defecto.
      addAwesomeMarkers(lng = Sedes$Longitud, lat = Sedes$Latitud, group = "Mostrar<br/>sedes UNAL", icon = Icons_Sedes, label = Labels_Sedes,
                        labelOptions = labelOptions(style = list("font-weight" = "large", padding = "3px 8px"),
                                                    textsize = "15px", direction = "auto")) %>%
      hideGroup("Mostrar<br/>sedes UNAL")

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
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = 6) %>%
      addLayersControl(baseGroups = Baldosas.names, overlayGroups = "Mostrar<br/>sedes UNAL", options = layersControlOptions(collapsed = compacto)) %>%
      addPolygons(stroke = TRUE, fillColor = ~Pal_Binary(Total), weight = 1, opacity = opacidad, color = "gray", dashArray = "3", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Mpios, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "12px", direction = "auto")) %>%
      addPolylines(data = Polygons_Depto, stroke = TRUE, color = "white", weight = 2, smoothFactor = 0.05) %>%
      addLegend(position = "bottomright", values = ~Total, bins = c(0, 1, 35000), colors = colBinary, opacity = opacidadClic,
                labels = c("0 graduados", "1 o m\u00e1s graduados"), title = titulo) %>%
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
                       color = "#377EB8", fill = TRUE, fillColor = "purplelight", fillOpacity = 0.9) %>%
      addAwesomeMarkers(lng = Sedes$Longitud, lat = Sedes$Latitud, group = "Mostrar<br/>sedes UNAL", icon = Icons_Sedes, label = Labels_Sedes,
                        labelOptions = labelOptions(style = list("font-weight" = "large", padding = "3px 8px"),
                                                    textsize = "15px", direction = "auto")) %>%
      hideGroup("Mostrar<br/>sedes UNAL")

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
    pal  <- colorBin(Colors, domain = Polygons_Mpio@data$Total, bins = Cortes)

    Mapa <- leaflet(data = Polygons_Mpio) %>% addTiles(attribution = Msj)

    for (i in 1:length(Baldosas)) {
      Mapa <- Mapa %>% addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
    }

    Mapa <- Mapa %>%
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = 6) %>%
      addLayersControl(baseGroups = Baldosas.names, overlayGroups = "Mostrar<br/>sedes UNAL", options = layersControlOptions(collapsed = compacto)) %>%
      addPolygons(stroke = TRUE, fillColor = ~pal(Total), weight = 1, opacity = opacidad, color = "gray", dashArray = "3", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Mpios, labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                    textsize = "12px", direction = "auto")) %>%
      addPolylines(data = Polygons_Depto, stroke = TRUE, color = "#252525", weight = 2, smoothFactor = 0.05) %>%
      addLegend(position = "bottomright", pal = pal, values = ~Total, opacity = opacidadClic, title = titulo) %>%
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
                       color = "#377EB8", fill = TRUE, fillColor = "purplelight", fillOpacity = 0.9) %>%
      addAwesomeMarkers(lng = Sedes$Longitud, lat = Sedes$Latitud, group = "Mostrar<br/>sedes UNAL", icon = Icons_Sedes, label = Labels_Sedes,
                        labelOptions = labelOptions(style = list("font-weight" = "large", padding = "3px 8px"),
                                                    textsize = "15px", direction = "auto")) %>%
      hideGroup("Mostrar<br/>sedes UNAL")

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
    Pal_Mpios  <- colorBin(palette = Colors_Mpios, domain = Polygons_Mpio@data$Total, bins = Cortes_Mpios)

    TitleSplit  <- strsplit(titulo, "\\s+")
    TitleMpio   <- paste0("<center>", TitleSplit[[1]][1], " por<br/>municipio<br/>", TitleSplit[[1]][2], "</center>")
    TitleDepto  <- paste0("<center>", TitleSplit[[1]][1], " por<br/>departamento<br/>", TitleSplit[[1]][2], "</center>")

    Mapa <- leaflet(data = Polygons_Mpio) %>% addTiles(attribution = Msj)

    for (i in 1:length(Baldosas)) {
      Mapa <- Mapa %>% addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
    }

    Mapa <- Mapa %>%
      setView(lat = Centroide_Col$Lat, lng = Centroide_Col$Lon, zoom = 6) %>%
      addLayersControl(baseGroups = Baldosas.names, overlayGroups = c("Mostrar<br/>Departamentos", "Mostrar<br/>sedes UNAL"),
                       options = layersControlOptions(collapsed = compacto, autoZindex = TRUE)) %>%
      addPolygons(stroke = TRUE, fillColor = ~Pal_Mpios(Total), weight = 1, opacity = opacidad, color = "gray", dashArray = "3", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = colBorde, dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Mpios, group =  "Municipios", labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                           textsize = "12px", direction = "auto")) %>%
      addPolygons(data = Polygons_Depto, stroke = TRUE, fillColor = ~Pal_Deptos(Total), weight = 2, opacity = opacidad, color = "#005A32", dashArray = "", fillOpacity = opacidad,
                  highlightOptions = list(weight = 4, color = "#5A0028", dashArray = "", fillOpacity = opacidadClic, bringToFront = TRUE),
                  label = Labels_Deptos, group = "Mostrar<br/>Departamentos", labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                                          textsize = "12px", direction = "auto")) %>%
      showGroup("Municipios") %>% hideGroup("Mostrar<br/>Departamentos")%>%
      addPolylines(data = Polygons_Depto, stroke = TRUE, color = "#005A32", weight = 2, smoothFactor = 0.05, group = "Municipios") %>%
      addLegend(position = "bottomright", pal = Pal_Mpios, values = ~Total, opacity = opacidadClic, title = TitleMpio) %>%
      addLegend(position = "bottomright", pal = Pal_Deptos, values = ~Total, opacity = opacidadClic, title = TitleDepto) %>%
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
                       fill = TRUE, fillColor = "orangelight", fillOpacity = 0.9, group = "Mostrar<br/>Departamentos") %>%
      addAwesomeMarkers(lng = Sedes$Longitud, lat = Sedes$Latitud, group = "Mostrar<br/>sedes UNAL", icon = Icons_Sedes, label = Labels_Sedes,
                        labelOptions = labelOptions(style = list("font-weight" = "large", padding = "3px 8px"),
                                                    textsize = "15px", direction = "auto")) %>%
      hideGroup("Mostrar<br/>sedes UNAL")

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
