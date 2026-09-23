# Internal Leaflet plugin support used by Plot.Mapa() and Plot.Mundo().

.unalr_leaflet_dependency <- function(
  name,
  version,
  directory,
  script,
  stylesheet = NULL
) {
  source_path <- system.file(
    "htmlwidgets",
    "unalr-leaflet",
    directory,
    package = "UnalR"
  )

  htmltools::htmlDependency(
    name = name,
    version = version,
    src = c(file = source_path),
    script = script,
    stylesheet = stylesheet,
    all_files = TRUE
  )
}

.unalr_fullscreen_dependency <- function() {
  list(
    .unalr_leaflet_dependency(
      name = "lfx-fullscreen",
      version = "1.0.2",
      directory = "fullscreen",
      script = "lfx-fullscreen-prod.js",
      stylesheet = "lfx-fullscreen-prod.css"
    )
  )
}

.unalr_search_dependency <- function() {
  list(
    .unalr_leaflet_dependency(
      name = "unalr-leaflet-search",
      version = "4.0.0",
      directory = "search",
      script = c("lfx-search-prod.js", "unalr-search-bindings.js"),
      stylesheet = "lfx-search-prod.css"
    )
  )
}

.unalr_add_fullscreen_control <- function(
  map,
  position = "topleft",
  pseudoFullscreen = FALSE
) {
  map$dependencies <- c(map$dependencies, .unalr_fullscreen_dependency())
  if (is.null(map$x$options)) {
    map$x$options <- list()
  }
  map$x$options["fullscreenControl"] <- list(
    list(position = position, pseudoFullscreen = pseudoFullscreen)
  )
  map
}

.unalr_search_features_options <- function(
  zoom = 17,
  openPopup = FALSE,
  textErr = "Location Not Found",
  textCancel = "Cancel",
  textPlaceholder = "Search...",
  position = "topleft",
  hideMarkerOnCollapse = FALSE
) {
  list(
    openPopup = openPopup,
    propertyName = "label",
    initial = FALSE,
    moveToLocation = TRUE,
    zoom = zoom,
    container = "",
    minLength = 1,
    casesensitive = FALSE,
    autoType = TRUE,
    delayType = 400,
    tooltipLimit = -1,
    tipAutoSubmit = TRUE,
    firstTipSubmit = FALSE,
    autoResize = TRUE,
    collapsed = TRUE,
    autoCollapse = FALSE,
    autoCollapseTime = 1200,
    textErr = textErr,
    textCancel = textCancel,
    textPlaceholder = textPlaceholder,
    position = position,
    hideMarkerOnCollapse = hideMarkerOnCollapse,
    marker = list(
      animate = TRUE,
      circle = list(
        radius = 10,
        weight = 3,
        color = "#e03",
        stroke = TRUE,
        fill = FALSE
      )
    )
  )
}

.unalr_add_search_features <- function(
  map,
  targetGroups,
  options = .unalr_search_features_options()
) {
  map$dependencies <- c(map$dependencies, .unalr_search_dependency())
  leaflet::invokeMethod(
    map,
    leaflet::getMapData(map),
    "addUnalRSearchFeatures",
    targetGroups,
    options
  )
}
