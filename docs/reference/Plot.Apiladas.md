# Cree un gráfico de barras apiladas dinámico/estático y flexible

Esta función proporciona excelentes herramientas y opciones para la
visualización de un gráfico de barras apiladas con el objetivo de
mostrar el tamaño relativo (*como porcentaje*) de una variable
categórica, subdivididas por colores en función de un subgrupo. Dicha
gráfica se va a representar usando la librería `Highcharter`, la cual
usa internamente `JavaScript`.

## Uso

``` r
Plot.Apiladas(
  datos,
  ejeX,
  valores,
  categoria,
  ano,
  periodo,
  addPeriodo = FALSE,
  colores,
  titulo = "",
  libreria = c("highcharter", "plotly"),
  estilo = NULL,
  estatico = FALSE
)
```

## Argumentos

- datos:

  Un data frame, no un vector numérico.

- ejeX:

  Una variable categórica dentro del data frame ingresado en `datos`.

- valores:

  Variable numérica que contiene los valores que desea graficar.

- categoria:

  Una variable categórica dentro del data frame ingresado en `datos`.

- ano:

  Argument deprecated. This Argument still exist but will be removed in
  the next version.

- periodo:

  Argument deprecated. This Argument still exist but will be removed in
  the next version.

- addPeriodo:

  Argument deprecated. This Argument still exist but will be removed in
  the next version.

- colores:

  Cadena de caracteres indicando los colores con los cuales se deben
  colorear cada una de las series correspondiente a cada nivel del
  argumento `categoria`. Si no se introduce algún vector se usará la
  paleta `rainbow` por defecto.

- titulo:

  Cadena de caracteres indicando el título principal del plot.

- libreria:

  Cadena de caracteres que indica el paquete con el cual se realizará el
  plot. Los valores permitidos son `"highcharter"` (*valor
  predeterminado*) y `"plotly"`. Los valores se emparejarán
  parcialmente.

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados
  para graficar las barras apiladas y cuyo objetivo es personalizar
  pequeños detalles de éste.

  - `hc.Tema`, `hc.Credits`, `gg.Tema`, `gg.Legend` y `gg.Texto`: Igual
    uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `LegendTitle`: Cadena de caracteres indicando un título para la
    leyenda (*diferentes niveles del argumento `categoria`*).

  - `ply.LegendPosition`: Igual uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `ply.Credits`: Igual uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `gg.Bar`: Igual uso que en
    [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md)

- estatico:

  Si es `FALSE` (*valor predeterminado*) el gráfico a retornar será
  dinámico (*dependiendo de la librería seleccionada*), en caso
  contrario se retornará un gráfico estático construido con `ggplot2`.

## Valor

Retorna el diagrama de barras apiladas (*objeto widget de HTML*) creado.
La clase del objeto retornado será un "htmlwidget" y adicionalmente
pertenecerá a la clase "highchart".

## Lista de argumentos de estilo

Sabemos que puede ser abrumador el número de argumentos dentro del
parámetro `estilo`, pero es necesario si queremos ofrecer al usuario la
máxima personalización dentro de cada función usando cualquier librería.
Por tal razón, a continuación, se detalla el listado completo de
argumentos, usados al especificar la librería y en qué función están
presentes (*marcado con una × si lo posee*).

|  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| **Librería** | **estilo\$** | [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md) | [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md) | `Plot.Apiladas()` | [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md) | [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md) | [`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md) | [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md) | [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md) |
| — | *gg.Tema* | × | × | × | × |  |  |  |  |
| l | *gg.Texto* | × | × | × | × |  |  |  |  |
| l | *gg.Legend* | × |  | × | × |  |  |  |  |
| l | *gg.Linea* | × |  |  |  |  |  |  |  |
| l | *gg.Punto* | × |  |  |  |  |  |  |  |
| l | *gg.Bar* |  | × | × |  |  |  |  |  |
| l | *gg.VarWidth* |  |  |  | × |  |  |  |  |
| l | *gg.OutShape* |  |  |  | × |  |  |  |  |
| l | *gg.JitWidth* |  |  |  | × |  |  |  |  |
| l | *gg.JitSize* |  |  |  | × |  |  |  |  |
| l | *gg.Range* |  |  |  |  | × |  |  |  |
| **ggplot2** | *gg.plty* |  |  |  |  | × |  |  |  |
| l | *gg.plwd* |  |  |  |  | × |  |  |  |
| l | *gg.cglwd* |  |  |  |  | × |  |  |  |
| l | *gg.cglcol* |  |  |  |  | × |  |  |  |
| l | *gg.fontsize.title* |  |  |  |  |  | × |  |  |
| l | *gg.fontsize.labels* |  |  |  |  |  | × |  |  |
| l | *gg.fontcolor.labels* |  |  |  |  |  | × |  |  |
| l | *gg.border.lwds* |  |  |  |  |  | × |  |  |
| l | *gg.border.col* |  |  |  |  |  | × |  |  |
| l | *gg.lowerbound.cex.labels* |  |  |  |  |  | × |  |  |
| l | *gg.force.print.labels* |  |  |  |  |  | × |  |  |
| — | *gg.overlap.labels* |  |  |  |  |  | × |  |  |
| » | *hc.Tema* | × | × | × | × |  | × | × | × |
| l | *hc.Credits* | × | × | × | × |  | × | × | × |
| **highcharter** | *hc.BoxInfo* | × |  |  |  |  |  |  |  |
| l | *hc.Slider* | × |  |  |  |  |  |  |  |
| » | *hc.borderRadius* |  |  |  |  |  | × |  |  |
| • | *ply.Credits* | × | × | × | × | × | × | × |  |
| ° | *ply.Legend* |  | × |  |  |  |  | × |  |
| ° | *ply.LegendPosition* | × |  | × | × | × |  |  |  |
| **plotly** | *ply.Interaction* | × |  |  | × |  |  |  |  |
| ° | *ply.Relleno* |  |  |  |  | × |  |  |  |
| ° | *ply.Opacidad* |  |  |  |  | × | × |  |  |
| • | *ply.LegendTitle* |  |  |  |  | × |  |  |  |
| **dygraphs** | *dyg.LegendWidth* | × |  |  |  |  |  |  |  |
| » | *dyg.Resaltar* | × |  |  |  |  |  |  |  |
| — | *e.Tema* |  |  |  |  | × |  |  |  |
| l | *e.Credits* |  |  |  |  | × |  |  |  |
| **echarts4r** | *e.Forma* |  |  |  |  | × |  |  |  |
| l | *e.LegType* |  |  |  |  | × |  |  |  |
| — | *e.LegLoc* |  |  |  |  | × |  |  |  |

## Ejemplos

``` r
# Ejemplo generalizado (sin uso de un consolidado como input)
# library("tibble"); library("dplyr")
set.seed(42)
Blood <- tibble(
  Quarter = sample(c("I", "II", "III", "IV"), size = 200, replace = TRUE),
  Group   = sample(
    c("O", "A", "B", "AB"), size = 200, prob = c(.5, .3, .16, .4), replace = TRUE
  ),
  Prevalence = round(runif(200)*100)
)
Plot.Apiladas(
  datos     = Blood     ,
  ejeX      = Quarter   ,
  valores   = Prevalence,
  categoria = Group     ,
  colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C")
)
#> Warning: ¡Se usará la librería 'highcharter' por defecto para realizar el plot!
#> Warning: 
#>     ¡Ha ingresado un dataframe que no está de forma condensada, es decir,
#>     para cada categoría existe más de un valor para un mismo punto del eje X!
#>     Se sumará los valores por defectos para dichos puntos que gocen de +1 valor
#>            

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Porcentaje","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"type":"linear","labels":{"format":"{value}%","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"min":0,"max":100},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotApiladas_Group"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":true},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"column":{"stacking":"normal"}},"series":[{"name":"A","data":[{"Quarter":"I","Clase":"A","Prevalence":502,"xAxis":"I","sumYear":2737,"percent_xAxis":18.3412,"y":18.3412,"name":"I"},{"Quarter":"II","Clase":"A","Prevalence":150,"xAxis":"II","sumYear":2659,"percent_xAxis":5.6412,"y":5.6412,"name":"II"},{"Quarter":"III","Clase":"A","Prevalence":119,"xAxis":"III","sumYear":1475,"percent_xAxis":8.0678,"y":8.0678,"name":"III"},{"Quarter":"IV","Clase":"A","Prevalence":464,"xAxis":"IV","sumYear":2519,"percent_xAxis":18.42,"y":18.42,"name":"IV"}],"type":"column"},{"name":"AB","data":[{"Quarter":"I","Clase":"AB","Prevalence":652,"xAxis":"I","sumYear":2737,"percent_xAxis":23.8217,"y":23.8217,"name":"I"},{"Quarter":"II","Clase":"AB","Prevalence":840,"xAxis":"II","sumYear":2659,"percent_xAxis":31.5908,"y":31.5908,"name":"II"},{"Quarter":"III","Clase":"AB","Prevalence":527,"xAxis":"III","sumYear":1475,"percent_xAxis":35.7288,"y":35.7288,"name":"III"},{"Quarter":"IV","Clase":"AB","Prevalence":890,"xAxis":"IV","sumYear":2519,"percent_xAxis":35.3315,"y":35.3315,"name":"IV"}],"type":"column"},{"name":"B","data":[{"Quarter":"I","Clase":"B","Prevalence":543,"xAxis":"I","sumYear":2737,"percent_xAxis":19.8392,"y":19.8392,"name":"I"},{"Quarter":"II","Clase":"B","Prevalence":474,"xAxis":"II","sumYear":2659,"percent_xAxis":17.8263,"y":17.8263,"name":"II"},{"Quarter":"III","Clase":"B","Prevalence":249,"xAxis":"III","sumYear":1475,"percent_xAxis":16.8814,"y":16.8814,"name":"III"},{"Quarter":"IV","Clase":"B","Prevalence":67,"xAxis":"IV","sumYear":2519,"percent_xAxis":2.6598,"y":2.6598,"name":"IV"}],"type":"column"},{"name":"O","data":[{"Quarter":"I","Clase":"O","Prevalence":1040,"xAxis":"I","sumYear":2737,"percent_xAxis":37.9978,"y":37.9978,"name":"I"},{"Quarter":"II","Clase":"O","Prevalence":1195,"xAxis":"II","sumYear":2659,"percent_xAxis":44.9417,"y":44.9417,"name":"II"},{"Quarter":"III","Clase":"O","Prevalence":580,"xAxis":"III","sumYear":1475,"percent_xAxis":39.322,"y":39.322,"name":"III"},{"Quarter":"IV","Clase":"O","Prevalence":1098,"xAxis":"IV","sumYear":2519,"percent_xAxis":43.5887,"y":43.5887,"name":"IV"}],"type":"column"}],"xAxis":{"type":"category","title":{},"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"colors":["#FF553D","#A5FF67","#40D2FF","#FFDB5C"],"tooltip":{"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}:<\/b> {point.percent_xAxis:.2f}%<br/>"},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","title":{"text":"","style":{"textDecoration":"underline"}},"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Txt <- "BARRAS APILADAS EN FUNCI\u00d3N DEL NIVEL ACAD\u00c9MICO Y EL A\u00d1O"
Msj <- paste(
  "Se considera \u00fanicamente los valores obtenidos en el primer periodo",
  "acad\u00e9mico de cada a\u00f1o."
)
Plot.Apiladas(
  datos     = ejConsolidadoGrad |> filter(YEAR %in% c(2018:2020), SEMESTRE == 1),
  categoria = "NIVEL",      # Pruebe también con alguna de -> unique(ejConsolidadoGrad$Variable)
  colores   = c("#FFA700", "#C10AA1", "#01CDFE", "#00FF44", "#FF0040"),
  titulo    = Txt,
  estilo    = list(LegendTitle = "NIVEL ACAD\u00c9MICO:", hc.Tema = 4, hc.Credits = Msj)
)
#> Warning: ¡Se usará la librería 'highcharter' por defecto para realizar el plot!
#> Warning: Unquoting language objects with `!!!` is deprecated as of rlang 0.4.0. Please
#> use `!!` instead.
#> 
#> # Bad: dplyr::select(data, !!!enquo(x))
#> 
#> # Good: dplyr::select(data, !!enquo(x)) # Unquote single quosure
#> dplyr::select(data, !!!enquos(x)) # Splice list of quosures
#> This warning is displayed once every 8 hours.
#> Warning: 
#>     ¡Ha ingresado un dataframe que no está de forma condensada, es decir,
#>     para cada categoría existe más de un valor para un mismo punto del eje X!
#>     Se sumará los valores por defectos para dichos puntos que gocen de +1 valor
#>            

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"BARRAS APILADAS EN FUNCIÓN DEL NIVEL ACADÉMICO Y EL AÑO","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Porcentaje","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"type":"linear","labels":{"format":"{value}%","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"min":0,"max":100},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotApiladas_Clase"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":true},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"column":{"stacking":"normal"}},"series":[{"name":"Doctorado","data":[{"YEAR":2018,"Clase":"Doctorado","Total":98,"xAxis":"2018","sumYear":5124,"percent_xAxis":1.9126,"y":1.9126,"name":"2018"},{"YEAR":2019,"Clase":"Doctorado","Total":111,"xAxis":"2019","sumYear":2690,"percent_xAxis":4.1264,"y":4.1264,"name":"2019"},{"YEAR":2020,"Clase":"Doctorado","Total":125,"xAxis":"2020","sumYear":4926,"percent_xAxis":2.5376,"y":2.5376,"name":"2020"}],"type":"column"},{"name":"Especialidades Médicas","data":[{"YEAR":2018,"Clase":"Especialidades Médicas","Total":128,"xAxis":"2018","sumYear":5124,"percent_xAxis":2.498,"y":2.498,"name":"2018"},{"YEAR":2019,"Clase":"Especialidades Médicas","Total":96,"xAxis":"2019","sumYear":2690,"percent_xAxis":3.5688,"y":3.5688,"name":"2019"},{"YEAR":2020,"Clase":"Especialidades Médicas","Total":130,"xAxis":"2020","sumYear":4926,"percent_xAxis":2.6391,"y":2.6391,"name":"2020"}],"type":"column"},{"name":"Especialización","data":[{"YEAR":2018,"Clase":"Especialización","Total":948,"xAxis":"2018","sumYear":5124,"percent_xAxis":18.5012,"y":18.5012,"name":"2018"},{"YEAR":2019,"Clase":"Especialización","Total":675,"xAxis":"2019","sumYear":2690,"percent_xAxis":25.0929,"y":25.0929,"name":"2019"},{"YEAR":2020,"Clase":"Especialización","Total":839,"xAxis":"2020","sumYear":4926,"percent_xAxis":17.0321,"y":17.0321,"name":"2020"}],"type":"column"},{"name":"Maestría","data":[{"YEAR":2018,"Clase":"Maestría","Total":932,"xAxis":"2018","sumYear":5124,"percent_xAxis":18.1889,"y":18.1889,"name":"2018"},{"YEAR":2019,"Clase":"Maestría","Total":796,"xAxis":"2019","sumYear":2690,"percent_xAxis":29.5911,"y":29.5911,"name":"2019"},{"YEAR":2020,"Clase":"Maestría","Total":869,"xAxis":"2020","sumYear":4926,"percent_xAxis":17.6411,"y":17.6411,"name":"2020"}],"type":"column"},{"name":"Pregrado","data":[{"YEAR":2018,"Clase":"Pregrado","Total":3018,"xAxis":"2018","sumYear":5124,"percent_xAxis":58.8993,"y":58.8993,"name":"2018"},{"YEAR":2019,"Clase":"Pregrado","Total":1012,"xAxis":"2019","sumYear":2690,"percent_xAxis":37.6208,"y":37.6208,"name":"2019"},{"YEAR":2020,"Clase":"Pregrado","Total":2963,"xAxis":"2020","sumYear":4926,"percent_xAxis":60.1502,"y":60.1502,"name":"2020"}],"type":"column"}],"xAxis":{"type":"category","title":{},"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"colors":["#FFA700","#C10AA1","#01CDFE","#00FF44","#FF0040"],"tooltip":{"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}:<\/b> {point.percent_xAxis:.2f}%<br/>"},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","title":{"text":"NIVEL ACADÉMICO:","style":{"textDecoration":"underline"}},"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"subtitle":{"text":"Se considera únicamente los valores obtenidos en el primer periodo académico de cada año.","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#FF2700","#008FD5","#77AB43","#636464","#C4C4C4"],"chart":{"backgroundColor":"#F0F0F0","plotBorderColor":"#606063","style":{"fontFamily":"Roboto","color":"#3C3C3C"}},"title":{"align":"left","style":{"fontWeight":"bold"}},"subtitle":{"align":"left"},"xAxis":{"gridLineWidth":1,"gridLineColor":"#D7D7D8","labels":{"style":{"fontFamily":"Unica One, sans-serif","color":"#3C3C3C"}},"lineColor":"#D7D7D8","minorGridLineColor":"#505053","tickColor":"#D7D7D8","tickWidth":1,"title":{"style":{"color":"#A0A0A3"}}},"yAxis":{"gridLineColor":"#D7D7D8","labels":{"style":{"fontFamily":"Unica One, sans-serif","color":"#3C3C3C"}},"lineColor":"#D7D7D8","minorGridLineColor":"#505053","tickColor":"#D7D7D8","tickWidth":1,"title":{"style":{"color":"#A0A0A3"}}},"tooltip":{"backgroundColor":"rgba(0, 0, 0, 0.85)","style":{"color":"#F0F0F0"}},"legend":{"itemStyle":{"color":"#3C3C3C"},"itemHiddenStyle":{"color":"#606063"}},"credits":{"style":{"color":"#666"}},"labels":{"style":{"color":"#D7D7D8"}},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#C0C0C0","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":["Roboto","Unica+One"],"debug":false},"evals":[],"jsHooks":[]}Plot.Apiladas(
  datos     = ejConsolidadoGrad |> filter(YEAR %in% c(2018:2020), SEMESTRE == 1),
  categoria = "AREAC_SNIES",
    colores   = c("#D2D4DC", "#FF8ABF", "#945BC2", "#D11879",
                  "#FF7F7F", "#FFA568", "#9CFF86", "#89D8FF"),
  titulo    = "BARRAS APILADAS EN FUNCI\u00d3N DEL \u00c1REA DEL SNIES",
  libreria  = "plotly",
  estilo    = list(
    LegendTitle = "NIVEL ACAD\u00c9MICO:",
    ply.Credits = list(x = 0.5, y = 1.5, text = gsub("l p", "l\np", Msj)),
    ply.LegendPosition = list(x = 0.04, y = -0.3, orientation = "h")
  )
)

{"x":{"visdat":{"6a786992a31":["function () ","plotlyVisDat"],"6a789f11748":["function () ","data"],"6a78752d6ae":["function () ","data"],"6a7859c96e0e":["function () ","data"],"6a785b305de6":["function () ","data"],"6a785fa1444d":["function () ","data"],"6a7825e46df":["function () ","data"],"6a78747154ef":["function () ","data"],"6a7828877c77":["function () ","data"]},"cur_data":"6a7828877c77","attrs":{"6a789f11748":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Agronomía, Veterinaria Y Afines","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#D2D4DC","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a78752d6ae":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Bellas Artes","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#FF8ABF","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a7859c96e0e":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Ciencias De La Educación","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#945BC2","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a785b305de6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Ciencias De La Salud","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#D11879","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a785fa1444d":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Ciencias Sociales Y Humanas","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#FF7F7F","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a7825e46df":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Economía, Administración, Contaduría Y Afines","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#FFA568","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a78747154ef":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Ingeniería, Arquitectura, Urbanismo Y Afines","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#9CFF86","line":{"color":"#3A4750","width":1.5}},"inherit":true},"6a7828877c77":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"color":{},"name":"Matemáticas Y Ciencias Naturales","type":"bar","orientation":"v","hovertemplate":{},"marker":{"color":"#89D8FF","line":{"color":"#3A4750","width":1.5}},"inherit":true}},"layout":{"margin":{"b":0,"l":50,"t":110,"r":50},"barmode":"stack","title":{"text":"<b>BARRAS APILADAS EN FUNCIÓN DEL ÁREA DEL SNIES<\/b>","font":{"family":"Open Sans","size":24,"color":"#333333"},"y":0.94999999999999996},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["2018","2019","2020"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Porcentaje","ticksuffix":"%"},"showlegend":true,"legend":{"x":0.040000000000000001,"y":-0.29999999999999999,"orientation":"h","traceorder":"normal","title":{"text":"<b>NIVEL ACADÉMICO:<\/b>"}},"autosize":true,"annotations":[{"x":0.5,"y":1.5,"text":"Se considera únicamente los valores obtenidos en el<br />primer periodo académico de cada año.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}},{"x":0.5,"y":1.5,"text":"Se considera únicamente los valores obtenidos en el<br />primer periodo académico de cada año.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"x":["2018","2019","2020"],"y":[2.4199999999999999,5.2788000000000004,2.7000000000000002],"name":"Agronomía, Veterinaria Y Afines","type":"bar","orientation":"v","hovertemplate":["(2018): 2.42%","(2019): 5.28%","(2020): 2.7%"],"marker":{"color":"#D2D4DC","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[5.6791999999999998,3.7545999999999999,6.1509999999999998],"name":"Bellas Artes","type":"bar","orientation":"v","hovertemplate":["(2018): 5.68%","(2019): 3.75%","(2020): 6.15%"],"marker":{"color":"#FF8ABF","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[0.25369999999999998,0.37169999999999997,0.3654],"name":"Ciencias De La Educación","type":"bar","orientation":"v","hovertemplate":["(2018): 0.25%","(2019): 0.37%","(2020): 0.37%"],"marker":{"color":"#945BC2","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[9.5043000000000006,12.119,11.287000000000001],"name":"Ciencias De La Salud","type":"bar","orientation":"v","hovertemplate":["(2018): 9.5%","(2019): 12.12%","(2020): 11.29%"],"marker":{"color":"#D11879","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[22.599499999999999,29.591100000000001,20.341000000000001],"name":"Ciencias Sociales Y Humanas","type":"bar","orientation":"v","hovertemplate":["(2018): 22.6%","(2019): 29.59%","(2020): 20.34%"],"marker":{"color":"#FF7F7F","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[9.2310999999999996,6.2081999999999997,8.4044000000000008],"name":"Economía, Administración, Contaduría Y Afines","type":"bar","orientation":"v","hovertemplate":["(2018): 9.23%","(2019): 6.21%","(2020): 8.4%"],"marker":{"color":"#FFA568","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(255,217,47,1)"},"error_y":{"color":"rgba(255,217,47,1)"},"error_x":{"color":"rgba(255,217,47,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[38.973500000000001,31.6357,39.6265],"name":"Ingeniería, Arquitectura, Urbanismo Y Afines","type":"bar","orientation":"v","hovertemplate":["(2018): 38.97%","(2019): 31.64%","(2020): 39.63%"],"marker":{"color":"#9CFF86","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(229,196,148,1)"},"error_y":{"color":"rgba(229,196,148,1)"},"error_x":{"color":"rgba(229,196,148,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2018","2019","2020"],"y":[11.338800000000001,11.040900000000001,11.124599999999999],"name":"Matemáticas Y Ciencias Naturales","type":"bar","orientation":"v","hovertemplate":["(2018): 11.34%","(2019): 11.04%","(2020): 11.12%"],"marker":{"color":"#89D8FF","line":{"color":"#3A4750","width":1.5}},"textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# Ejemplo usando el caso estático (ggplot2)
Plot.Apiladas(
  datos     = ejConsolidadoGrad |> filter(YEAR %in% c(2019:2021), SEMESTRE == 1),
  categoria = "NIVEL",
  colores   = c("#FFA700", "#C10AA1", "#01CDFE", "#00FF44", "#FF0040"),
  titulo    = gsub("L AC", "L\nAC", Txt),
  estatico  = TRUE,
  estilo    = list(
    LegendTitle = "NIVEL ACAD\u00c9MICO:", gg.Tema = 8,
    gg.Legend = list(legend.position = "right", legend.direction = "vertical"),
    gg.Bar    = list(width = 0.6, color = "#000000"),
    gg.Texto  = list(
      subtitle = "\u00bb\u00bb\u00bb", tag = "\u00ae",
      caption  = "Informaci\u00f3n Disponible desde 2009-1"
    )
  )
)
#> Warning: 
#>     ¡Ha ingresado un dataframe que no está de forma condensada, es decir,
#>     para cada categoría existe más de un valor para un mismo punto del eje X!
#>     Se sumará los valores por defectos para dichos puntos que gocen de +1 valor
#>            
```
