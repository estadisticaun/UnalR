# Cree un gráfico de barras que muestre la información de forma horizontal o vertical, para variables nominales u ordinales con dos diferentes paquetes

Esta función permite mostrar de forma interactiva (*y estática*) un
gráfico de barras verticales u horizontales cuya altura/longitud es
proporcional al valor de la variable (*categorías de una variable
cualitativa*), lo anterior para ayudar a la creación de informes
descriptivos y analíticos. Dicho diagrama se puede representar usando
dos diferentes librerías que son `Highcharter` y `Plotly`, las cuales
usan internamente `JavaScript`.

## Uso

``` r
Plot.Barras(
  datos,
  valores,
  categoria,
  ano,
  periodo,
  freqRelativa = FALSE,
  ylim,
  vertical = TRUE,
  ordinal = FALSE,
  colores,
  titulo = "",
  labelX = "",
  labelY = "Número de",
  labelEje,
  addPeriodo = FALSE,
  textInfo = labelY,
  libreria = c("highcharter", "plotly"),
  estilo = NULL,
  estatico = FALSE
)
```

## Argumentos

- datos:

  Un data frame, no un vector numérico.

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

- freqRelativa:

  Si es `FALSE` (*valor predeterminado*) la serie graficada representará
  las frecuencias absolutas (*conteo*) más no las relativas
  (*porcentaje*).

- ylim:

  Vector numérico que especifica el límite inferior y superior,
  respectivamente, del eje `Y`. Si no se introduce algún valor se
  mostrará todo el rango disponible para dicho eje.

- vertical:

  Si es `TRUE` (*valor predeterminado*) indicará que la orientación del
  gráfico será vertical.

- ordinal:

  Si es `TRUE` indicará que las categorías de la variable ingresada son
  ordinales (*no nominales*), esto con el fin de ordenar la disposición
  en el que se presentan en el eje del gráfico, el valor por defecto es
  `FALSE`.

- colores:

  Cadena de caracteres indicando los colores con los cuales se deben
  colorear cada una de las series correspondiente a cada nivel del
  argumento `categoria`. Si no se introduce algún vector se usará la
  paleta `rainbow` por defecto.

- titulo:

  Cadena de caracteres indicando el título principal del plot.

- labelX:

  Cadena de caracteres indicando la etiqueta del eje `X`. Por defecto se
  emplea el rótulo "Periodo".

- labelY:

  Cadena de caracteres indicando la etiqueta del eje `Y`.

- labelEje:

  Cadena de caracteres indicando la etiqueta del eje `X` o `Y`
  (*dependiendo de la orientación del gráfico*). Por defecto se emplea
  el rótulo `"Número de "`.

- addPeriodo:

  Argument deprecated. This Argument still exist but will be removed in
  the next version.

- textInfo:

  Cadena de caracteres que especifica el texto que se escribe dentro de
  la caja de información al posar el cursor en alguna barra en el
  gráfico, producido por `Highcharter`, el valor por defecto es igual al
  de `labelX`.

- libreria:

  Cadena de caracteres que indica el paquete con el cual se realizará el
  plot. Los valores permitidos son `"highcharter"` (*valor
  predeterminado*) y `"plotly"`. Los valores se emparejarán
  parcialmente.

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados de
  acuerdo con la librería especificada para graficar el plot y cuyo
  objetivo es personalizar pequeños detalles de ésta.

  - `hc.Tema`, `hc.Credits`, `ply.Credits`, `gg.Tema` y `gg.Texto`:
    Igual uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `ply.Legend`: Por defecto la gráfica muestra la leyenda fuera del
    gráfico de pie, si se introduce la cadena de texto `"inside"` se
    resumirá toda la información dentro del pie.

  - `gg.Bar`: Una lista de parámetros admitidos por la función
    [geom_bar()](https://ggplot2.tidyverse.org/reference/geom_bar.html)).

- estatico:

  Si es `FALSE` (*valor predeterminado*) el gráfico a retornar será
  dinámico (*dependiendo de la librería seleccionada*), en caso
  contrario se retornará un gráfico estático construido con `ggplot2`.

## Valor

Retorna el diagrama de barras (*objeto widget de HTML*) creado. La clase
del objeto retornado será un "htmlwidget" y dependiendo de la librería
usada pertenecerá adicionalmente a la clase "highchart" o "plotly".

## Detalles

Al usar el paquete `Highcharter` y usar las opciones de descarga, el
nombre del archivo descargado será la concatenación del plot graficado y
la categoría usada, así, por ejemplo, si se graficó el diagrama de
barras para la categoría "Nacionalidad" el nombre será
`PlotBarras_Nacionalidad.png`.

## Lista de argumentos de estilo

Sabemos que puede ser abrumador el número de argumentos dentro del
parámetro `estilo`, pero es necesario si queremos ofrecer al usuario la
máxima personalización dentro de cada función usando cualquier librería.
Por tal razón, a continuación, se detalla el listado completo de
argumentos, usados al especificar la librería y en qué función están
presentes (*marcado con una × si lo posee*).

|  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| **Librería** | **estilo\$** | [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md) | `Plot.Barras()` | [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md) | [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md) | [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md) | [`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md) | [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md) | [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md) |
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
  Group = sample(c("O", "A", "B", "AB"), size = 200, prob = c(0.5, 0.3, 0.16, 0.4), replace = TRUE),
  RH    = sample(c("+", "-"), size = 200, replace = TRUE),
  Prevalence = round(runif(200)*100)
)
Plot.Barras(
  datos     = Blood     ,
  valores   = Prevalence,
  categoria = Group     ,
  ordinal   = TRUE      ,
  colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
  labelY    = "Prevalence"
)
#> Warning: ¡Se usará la librería 'highcharter' por defecto para realizar el plot!

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Prevalence","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"labels":{"format":"{value}","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotBarras_Group"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"},"bar":{"tooltip":{"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}<\/b> ({point.Extra}%)<br/>"},"colorByPoint":true,"colors":["#FF553D","#A5FF67","#40D2FF","#FFDB5C"],"dataLabels":{"enabled":true,"pointFormat":"{point.y}","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"column":{"tooltip":{"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}<\/b> ({point.Extra}%)<br/>"},"colorByPoint":true,"colors":["#FF553D","#A5FF67","#40D2FF","#FFDB5C"],"dataLabels":{"enabled":true,"pointFormat":"{point.y}","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}}},"series":[{"group":"group","data":[{"Clase":"A","Y":2386,"Extra":25.4,"y":2386,"name":"A"},{"Clase":"AB","Y":2286,"Extra":24.3,"y":2286,"name":"AB"},{"Clase":"B","Y":1238,"Extra":13.2,"y":1238,"name":"B"},{"Clase":"O","Y":3480,"Extra":37.1,"y":3480,"name":"O"}],"type":"column","name":"Prevalence","showInLegend":false}],"xAxis":{"categories":["A","AB","B","O"],"title":{"text":"","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}Plot.Barras(
  datos     = Blood     ,
  valores   = Prevalence,
  categoria = Group     ,
  colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
  labelY    = "Prevalence",
  libreria  = "plotly"
)

{"x":{"visdat":{"6a786a944ee2":["function () ","plotlyVisDat"]},"cur_data":"6a786a944ee2","attrs":{"6a786a944ee2":{"x":["O","A","AB","B"],"y":[3480,2386,2286,1238],"orientation":"v","hovertemplate":["3480 (37.1%)","2386 (25.4%)","2286 (24.3%)","1238 (13.2%)"],"marker":{"color":["#FF553D","#A5FF67","#40D2FF","#FFDB5C"],"line":{"color":"#3A4750","width":1.5}},"color":["O","A","AB","B"],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b><\/b>","font":{"family":"Open Sans","size":24,"color":"#333333"},"y":0.94999999999999996},"xaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["B","AB","A","O"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Prevalence","ticksuffix":"","range":[]},"showlegend":true,"autosize":true,"annotations":[{"x":0.11,"y":1.1000000000000001,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}},{"x":0.11,"y":1.1000000000000001,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"x":["B"],"y":[1238],"orientation":"v","hovertemplate":"1238 (13.2%)","marker":{"color":"#FF553D","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"B","textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["AB"],"y":[2286],"orientation":"v","hovertemplate":"2286 (24.3%)","marker":{"color":"#A5FF67","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"AB","textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["A"],"y":[2386],"orientation":"v","hovertemplate":"2386 (25.4%)","marker":{"color":"#40D2FF","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"A","textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["O"],"y":[3480],"orientation":"v","hovertemplate":"3480 (37.1%)","marker":{"color":"#FFDB5C","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"O","textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Msj <- "Ac\u00e1 puede ir m\u00e1s informaci\u00f3n acerca del gr\u00e1fico."
Plot.Barras(
  datos        = ejConsolidadoGrad |> filter(YEAR==2021, SEMESTRE==1),
  categoria    = "NIVEL",
  freqRelativa = TRUE,
  vertical     = TRUE,
  ordinal      = TRUE,
  colores      = c("#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4", "#2B83BA"),
  titulo       = "GRADUADOS DE ACUERDO CON EL NIVEL DE FORMACI\u00d3N (Periodo 2021-1)",
  labelY       = "Frecuencia Relativa<br>(% de graduados)",
  textInfo     = "Porcentaje de Graduados",
  libreria     = "highcharter",
  estilo       = list(hc.Tema = 2, hc.Credits = Msj)
)

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"GRADUADOS DE ACUERDO CON EL NIVEL DE FORMACIÓN (Periodo 2021-1)","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Frecuencia Relativa<br>(% de graduados)","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"labels":{"format":"{value}%","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotBarras_Clase"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"},"bar":{"tooltip":{"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}%<\/b> ({point.Extra})<br/>"},"colorByPoint":true,"colors":["#D7191C","#FDAE61","#FFFFBF","#ABDDA4","#2B83BA"],"dataLabels":{"enabled":true,"pointFormat":"{point.y}%","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"column":{"tooltip":{"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}%<\/b> ({point.Extra})<br/>"},"colorByPoint":true,"colors":["#D7191C","#FDAE61","#FFFFBF","#ABDDA4","#2B83BA"],"dataLabels":{"enabled":true,"pointFormat":"{point.y}%","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}}},"series":[{"group":"group","data":[{"Clase":"Doctorado","Extra":95,"Y":2.2,"y":2.2,"name":"Doctorado"},{"Clase":"Especialidades Médicas","Extra":106,"Y":2.4,"y":2.4,"name":"Especialidades Médicas"},{"Clase":"Especialización","Extra":694,"Y":15.8,"y":15.8,"name":"Especialización"},{"Clase":"Maestría","Extra":686,"Y":15.6,"y":15.6,"name":"Maestría"},{"Clase":"Pregrado","Extra":2819,"Y":64.09999999999999,"y":64.09999999999999,"name":"Pregrado"}],"type":"column","name":"Porcentaje de Graduados","showInLegend":false}],"xAxis":{"categories":["Doctorado","Especialidades Médicas","Especialización","Maestría","Pregrado"],"title":{"text":"","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"subtitle":{"text":"Acá puede ir más información acerca del gráfico.","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#0266C8","#F90101","#F2B50F","#00933B"],"chart":{"style":{"fontFamily":"Roboto","color":"#444444"}},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#C0C0C0","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Roboto","debug":false},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Txt <- "DISTRIBUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR NIVEL"
Msj <- "A\u00f1o 2020, sin segregar por semestre (considerando ambos)."
Plot.Barras(
  datos     = ejConsolidadoGrad |> filter(YEAR == 2020),
  categoria = "NIVEL",
  vertical  = FALSE,
  ordinal   = FALSE,
  colores   = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854"),
  titulo    = Txt,
  labelY    = "N\u00famero de Graduados",
  libreria  = "plotly",
  estilo    = list(
    ply.Credits = list(x = 0.45, y = 1.1, text = Msj), ply.Legend = FALSE
  )
)

{"x":{"visdat":{"6a78229d5f6a":["function () ","plotlyVisDat"]},"cur_data":"6a78229d5f6a","attrs":{"6a78229d5f6a":{"x":[4969,1726,1625,266,153],"y":["Pregrado","Maestría","Especialización","Doctorado","Especialidades Médicas"],"orientation":"h","hovertemplate":["4969 (56.9%)","1726 (19.8%)","1625 (18.6%)","266 (3%)","153 (1.8%)"],"marker":{"color":["#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854"],"line":{"color":"#3A4750","width":1.5}},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":0,"l":50,"t":110,"r":50},"title":{"text":"<b>DISTRIBUCIÓN DEL NÚMERO DE GRADUADOS POR NIVEL<\/b>","font":{"family":"Open Sans","size":24,"color":"#333333"},"y":0.94999999999999996},"xaxis":{"domain":[0,1],"automargin":true,"title":"Número de Graduados","ticksuffix":""},"yaxis":{"domain":[0,1],"automargin":true,"title":"","type":"category","categoryorder":"array","categoryarray":["Especialidades Médicas","Doctorado","Especialización","Maestría","Pregrado"]},"showlegend":false,"autosize":true,"annotations":[{"x":0.45000000000000001,"y":1.1000000000000001,"text":"Año 2020, sin segregar por semestre (considerando ambos).","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}},{"x":0.45000000000000001,"y":1.1000000000000001,"text":"Año 2020, sin segregar por semestre (considerando ambos).","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"x":[266],"y":["Doctorado"],"orientation":"h","hovertemplate":"266 (3%)","marker":{"color":"#66C2A5","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"Doctorado","textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[153],"y":["Especialidades Médicas"],"orientation":"h","hovertemplate":"153 (1.8%)","marker":{"color":"#FC8D62","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"Especialidades Médicas","textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1625],"y":["Especialización"],"orientation":"h","hovertemplate":"1625 (18.6%)","marker":{"color":"#8DA0CB","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"Especialización","textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1726],"y":["Maestría"],"orientation":"h","hovertemplate":"1726 (19.8%)","marker":{"color":"#E78AC3","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"Maestría","textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[4969],"y":["Pregrado"],"orientation":"h","hovertemplate":"4969 (56.9%)","marker":{"color":"#A6D854","line":{"color":"#3A4750","width":1.5}},"type":"bar","name":"Pregrado","textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
# Ejemplo usando el caso estático (ggplot2)
Plot.Barras(
  datos     = ejConsolidadoGrad |> filter(YEAR == 2020),
  categoria = "NIVEL",
  vertical  = FALSE,
  ordinal   = FALSE,
  colores   = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"),
  titulo    = gsub("DE GR", "DE\nGR", Txt),
  labelY    = "N\u00famero de Graduados",
  estatico  = TRUE,
  estilo    = list(
    gg.Tema  = 10,
    gg.Bar   = list(width = 0.2, color = "#000000"),
    gg.Texto = list(subtitle = gsub("A", "\nA", Msj),
                    caption  = "Informaci\u00f3n Disponible desde 2009-1",
                    tag      = "\u00ae"
    )
  )
)
```
