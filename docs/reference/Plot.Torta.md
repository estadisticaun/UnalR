# Cree un gráfico circular/torta/pie dinámico/estático y flexible con dos diferentes paquetes

Esta función permite mostrar de forma interactiva (*y estática*) una
descripción compacta y general de una variable con sus respectivas
categorías. Dicho diagrama se puede representar usando dos diferentes
librerías que son `Highcharter` y `Plotly`, las cuales usan internamente
`JavaScript`.

## Uso

``` r
Plot.Torta(
  datos,
  valores,
  categoria,
  ano,
  periodo,
  colores,
  titulo = "",
  label = "",
  addPeriodo = FALSE,
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

- colores:

  Cadena de caracteres indicando los colores con los cuales se deben
  colorear cada una de las series correspondiente a cada nivel del
  argumento `categoria`. Si no se introduce algún vector se usará la
  paleta `rainbow` por defecto.

- titulo:

  Cadena de caracteres indicando el título principal del plot.

- label:

  Cadena de caracteres indicando la etiqueta a la que hace referencia el
  plot.

- addPeriodo:

  Argument deprecated. This Argument still exist but will be removed in
  the next version.

- libreria:

  Cadena de caracteres que indica el paquete con el cual se realizará el
  plot. Los valores permitidos son `"highcharter"` (*valor
  predeterminado*) y `"plotly"`. Los valores se emparejarán
  parcialmente.

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados de
  acuerdo con la librería especificada para graficar la torta y cuyo
  objetivo es personalizar pequeños detalles de ésta.

  - `LegendTitle`, `hc.Tema`, `hc.Credits` y `ply.Credits`: Igual uso
    que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `ply.Legend`: Igual uso que en
    [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md)

- estatico:

  Si es `FALSE` (*valor predeterminado*) el gráfico a retornar será
  dinámico (*dependiendo de la librería seleccionada*), en caso
  contrario se retornará un gráfico estático construido con `ggplot2`.

## Valor

Retorna el diagrama circular (*objeto widget de HTML*) creado. La clase
del objeto retornado será un "htmlwidget" y dependiendo de la librería
usada pertenecerá adicionalmente a la clase "highchart" o "plotly".

## Detalles

Al usar el paquete `Highcharter` y usar las opciones de descarga, el
nombre del archivo descargado será la concatenación del plot graficado y
la categoría usada, así, por ejemplo, si se graficó el diagrama de pie
para la categoría "Sexo" el nombre será `PlotTorta_Sexo.png`.

## Nota

Los gráficos circulares son una forma muy mala de mostrar información.
El ojo es bueno para juzgar medidas lineales y malo para juzgar áreas
relativas. Un gráfico de barras o un gráfico de puntos es una forma
preferible de mostrar este tipo de datos.

## Lista de argumentos de estilo

Sabemos que puede ser abrumador el número de argumentos dentro del
parámetro `estilo`, pero es necesario si queremos ofrecer al usuario la
máxima personalización dentro de cada función usando cualquier librería.
Por tal razón, a continuación, se detalla el listado completo de
argumentos, usados al especificar la librería y en qué función están
presentes (*marcado con una × si lo posee*).

|  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| **Librería** | **estilo\$** | [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md) | [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md) | [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md) | [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md) | [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md) | [`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md) | `Plot.Torta()` | [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md) |
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
  Prevalence = round(runif(200)*100)
)
Plot.Torta(
  datos     = Blood     ,
  valores   = Prevalence,
  categoria = Group     ,
  colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
  label     = "No. of Prevalence"
)
#> Warning: ¡Se usará la librería 'highcharter' por defecto para realizar el plot!

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Total"},"type":"linear"},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotTorta_Group"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"pie":{"allowPointSelect":true,"colorByPoint":true,"colors":["#FF553D","#A5FF67","#40D2FF","#FFDB5C"],"dataLabels":{"enabled":true,"format":"<b>{point.name}<\/b>: {point.percentage:.1f} %","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}}},"series":[{"group":"group","data":[{"Clase":"A","Total":2395,"y":2395,"name":"A"},{"Clase":"AB","Total":2473,"y":2473,"name":"AB"},{"Clase":"B","Total":1402,"y":1402,"name":"B"},{"Clase":"O","Total":3173,"y":3173,"name":"O"}],"type":"pie","name":"No. of Prevalence","showInLegend":true}],"xAxis":{"type":"category","title":{"text":"Clase"},"categories":null},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","title":{"text":"","style":{"textDecoration":"underline"}},"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}Plot.Torta(
  datos     = Blood     ,
  valores   = Prevalence,
  categoria = Group     ,
  colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
  titulo    = "DISTRIBUTION OF BLOOD GROUPS",
  estatico  = TRUE,
  estilo    = list(gg.Tema = 6)
)

Plot.Torta(
  datos     = Blood     ,
  valores   = Prevalence,
  categoria = Group     ,
  colores   = c("#FF553D", "#A5FF67", "#40D2FF", "#FFDB5C"),
  titulo    = "DISTRIBUTION OF BLOOD GROUPS",
  estatico  = TRUE,
  estilo = list(
    gg.Tema  = 7, gg.Donut = TRUE, gg.Percent = FALSE,
    gg.Texto = list(
      subtitle = "Synthetic or fake data that resembles real-world data",
      caption  = "* Data Simulation",
      tag      = "\u00ae"
    )
  )
)

# ---------------------------------------------------------------------------
col <- c("#F15A24", "#8CC63F")
Msj <- "Distribuci\u00f3n de estudiantes graduados en el primer periodo acad\u00e9mico del 2021."
Txt <- "DISTRIBUCI\u00d3N DE GRADUADOS POR MODALIDAD DE FORMACI\u00d3N"
Plot.Torta(
  datos     = ejConsolidadoGrad |> filter(YEAR==2021, SEMESTRE==1),
  categoria = "TIPO_NIVEL",
  colores   = col,
  titulo    = paste(Txt, "(Periodo 2021-1)"),
  label     = "N\u00famero de Graduados",
  libreria  = "highcharter",
  estilo    = list(
    LegendTitle = "\u00c9sta es una descripci\u00f3n para la leyenda:",
    hc.Tema = 7, hc.Credits = Msj
  )
)

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"DISTRIBUCIÓN DE GRADUADOS POR MODALIDAD DE FORMACIÓN (Periodo 2021-1)","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Total"},"type":"linear"},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotTorta_Clase"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"pie":{"allowPointSelect":true,"colorByPoint":true,"colors":["#F15A24","#8CC63F"],"dataLabels":{"enabled":true,"format":"<b>{point.name}<\/b>: {point.percentage:.1f} %","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}}},"series":[{"group":"group","data":[{"Clase":"Postgrado","Total":1581,"y":1581,"name":"Postgrado"},{"Clase":"Pregrado","Total":2819,"y":2819,"name":"Pregrado"}],"type":"pie","name":"Número de Graduados","showInLegend":true}],"xAxis":{"type":"category","title":{"text":"Clase"},"categories":null},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","title":{"text":"Ésta es una descripción para la leyenda:","style":{"textDecoration":"underline"}},"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"subtitle":{"text":"Distribución de estudiantes graduados en el primer periodo académico del 2021.","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#F45B5B","#8085E9","#8D4654","#7798BF","#AAEEEE","#FF0066","#EEAAEE","#55BF3B","#DF5353"],"chart":{"backgroundColor":null,"divBackgroundImage":"https://www.highcharts.com/samples/graphics/sand.png","style":{"fontFamily":"Signika, serif"}},"title":{"style":{"color":"black","fontSize":"16px","fontWeight":"bold"}},"subtitle":{"style":{"color":"black"}},"tooltip":{"borderWidth":0},"legend":{"itemStyle":{"fontWeight":"bold","fontSize":"13px"}},"xAxis":{"labels":{"style":{"color":"#6e6e70"}}},"yAxis":{"labels":{"style":{"color":"#6e6e70"}}},"plotOptions":{"series":{"shadow":false},"candlestick":{"lineColor":"#404048"},"map":{"shadow":false}},"navigator":{"xAxis":{"gridLineColor":"#D0D0D8"}},"rangeSelector":{"buttonTheme":{"fill":"white","stroke":"#C0C0C8","stroke-width":1,"states":{"select":{"fill":"#D0D0D8"}}}},"scrollbar":{"trackBorderColor":"#C0C0C8"},"background2":"#E0E0E8"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Signika","debug":false},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Msj <- "Distribuci\u00f3n hist\u00f3rica de estudiantes graduados (desde el 2009-I al 2021-I)."
Plot.Torta(
  datos     = ejConsolidadoGrad,
  categoria = "TIPO_NIVEL",
  colores   = col,
  titulo    = gsub("DOS POR", "DOS\nPOR", Txt),
  libreria  = "plotly",
  estilo    = list(
    ply.Legend = "inside", ply.Credits = list(
      x = 0.8, y = 1.1, text = paste0("<b>", Msj, "</b>")
    )
  )
)

{"x":{"visdat":{"6a78490218ee":["function () ","plotlyVisDat"]},"cur_data":"6a78490218ee","attrs":{"6a78490218ee":{"labels":["Postgrado","Pregrado"],"values":[41821,68232],"textposition":"inside","textinfo":"label+value+percent","insidetextfont":{"color":"#FFFFFF","size":20},"hoverinfo":"label+value","insidetextorientation":"radial","marker":{"colors":["#F15A24","#8CC63F"],"line":{"color":"#000000","width":1.5}},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"pie"}},"layout":{"margin":{"b":0,"l":50,"t":110,"r":50},"title":{"text":"<b>DISTRIBUCIÓN DE GRADUADOS<br />POR MODALIDAD DE FORMACIÓN<\/b>","font":{"family":"Open Sans","size":24,"color":"#333333"},"y":0.94999999999999996},"showlegend":false,"autosize":true,"annotations":[{"x":0.80000000000000004,"y":1.1000000000000001,"text":"<b>Distribución histórica de estudiantes graduados (desde el 2009-I al 2021-I).<\/b>","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}},{"x":0.80000000000000004,"y":1.1000000000000001,"text":"<b>Distribución histórica de estudiantes graduados (desde el 2009-I al 2021-I).<\/b>","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#2B908F"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"labels":["Postgrado","Pregrado"],"values":[41821,68232],"textposition":["inside","inside"],"textinfo":"label+value+percent","insidetextfont":{"color":"#FFFFFF","size":20},"hoverinfo":["label+value","label+value"],"insidetextorientation":"radial","marker":{"color":"rgba(31,119,180,1)","colors":["#F15A24","#8CC63F"],"line":{"color":"#000000","width":1.5}},"type":"pie","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
