# Cree un gráfico profundo (*drill down*) de torta/barras dinámico y flexible

Esta función proporciona excelentes herramientas y opciones para la
visualización de un gráfico drill down con el objetivo de poder
inspeccionar los datos con mayor nivel de detalle, sin la necesidad de
navegar o salir de él, pudiendo hacer clic en diversos elementos como
columnas o sectores circulares. Dicha gráfica se va a representar usando
la librería `Highcharter`, la cual usa internamente `JavaScript`.

## Uso

``` r
Plot.Drilldown(
  datos,
  varPrincipal,
  varSecundaria,
  ano,
  periodo,
  torta = TRUE,
  vertical = TRUE,
  colores,
  colores2,
  titulo = "",
  label = "",
  textInfo = "",
  addPeriodo = TRUE,
  estilo = NULL
)
```

## Argumentos

- datos:

  Un data frame, no un vector numérico.

- varPrincipal:

  Una variable categórica dentro del data frame ingresado en `datos`.

- varSecundaria:

  Otra variable categórica dentro del data frame ingresado en `datos`,
  diferente a la principal, pues se segregará a otros niveles.

- ano:

  Igual uso que en
  [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md)

- periodo:

  Igual uso que en
  [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md)

- torta:

  Si es `TRUE` (*valor predeterminado*) el primer nivel o gráfico
  principal será un diagrama de torta, defínalo en `FALSE` si desea que
  éste sea un gráfico de barras.

- vertical:

  Si es `TRUE` (*valor predeterminado*) indicará que tanto la
  orientación del gráfico principal como secundario será vertical.
  Solamente aplicará si el argumento `torta` es `FALSE`.

- colores:

  Cadena de caracteres indicando los colores con los cuales se deben
  colorear cada una de las trazas correspondiente a cada nivel del
  argumento `varPrincipal`. Si no se introduce algún vector se usará la
  paleta `rainbow` por defecto.

- colores2:

  Igual que `colores` pero aplicado al gráfico secundario.

- titulo:

  Igual uso que en
  [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

- label:

  Cadena de caracteres indicando el agregado al que hace referencia el
  gráfico. Por defecto no se emplea ningún rótulo.

- textInfo:

  Cadena de caracteres indicando el texto que aparecerá dentro de la
  caja de información al pasar el mouse por las diferentes columnas del
  gráfico de barras.

- addPeriodo:

  Igual uso que en
  [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md)

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados
  para graficar el drill down y cuyo objetivo es personalizar pequeños
  detalles de éste.

  - `LegendTitle`: Cadena de caracteres indicado un título para la
    leyenda (*diferentes niveles del argumento `varPrincipal`*).

  - `hc.Tema` y `hc.Credits`: Igual uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

## Valor

Retorna el diagrama drill down (*objeto widget de HTML*) creado. La
clase del objeto retornado será un "htmlwidget" y adicionalmente
pertenecerá a la clase "highchart".

## Ejemplos

``` r
# library(dplyr)
df <- ejMiniConsolidadoAsp |>
  filter(Clase != "Sin Información", tolower(Clase) != "no aplica")
text <- "DISTRIBUCI\u00d3N DE ASPIRANTES A PREGRADO EN SITUACI\u00d3N DE DISCAPACIDAD"
Msj  <- paste(
  "Discapacidad: Deficiencia, limitaci\u00f3n de la actividad ",
  "y la restricci\u00f3n de la participaci\u00f3n."
)
Plot.Drilldown(
  datos         = df,
  varPrincipal  = "DISCAPACIDAD",
  varSecundaria = "TIPO_DISC",
  ano           = max(df$YEAR),
  periodo       = slice(df, n())$SEMESTRE,
  torta         = TRUE, # Pruebe poniendo ambos valores ahora en FALSE
  vertical      = TRUE,
  colores       = c("#FF0040", "#00FF40"),
  colores2      = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"),
  titulo        = text,
  label         = "Aspirantes",
  textInfo      = "Aspirantes con discapacidades por tipo",
  addPeriodo    = TRUE,
  estilo        = list(hc.Tema = 7, hc.Credits = Msj)
)

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"DISTRIBUCIÓN DE ASPIRANTES A PREGRADO EN SITUACIÓN DE DISCAPACIDAD (Periodo 2021-2)","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Número de Aspirantes","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"type":"linear","labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotDrilldown_Discapacidad"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"pie":{"allowPointSelect":true,"colorByPoint":true,"colors":["#FF0040","#00FF40"],"dataLabels":{"enabled":true,"format":"<b>{point.name}<\/b>: {point.percentage:.1f} %","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"column":{"colorByPoint":true,"colors":["#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F"],"dataLabels":{"enabled":true,"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}}},"series":[{"group":"group","data":[{"Clase":"No","Total":23621,"drilldown":null,"y":23621,"name":"No"},{"Clase":"Sí","Total":133,"drilldown":"Sí","y":133,"name":"Sí"}],"type":"pie","name":"Total de Aspirantes","showInLegend":true}],"xAxis":{"type":"category","title":{"text":"Clase"},"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"drilldown":{"drillUpButton":{"theme":{"fill":"#00C0FF","states":{"hover":{"fill":"#FFC000"}}}},"activeDataLabelStyle":{"color":"#0080FF","textDecoration":"underline","fontStyle":"italic"},"activeAxisLabelStyle":{"textDecoration":"none"},"allowPointDrilldown":true,"series":[{"id":"Sí","name":"Aspirantes con discapacidades por tipo","data":[["Visual",54],["Motriz",35],["Psicosocial",17],["Auditiva",15],["Cognitiva",12],["Otras",0]],"type":"column"}]},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","title":{"text":"","style":{"textDecoration":"underline"}},"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"subtitle":{"text":"Discapacidad: Deficiencia, limitación de la actividad  y la restricción de la participación.","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#F45B5B","#8085E9","#8D4654","#7798BF","#AAEEEE","#FF0066","#EEAAEE","#55BF3B","#DF5353"],"chart":{"backgroundColor":null,"divBackgroundImage":"https://www.highcharts.com/samples/graphics/sand.png","style":{"fontFamily":"Signika, serif"}},"title":{"style":{"color":"black","fontSize":"16px","fontWeight":"bold"}},"subtitle":{"style":{"color":"black"}},"tooltip":{"borderWidth":0},"legend":{"itemStyle":{"fontWeight":"bold","fontSize":"13px"}},"xAxis":{"labels":{"style":{"color":"#6e6e70"}}},"yAxis":{"labels":{"style":{"color":"#6e6e70"}}},"plotOptions":{"series":{"shadow":false},"candlestick":{"lineColor":"#404048"},"map":{"shadow":false}},"navigator":{"xAxis":{"gridLineColor":"#D0D0D8"}},"rangeSelector":{"buttonTheme":{"fill":"white","stroke":"#C0C0C8","stroke-width":1,"states":{"select":{"fill":"#D0D0D8"}}}},"scrollbar":{"trackBorderColor":"#C0C0C8"},"background2":"#E0E0E8"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Signika","debug":false},"evals":[],"jsHooks":[]}
```
