# Cree un diagrama de árbol (*treemap*) dinámico y flexible con diversos paquetes

Esta función proporciona excelentes herramientas y opciones para la
visualización de datos jerárquicos/estructurados como un conjunto de
rectángulos anidados. Cada grupo está representado por un rectángulo,
cuya área (*tamaño*) es proporcional a su frecuencia absoluta
(*recuento*) y el color se usa para mostrar otra dimensión numérica.
Usando la interactividad, es posible representar varias
dimensiones/niveles: grupos, subgrupos, etc. Dicha gráfica se va a
representar usando la librería `Highcharter`, `Plotly`, `d3treeR`, entre
otras, las cuales usan internamente `JavaScript`.

## Uso

``` r
Plot.Treemap(
  datos,
  variables,
  atributo,
  textFreq = "N",
  metodo = c("Classic", "Classic2", "Sunburst", "Sunburst2"),
  estadistico = c("Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
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

- variables:

  Una lista (*ya sea creada con la sintaxis `base` o `tidy`*) con las
  variables categóricas dentro del data frame ingresado en `datos` con
  las que se desea crear la jerarquía (*recuerde que esta se crea de
  izquierda a derecha, es decir la primera hace referencia al grupo, la
  segunda al subgrupo, etc.*).

- atributo:

  Una variable numérica dentro del data frame ingresado en `datos`. Este
  es opcional y solo aplica en el caso de un nivel, es decir, cuando se
  ingresa únicamente una variable.

- textFreq:

  Cadena de caracteres indicando el nombre que se le va a dar al
  recuento en cada uno de los grupos. Por defecto se emplea el rótulo
  "N".

- metodo:

  Cadena de caracteres indicando el diseño con el cual se realizará el
  gráfico (*en el caso de ingresar dos niveles o más*). Los valores
  permitidos son `"Classic"` (*valor predeterminado*), `"Classic2"`,
  `"Sunburst"` y `"Sunburst2"`, así se usará las funciones
  d3treeR::d3tree(), d3treeR::d3tree2(),
  [sunburst()](https://rdrr.io/pkg/sunburstR/man/sunburst.html) y
  [sund2b()](https://rdrr.io/pkg/sunburstR/man/sund2b.html)
  respectivamente.

- estadistico:

  Igual uso que en
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)

- colores:

  Igual uso que en
  [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md),
  con algunos matices, cuando usamos `Highcharter` y nos encontramos en
  el caso de un nivel y especificamos un atributo es recomendable
  pasarle una escala de colores, pues con esto se construirá la barra
  horizontal. En el caso de usar el argumento `atributo` se puede
  ingresar el nombre de una paleta, por ejemplo "Set1".

- titulo:

  Cadena de caracteres indicando el título principal del plot.

- libreria:

  Igual uso que en
  [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md),
  con algunos matices, pues en el caso de ingresar más de una variable
  categórica se omitirá dicho argumento, ya que `metodo` tomará su
  lugar.

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados
  para graficar el treemap y cuyo objetivo es personalizar pequeños
  detalles de éste.

  - `hc.Tema` y `hc.Credits`: Igual uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `hc.borderRadius`: Un número entero positivo que indica el radio del
    borde de cada elemento. El valor por defecto es 0 (*rectángulos*).

  - `ply.Opacidad`: Igual uso que en
    [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md)

  - `ply.Credits`: Igual uso que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `sun.Explanation`: Cadena de caracteres indicando qué es lo que se
    desea ver en el centro del sunburst al pasar el mouse por los
    diferentes anillos (*niveles de la jerarquía*). Los valores
    permitidos son "All" (*valor predeterminado*), "Count" y "Percent".
    Solo aplica para cuando `metodo = "Sunburst"`.

  - `sun.Color`: A diferencia del argumento `colores` acá puede pasar
    una paleta o vector de colores sin que se recorte (*más no se
    recicle*) éste a la longitud de categorías del nodo padre. Su uso
    reemplaza el funcionamiento del argumento `colores`. Solo aplica
    para cuando `metodo = "Sunburst"`.

  - `sun.showLabels`: Si es `FALSE` (*valor predeterminado*) no se
    mostrará etiquetas en los cortes. Solo aplica para cuando
    `metodo = "Sunburst2"`.

  - `sun.colorRoot`: Cadena de caracteres que indica el color del nodo
    raíz (*root*). Puede indicar el color con el nombre (`"red"`),
    código hexadecimal (`"#FF0000"`) o RGB (`rgb(1, 0, 0)`). El valor
    por defecto es "rojo". Solo aplica para cuando
    `metodo = "Sunburst2"`.

  - `gg.fontsize.title`: Tamaño de la fuente del título. El valor por
    defecto es `14`. Para más detalles, consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.fontsize.labels`: Tamaño de la fuente de las etiquetas. Si
    ingresa un número especificará el tamaño para todos los niveles de
    agregación, por el contrario, si ingresa un vector podrá especificar
    el tamaño para cada nivel. El valor por defecto es `11`. Para más
    detalles, consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.fontcolor.labels`: Especifica los colores de la etiqueta. Ya sea
    una cadena de caracteres o un vector (*uno para cada nivel de
    agregación*). El valor por defecto es `NULL`. Para más detalles,
    consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.border.lwds`: Tamaño de las líneas de borde. Si ingresa un
    número especificará el grosor para todos los rectángulos, o un
    vector para especificar el grueso para cada nivel de agregación.
    Para más detalles, consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.border.col`: Color de los bordes dibujados alrededor de cada
    rectángulo, ya sea un valor único o un vector. El valor por defecto
    es `'#000000'`. Para más detalles, consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.lowerbound.cex.labels`: Número entre \\\[0, 1\]\\, 0 significa
    dibujar todas las etiquetas y 1 significa dibujar sólo las etiquetas
    si encajan (*considerando el `fontsize.labels`*). El valor por
    defecto es `0.4`. Para más detalles, consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.force.print.labels`: Si es `FALSE` (*valor predeterminado*) las
    etiquetas de datos no se ven obligadas a imprimirse si no encajan.
    Para más detalles consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

  - `gg.overlap.labels`: Número entre \\\[0, 1\]\\, que determina la
    tolerancia de superposición entre etiquetas. 0 significa que las
    etiquetas de los niveles inferiores no se imprimen si las etiquetas
    de los niveles superiores se superponen, 1 significa que las
    etiquetas siempre se imprimen. El valor por defecto es `0.5`. Para
    más detalles, consulte la función
    [treemap()](https://rdrr.io/pkg/treemap/man/treemap.html).

- estatico:

  Si es `FALSE` (*valor predeterminado*) el gráfico a retornar será
  dinámico (*dependiendo de la librería seleccionada*), en caso
  contrario se retornará un gráfico estático construido con `ggplot2`.

## Valor

Retorna el treemap (*objeto widget de HTML*) creado. La clase del objeto
retornado será un "htmlwidget" y dependiendo de la librería usada
pertenecerá adicionalmente a la clase "highchart", "plotly", " d3tree",
"d3tree2", "sunburst" o "sund2b".

## Detalles

Si está trabajando en un `R Markdown` o un aplicativo `Shiny` no se
puede usar de forma conjunta el `método = Classic` (o `Classic2`) y
`método = Sunburst` (o `Sunburst2`), pues se trata de un problema
interno, ya que usan versiones diferentes de `d3`, puede darle
seguimiento al problema
[aquí](https://github.com/timelyportfolio/sunburstR/issues/102). De
igual forma, si utiliza la librería `sunburstR` en algunas ocasiones se
le verán afectadas las tablas creadas con `DT`.

## Lista de argumentos de estilo

Sabemos que puede ser abrumador el número de argumentos dentro del
parámetro `estilo`, pero es necesario si queremos ofrecer al usuario la
máxima personalización dentro de cada función usando cualquier librería.
Por tal razón, a continuación, se detalla el listado completo de
argumentos, usados al especificar la librería y en qué función están
presentes (*marcado con una × si lo posee*).

|  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| **Librería** | **estilo\$** | [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md) | [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md) | [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md) | [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md) | [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md) | `Plot.Treemap()` | [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md) | [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md) |
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
library(viridis)
Msj <- "Acompa\u00f1ado del Estad\u00edstico seleccionado para la Variable Edad."
Plot.Treemap(
  datos       = ejGraduados,
  variables   = SEDE_NOMBRE_MAT,
  atributo    = EDAD_MOD,
  textFreq    = "Tamaño de la Muestra",
  estadistico = "Max",
  colores     = inferno(10),
  titulo      = "TOTAL DE GRADUADOS POR SEDE DE LA UNIVERSIDAD NACIONAL",
  libreria    = "highcharter",
  estilo      = list(hc.Tema = 7, hc.borderRadius = 20, hc.Credits = Msj)
)

{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"TOTAL DE GRADUADOS POR SEDE DE LA UNIVERSIDAD NACIONAL","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":[]},"type":"linear"},"credits":{"enabled":true,"text":"DNPE","href":"http://estadisticas.unal.edu.co/home/"},"exporting":{"enabled":true,"filename":"PlotTreemap_SEDE_NOMBRE_MAT"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified","borderRadius":20},"scatter":{"marker":{"symbol":"circle"}}},"colorAxis":{"stops":[[0,"#000004"],[0.1111111111111111,"#1B0C42"],[0.2222222222222222,"#4B0C6B"],[0.3333333333333333,"#781C6D"],[0.4444444444444444,"#A52C60"],[0.5555555555555556,"#CF4446"],[0.6666666666666666,"#ED6925"],[0.7777777777777778,"#FB9A06"],[0.8888888888888888,"#F7D03C"],[1,"#FCFFA4"]]},"series":[{"group":"group","data":[{"X":"Amazonía","Y":69,"n":23,"Porcentaje":0.083,"value":23,"colorValue":69,"name":"Amazonía"},{"X":"Bogotá","Y":82,"n":17118,"Porcentaje":61.509,"value":17118,"colorValue":82,"name":"Bogotá"},{"X":"Caribe","Y":52,"n":26,"Porcentaje":0.093,"value":26,"colorValue":52,"name":"Caribe"},{"X":"Manizales","Y":74,"n":3666,"Porcentaje":13.173,"value":3666,"colorValue":74,"name":"Manizales"},{"X":"Medellín","Y":74,"n":5671,"Porcentaje":20.377,"value":5671,"colorValue":74,"name":"Medellín"},{"X":"Palmira","Y":68,"n":1326,"Porcentaje":4.765,"value":1326,"colorValue":68,"name":"Palmira"}],"type":"treemap","name":"Tamaño de la Muestra","dataLabels":{"enabled":true,"format":"{point.name}<br/>{point.Porcentaje: .1f}%"}}],"xAxis":{"type":"category","title":{"text":"X"},"categories":null},"tooltip":{"pointFormat":"{series.name}: {point.n} <br> <b>Max: {point.Y: .2f}<\/b>","useHTML":true},"subtitle":{"text":"Acompañado del Estadístico seleccionado para la Variable Edad.","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#F45B5B","#8085E9","#8D4654","#7798BF","#AAEEEE","#FF0066","#EEAAEE","#55BF3B","#DF5353"],"chart":{"backgroundColor":null,"divBackgroundImage":"https://www.highcharts.com/samples/graphics/sand.png","style":{"fontFamily":"Signika, serif"}},"title":{"style":{"color":"black","fontSize":"16px","fontWeight":"bold"}},"subtitle":{"style":{"color":"black"}},"tooltip":{"borderWidth":0},"legend":{"itemStyle":{"fontWeight":"bold","fontSize":"13px"}},"xAxis":{"labels":{"style":{"color":"#6e6e70"}}},"yAxis":{"labels":{"style":{"color":"#6e6e70"}}},"plotOptions":{"series":{"shadow":false},"candlestick":{"lineColor":"#404048"},"map":{"shadow":false}},"navigator":{"xAxis":{"gridLineColor":"#D0D0D8"}},"rangeSelector":{"buttonTheme":{"fill":"white","stroke":"#C0C0C8","stroke-width":1,"states":{"select":{"fill":"#D0D0D8"}}}},"scrollbar":{"trackBorderColor":"#C0C0C8"},"background2":"#E0E0E8"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Signika","debug":false},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Plot.Treemap(
  datos       = ejGraduados,
  variables   = FACULTAD,
  atributo    = EDAD_MOD,
  textFreq    = "n",
  estadistico = "CV",
  colores     = turbo(10, direction = -1),
  titulo      = "TOTAL DE GRADUADOS POR FACULTAD EN LA UNAL",
  libreria    = "plotly",
  estilo      = list(ply.Credits = list(x = 0.6, y = 1, text = Msj))
)

{"x":{"visdat":{"6a7828c62415":["function () ","plotlyVisDat"]},"cur_data":"6a7828c62415","attrs":{"6a7828c62415":{"parents":null,"values":{},"labels":{},"text":{},"opacity":1,"textposition":"middle center","texttemplate":"<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","hovertemplate":"%{label}","marker":{"colors":["#7A0403FF","#CB2A04FF","#F66B19FF","#FABA39FF","#C7EF34FF","#72FE5EFF","#1AE4B6FF","#36AAF9FF","#4662D7FF","#30123BFF"]},"sort":true,"name":"","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"treemap"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b>TOTAL DE GRADUADOS POR FACULTAD EN LA UNAL<\/b>","font":{"family":"Old Standard TT","size":24,"color":"#333333"},"y":0.995},"autosize":true,"showlegend":true,"annotations":[{"x":0.59999999999999998,"y":1,"text":"Acompañado del Estadístico seleccionado para la Variable Edad.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}},{"x":0.59999999999999998,"y":1,"text":"Acompañado del Estadístico seleccionado para la Variable Edad.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"parents":[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],"values":[1130,23,607,1760,26,3002,1070,430,1346,300,2571,411,2736,388,3202,896,2236,1727,355,3376,238],"labels":["Administración","Amazonía","Arquitectura","Artes","Caribe","Ciencias","Ciencias agrarias","Ciencias agropecuarias","Ciencias económicas","Ciencias exactas y naturales","Ciencias humanas","Ciencias humanas y económicas","Derecho, ciencias políticas y sociales","Enfermería","Ingeniería","Ingeniería y administración","Ingeniería y arquitectura","Medicina","Medicina veterinaria y de zootecnia","Minas","Odontología"],"text":[0.21310745134577583,0.23748033919832168,0.22490918420817826,0.23313763022958686,0.22139291891328133,0.22671831445826762,0.21879107578215123,0.26502041018509165,0.2055319929532646,0.26247477375086459,0.23640889439301735,0.28382352690004986,0.24367765896323781,0.22947859960760686,0.1840426248488386,0.24334774560180789,0.2341791061869079,0.21357890055520781,0.19758666940034103,0.20398442871945863,0.15005884483324003],"opacity":1,"textposition":"middle center","texttemplate":["<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}","<b>%{label}<\/b><br /> n: %{value} (<i>%{percentParent}<\/i>)<br />CV: %{text: .2f}"],"hovertemplate":["%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}","%{label}"],"marker":{"color":"rgba(31,119,180,1)","colors":["#7A0403FF","#CB2A04FF","#F66B19FF","#FABA39FF","#C7EF34FF","#72FE5EFF","#1AE4B6FF","#36AAF9FF","#4662D7FF","#30123BFF"],"line":{"color":"rgba(255,255,255,1)"}},"sort":true,"name":"","type":"treemap","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}if (FALSE) { # all(FALSE)
# ---------------------------------------------------------------------------
# library(dplyr)
misColores <- c(
  "#29ABE2", # AZUL CLARO  | Amazonia
  "#8CC63F", # VERDE       | Bogota
  "#CC241D", # ROJO        | Caribe
  "#0071BC", # AZUL VIVO   | Manizales
  "#F15A24", # NARANJA     | Medellin
  "#FBB03B", # AMARILLO    | Orinoquia
  "#93278F", # MORADO      | Palmira
  "#8A381A"  # GRIS        | Tumaco
)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
  metodo    = "Classic",
  colores   = misColores # "Set3"
)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
  metodo    = "Classic2",
  colores   = "Set2"
)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
  metodo    = "Sunburst",
  colores   = misColores,
  estilo    = list(sun.Explanation = "All")
)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
  metodo    = "Sunburst",
  # colores   = misColores,
  estilo    = list(
    sun.Explanation = "All",
    sun.Color = list(range = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61",
                               "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4",
                               "#66C2A5", "#3288BD", "#5E4FA2"
                               )
                     )
  )
)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
  metodo    = "Sunburst2"
)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD, PROGRAMA),
  metodo    = "Sunburst2",
  colores   = misColores,
  estilo    = list(sun.showLabels = TRUE, sun.colorRoot = "#EF0055")
)
}
# ---------------------------------------------------------------------------
# Ejemplo usando el caso estático (treemap)
# library(dplyr)
Plot.Treemap(
  datos     = ejGraduados,
  variables = vars(SEDE_NOMBRE_MAT, FACULTAD),
  colores   = c("#FF3232", "#AFFF5E", "#FD6DB3", "#4CCAF2", "#FF9248", "#FBB03B"),
  titulo    = "TOTAL DE GRADUADOS \u00d7 SEDE",
  estatico  = TRUE,
  estilo    = list(
    gg.fontsize.title = 12, gg.fontsize.labels = c(15, 9),
    gg.fontcolor.labels = c("#FFFFFF", "#212020"),
    gg.border.lwds = c(4, 2), gg.border.col = c("#73095D", "#D60D4B"),
    gg.lowerbound.cex.labels = 0.3, gg.overlap.labels = 0.1
  )
)
```
