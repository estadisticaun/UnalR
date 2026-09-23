# Cree un gráfico de radar dinámico y flexible con dos diferentes paquetes

Esta función proporciona excelentes herramientas y opciones para la
visualización de un gráfico de radar (*también conocido como gráfico de
araña*) dinámico con el objetivo de observar datos multivariados de
forma bidimensional. Dicho radar chart o spider plot se puede
representar usando dos diferentes librerías que son `Plotly` y
`ECharts`, las cuales usan internamente `JavaScript`.

## Uso

``` r
Plot.Radar(
  datos,
  categoria,
  variables,
  estadistico = c("Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
  colores,
  rango,
  ordinal = FALSE,
  titulo = "",
  libreria = c("plotly", "echarts"),
  estilo = NULL,
  estatico = FALSE
)
```

## Argumentos

- datos:

  Un data frame, se espera en formato de microdatos no un agregado.

- categoria:

  Una variable categórica dentro del data frame ingresado en `datos`.

- variables:

  Lista (*ya sea creada con la sintaxis `base` o `tidy`*) con las
  variables numéricas (*mínimo tres para que se pueda realizar el
  gráfico*) dentro del data frame ingresado en `datos`.

- estadistico:

  Cadena de caracteres que indica el estadístico a graficar. Los valores
  permitidos son `"Promedio"` (*valor predeterminado*), `"Mediana"`,
  `"Varianza"`, `"SD"`, `"CV"`, `"Min"` y `"Max`".

- colores:

  Cadena de caracteres indicando los colores con los cuales se deben
  colorear cada una de las trazas correspondiente a cada nivel del
  argumento `categoria`. Si no se introduce algún vector se usará la
  paleta `rainbow` por defecto.

- rango:

  Vector numérico de longitud dos que indica el valor mínimo y máximo,
  respectivamente. Si no conoce el dominio del estadístico seleccionado
  omita éste parámetro, pues internamente se usará `c(0, NaN)` como
  rango.

- ordinal:

  Si es `TRUE` indicará que las categorías de la variable ingresada son
  ordinales (*no nominales*), esto con el fin de ordenar la disposición
  en el que se presentan en el eje del gráfico, el valor por defecto es
  `FALSE`.

- titulo:

  Cadena de caracteres indicando el título principal del plot.

- libreria:

  Cadena de caracteres que indica el paquete con el cual se realizará el
  radar. Los valores permitidos son `"plotly"` (*valor predeterminado*)
  o `"echarts"`. Los valores se emparejarán parcialmente.

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados de
  acuerdo con la librería especificada para graficar el radar y cuyo
  objetivo es personalizar pequeños detalles de éste.

  - `ply.LegendTitle`, `ply.LegendPosition` y `ply.Credits`: Igual uso
    que en
    [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)

  - `ply.Relleno`: Cadena de caracteres indicando cómo se debe rellenar
    el área, `toself` (*valor predeterminado*) conecta los puntos de la
    traza de forma cerrada y superpone las áreas, mientras que `tonext`
    deja visible la capa más profunda y si se comparten áreas no las
    superpone; finalmente especifique `none` si desea ver únicamente los
    polígonos y que no se rellene el área dentro de ellos.

  - `ply.Opacidad`: Un número entre \\\[0, 1\]\\ que indica la opacidad
    de los polígonos/trazos.

  - `e.Credits`: Cadena de caracteres indicando el subtítulo del gráfico
    principal. Para mayor información, consulte la función
    [e_title()](https://echarts4r.john-coene.com/reference/e_title.html).

  - `e.Forma`: Cadena de caracteres indicando el tipo de renderizado del
    radar, los valores admitidos son `polygon` (*valor predeterminado*)
    y `circle`. Para mayor información, consulte la función
    [e_radar_opts()](https://echarts4r.john-coene.com/reference/e_radar_opts.html).

  - `e.Tema`: Modifica el tema con el cual se creará el gráfico. Los
    posibles valores son un número entero entre \\\[1, 14\]\\ el cual
    hace referencia a diferentes temas disponibles en dicha librería
    (`helianthus`, `azul`, `inspired`, `macarons`, `westeros`, `walden`,
    `roma`, `royal`, `fruit`, `dark`, `chalk`, `purple-passion`,
    `vintage` y `essos` respectivamente). El tema por defecto se logra
    al no ingresar valor alguno. Para más información consulte
    [aquí](https://echarts4r.john-coene.com/articles/themes.html).

  - `e.LegType`: Cadena de caracteres indicando el tipo de leyenda, los
    valores admitidos son `plain` (*valor predeterminado*) y `scroll`
    (*útil cuando es necesario mostrar demasiados elementos*). Para
    mayor información consulte la función
    [e_legend()](https://echarts4r.john-coene.com/reference/e_legend.html).

  - `e.LegLoc`: Valor numérico o cadena de caracteres indicando la
    distancia entre la leyenda y el lado derecho del contenedor, puede
    ser expresado como un valor puntual o un valor porcentual relativo
    al ancho del contenedor.

  - `gg.Range`: Valor booleano opcional, si se especifica en `TRUE` el
    rango a tomar será simétrico, en el sentido en que se tomará el
    mínimo y máximo global de todas las variables, uno mismo para cada
    una de ellas. Diferente a si se omite el parámetro `rango`, pues acá
    el mínimo y máximo varía para cada variable.

  - `gg.plty`: Tipo de línea para los datos del gráfico. Para más
    detalles consulte la función
    [radarchart()](https://rdrr.io/pkg/fmsb/man/radarchart.html).

  - `gg.plwd`: Ancho de la línea para los datos del gráfico. Para más
    detalles consulte la función
    [radarchart()](https://rdrr.io/pkg/fmsb/man/radarchart.html).

  - `gg.cglwd`: Ancho de la línea para las grillas del radar. Para más
    detalles consulte la función
    [radarchart()](https://rdrr.io/pkg/fmsb/man/radarchart.html).

  - `gg.cglcol`: Color de la línea para las grillas del radar. Para más
    detalles consulte la función
    [radarchart()](https://rdrr.io/pkg/fmsb/man/radarchart.html).

- estatico:

  Si es `FALSE` (*valor predeterminado*) el gráfico a retornar será
  dinámico (*dependiendo de la librería seleccionada*), en caso
  contrario se retornará un gráfico estático construido con `ggplot2`.

## Valor

Retorna el radar (*objeto widget de HTML*) creado. La clase del objeto
retornado será un "htmlwidget" y dependiendo de la librería usada
pertenecerá adicionalmente a la clase "plotly" o "echarts4r".

## Lista de argumentos de estilo

Sabemos que puede ser abrumador el número de argumentos dentro del
parámetro `estilo`, pero es necesario si queremos ofrecer al usuario la
máxima personalización dentro de cada función usando cualquier librería.
Por tal razón, a continuación, se detalla el listado completo de
argumentos, usados al especificar la librería y en qué función están
presentes (*marcado con una × si lo posee*).

|  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| **Librería** | **estilo\$** | [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md) | [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md) | [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md) | [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md) | `Plot.Radar()` | [`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md) | [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md) | [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md) |
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
# library(dplyr)
Plot.Radar(
  datos     = ejSaberPro2020,
  categoria = TIPO_COL,
  variables = vars(PUNT_LECT_CRIT, PUNT_RAZO_CUANT, PUNT_INGLES),
  colores   = c("#2ACE82", "#FE2667", "#32E7C8", "#FF8D00"),
  rango     = c(0, NaN),
  estilo    = list(ply.Relleno = "tonext")
)
#> Warning: ¡Se usará como estadístico la media muestral ('mean') por defecto!
#> Warning: ¡Se usará la librería 'plotly' por defecto para realizar el plot!
#> Joining with `by = join_by(TIPO_COL)`

{"x":{"visdat":{"6a783e361f30":["function () ","plotlyVisDat"]},"cur_data":"6a783e361f30","attrs":{"6a783e361f30":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar"},"6a783e361f30.1":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[179.06817231283981]],[[184.06189878711837]],[[178.32455039732329]]],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":[{"TIPO_COL":"Oficial"}],"line":{"color":"#2ACE82","width":2},"marker":{"color":"#2ACE82","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#2ACE82"},"opacity":1,"hoverinfo":"text","text":["Oficial<br> Promedio: 179.068<br> N: 2391","Oficial<br> Promedio: 184.062<br> N: 2391","Oficial<br> Promedio: 178.325<br> N: 2391"],"inherit":true},"6a783e361f30.2":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[183.14736842105262]],[[191.55789473684212]],[[188.7578947368421]]],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":[{"TIPO_COL":"Otros"}],"line":{"color":"#FE2667","width":2},"marker":{"color":"#FE2667","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#FE2667"},"opacity":1,"hoverinfo":"text","text":["Otros<br> Promedio: 183.147<br> N: 95","Otros<br> Promedio: 191.558<br> N: 95","Otros<br> Promedio: 188.758<br> N: 95"],"inherit":true},"6a783e361f30.3":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[189.2945091514143]],[[193.88851913477538]],[[201.16805324459236]]],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":[{"TIPO_COL":"Privado"}],"line":{"color":"#32E7C8","width":2},"marker":{"color":"#32E7C8","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#32E7C8"},"opacity":1,"hoverinfo":"text","text":["Privado<br> Promedio: 189.295<br> N: 2404","Privado<br> Promedio: 193.889<br> N: 2404","Privado<br> Promedio: 201.168<br> N: 2404"],"inherit":true},"6a783e361f30.4":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[181.66666666666666]],[[162.33333333333334]],[[199]]],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":[{"TIPO_COL":null}],"line":{"color":"#FF8D00","width":2},"marker":{"color":"#FF8D00","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#FF8D00"},"opacity":1,"hoverinfo":"text","text":["NA<br> Promedio: 181.667<br> N: 6","NA<br> Promedio: 162.333<br> N: 6","NA<br> Promedio: 199<br> N: 6"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b><\/b>","font":{"family":"Old Standard TT","size":24,"color":"#333333"},"y":0.995},"autosize":true,"showlegend":true,"polar":{"radialaxis":{"visible":true,"range":0}},"legend":{"x":1,"y":0.5,"orientation":"v","title":{"text":"<b><\/b>","font":{"family":"Open Sans","size":14,"color":"#525252"}},"traceorder":"normal"},"annotations":[{"x":0,"y":0,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}},{"x":0,"y":0,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"fillcolor":"rgba(31,119,180,0.5)","fill":"tonext","mode":"markers+lines","type":"scatterpolar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"frame":null},{"fillcolor":["#2ACE82"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[179.06817231283981,184.06189878711837,178.32455039732329],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":"Oficial","line":{"color":"#2ACE82","width":2},"marker":{"color":"#2ACE82","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text"],"text":["Oficial<br> Promedio: 179.068<br> N: 2391","Oficial<br> Promedio: 184.062<br> N: 2391","Oficial<br> Promedio: 178.325<br> N: 2391"],"frame":null},{"fillcolor":["#FE2667"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[183.14736842105262,191.55789473684212,188.7578947368421],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":"Otros","line":{"color":"#FE2667","width":2},"marker":{"color":"#FE2667","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text"],"text":["Otros<br> Promedio: 183.147<br> N: 95","Otros<br> Promedio: 191.558<br> N: 95","Otros<br> Promedio: 188.758<br> N: 95"],"frame":null},{"fillcolor":["#32E7C8"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[189.2945091514143,193.88851913477538,201.16805324459236],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":"Privado","line":{"color":"#32E7C8","width":2},"marker":{"color":"#32E7C8","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text"],"text":["Privado<br> Promedio: 189.295<br> N: 2404","Privado<br> Promedio: 193.889<br> N: 2404","Privado<br> Promedio: 201.168<br> N: 2404"],"frame":null},{"fillcolor":["#FF8D00"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[181.66666666666666,162.33333333333334,199],"theta":["PUNT_LECT_CRIT","PUNT_RAZO_CUANT","PUNT_INGLES"],"name":"NA","line":{"color":"#FF8D00","width":2},"marker":{"color":"#FF8D00","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text"],"text":["NA<br> Promedio: 181.667<br> N: 6","NA<br> Promedio: 162.333<br> N: 6","NA<br> Promedio: 199<br> N: 6"],"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Plot.Radar(
  datos     = ejSaberPro2020,
  categoria = SEDE_NOMBRE_ADM,
  variables = vars(
    PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
    PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  rango     = c(0, NaN)
)
#> Warning: ¡Se usará como estadístico la media muestral ('mean') por defecto!
#> Warning: ¡Se usará la librería 'plotly' por defecto para realizar el plot!
#> Joining with `by = join_by(SEDE_NOMBRE_ADM)`

{"x":{"visdat":{"6a783a3640b2":["function () ","plotlyVisDat"]},"cur_data":"6a783a3640b2","attrs":{"6a783a3640b2":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar"},"6a783a3640b2.1":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[154.2093023255814]],[[159.88372093023256]],[[157.48837209302326]],[[162.62790697674419]],[[152.3953488372093]],[[145.41463414634146]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Amazonía"}],"line":{"color":"#FF000033","width":2},"marker":{"color":"#FF000033","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#FF000033"},"opacity":1,"hoverinfo":"text","text":["Amazonía<br> Promedio: 154.209<br> N: 43","Amazonía<br> Promedio: 159.884<br> N: 43","Amazonía<br> Promedio: 157.488<br> N: 43","Amazonía<br> Promedio: 162.628<br> N: 43","Amazonía<br> Promedio: 152.395<br> N: 43","Amazonía<br> Promedio: 145.415<br> N: 43"],"inherit":true},"6a783a3640b2.2":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[187.13264283327766]],[[192.50952221850986]],[[197.74006014032744]],[[189.95656531907784]],[[189.83294353491479]],[[168.3828125]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Bogotá"}],"line":{"color":"#FFBF0033","width":2},"marker":{"color":"#FFBF0033","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#FFBF0033"},"opacity":1,"hoverinfo":"text","text":["Bogotá<br> Promedio: 187.133<br> N: 2993","Bogotá<br> Promedio: 192.51<br> N: 2993","Bogotá<br> Promedio: 197.74<br> N: 2993","Bogotá<br> Promedio: 189.957<br> N: 2993","Bogotá<br> Promedio: 189.833<br> N: 2993","Bogotá<br> Promedio: 168.383<br> N: 2993"],"inherit":true},"6a783a3640b2.3":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[165.80000000000001]],[[172.19999999999999]],[[181.80000000000001]],[[161.19999999999999]],[[149.40000000000001]],[[163.59999999999999]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Caribe"}],"line":{"color":"#80FF0033","width":2},"marker":{"color":"#80FF0033","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#80FF0033"},"opacity":1,"hoverinfo":"text","text":["Caribe<br> Promedio: 165.8<br> N: 5","Caribe<br> Promedio: 172.2<br> N: 5","Caribe<br> Promedio: 181.8<br> N: 5","Caribe<br> Promedio: 161.2<br> N: 5","Caribe<br> Promedio: 149.4<br> N: 5","Caribe<br> Promedio: 163.6<br> N: 5"],"inherit":true},"6a783a3640b2.4":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[169.98552821997106]],[[180.67293777134589]],[[174.94356005788711]],[[172.56150506512301]],[[172.63531114327063]],[[151.91592920353983]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Manizales"}],"line":{"color":"#00FF4033","width":2},"marker":{"color":"#00FF4033","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#00FF4033"},"opacity":1,"hoverinfo":"text","text":["Manizales<br> Promedio: 169.986<br> N: 691","Manizales<br> Promedio: 180.673<br> N: 691","Manizales<br> Promedio: 174.944<br> N: 691","Manizales<br> Promedio: 172.562<br> N: 691","Manizales<br> Promedio: 172.635<br> N: 691","Manizales<br> Promedio: 151.916<br> N: 691"],"inherit":true},"6a783a3640b2.5":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[181.6478494623656]],[[197.53629032258064]],[[190.63844086021504]],[[183.94892473118279]],[[183.84677419354838]],[[155.78404401650619]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Medellín"}],"line":{"color":"#00FFFF33","width":2},"marker":{"color":"#00FFFF33","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#00FFFF33"},"opacity":1,"hoverinfo":"text","text":["Medellín<br> Promedio: 181.648<br> N: 744","Medellín<br> Promedio: 197.536<br> N: 744","Medellín<br> Promedio: 190.638<br> N: 744","Medellín<br> Promedio: 183.949<br> N: 744","Medellín<br> Promedio: 183.847<br> N: 744","Medellín<br> Promedio: 155.784<br> N: 744"],"inherit":true},"6a783a3640b2.6":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[172.85714285714286]],[[182.98214285714286]],[[174.91071428571428]],[[175.89285714285714]],[[176.98214285714286]],[[159.12962962962962]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Orinoquía"}],"line":{"color":"#0040FF33","width":2},"marker":{"color":"#0040FF33","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#0040FF33"},"opacity":1,"hoverinfo":"text","text":["Orinoquía<br> Promedio: 172.857<br> N: 56","Orinoquía<br> Promedio: 182.982<br> N: 56","Orinoquía<br> Promedio: 174.911<br> N: 56","Orinoquía<br> Promedio: 175.893<br> N: 56","Orinoquía<br> Promedio: 176.982<br> N: 56","Orinoquía<br> Promedio: 159.13<br> N: 56"],"inherit":true},"6a783a3640b2.7":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[157.09142857142857]],[[163.17714285714285]],[[157.37714285714284]],[[163.47714285714287]],[[163.38571428571427]],[[142.9378698224852]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Palmira"}],"line":{"color":"#8000FF33","width":2},"marker":{"color":"#8000FF33","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#8000FF33"},"opacity":1,"hoverinfo":"text","text":["Palmira<br> Promedio: 157.091<br> N: 350","Palmira<br> Promedio: 163.177<br> N: 350","Palmira<br> Promedio: 157.377<br> N: 350","Palmira<br> Promedio: 163.477<br> N: 350","Palmira<br> Promedio: 163.386<br> N: 350","Palmira<br> Promedio: 142.938<br> N: 350"],"inherit":true},"6a783a3640b2.8":{"fill":"toself","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[156.42857142857142]],[[163.14285714285714]],[[142.35714285714286]],[[157.21428571428572]],[[157.35714285714286]],[[162.42857142857142]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Tumaco"}],"line":{"color":"#FF00BF33","width":2},"marker":{"color":"#FF00BF33","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#FF00BF33"},"opacity":1,"hoverinfo":"text","text":["Tumaco<br> Promedio: 156.429<br> N: 14","Tumaco<br> Promedio: 163.143<br> N: 14","Tumaco<br> Promedio: 142.357<br> N: 14","Tumaco<br> Promedio: 157.214<br> N: 14","Tumaco<br> Promedio: 157.357<br> N: 14","Tumaco<br> Promedio: 162.429<br> N: 14"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b><\/b>","font":{"family":"Old Standard TT","size":24,"color":"#333333"},"y":0.995},"autosize":true,"showlegend":true,"polar":{"radialaxis":{"visible":true,"range":0}},"legend":{"x":1,"y":0.5,"orientation":"v","title":{"text":"<b><\/b>","font":{"family":"Open Sans","size":14,"color":"#525252"}},"traceorder":"normal"},"annotations":[{"x":0,"y":0,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}},{"x":0,"y":0,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"fillcolor":"rgba(31,119,180,0.5)","fill":"toself","mode":"markers+lines","type":"scatterpolar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"frame":null},{"fillcolor":["#FF000033"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[154.2093023255814,159.88372093023256,157.48837209302326,162.62790697674419,152.3953488372093,145.41463414634146],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Amazonía","line":{"color":"#FF000033","width":2},"marker":{"color":"#FF000033","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Amazonía<br> Promedio: 154.209<br> N: 43","Amazonía<br> Promedio: 159.884<br> N: 43","Amazonía<br> Promedio: 157.488<br> N: 43","Amazonía<br> Promedio: 162.628<br> N: 43","Amazonía<br> Promedio: 152.395<br> N: 43","Amazonía<br> Promedio: 145.415<br> N: 43"],"frame":null},{"fillcolor":["#FFBF0033"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[187.13264283327766,192.50952221850986,197.74006014032744,189.95656531907784,189.83294353491479,168.3828125],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Bogotá","line":{"color":"#FFBF0033","width":2},"marker":{"color":"#FFBF0033","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Bogotá<br> Promedio: 187.133<br> N: 2993","Bogotá<br> Promedio: 192.51<br> N: 2993","Bogotá<br> Promedio: 197.74<br> N: 2993","Bogotá<br> Promedio: 189.957<br> N: 2993","Bogotá<br> Promedio: 189.833<br> N: 2993","Bogotá<br> Promedio: 168.383<br> N: 2993"],"frame":null},{"fillcolor":["#80FF0033"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[165.80000000000001,172.19999999999999,181.80000000000001,161.19999999999999,149.40000000000001,163.59999999999999],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Caribe","line":{"color":"#80FF0033","width":2},"marker":{"color":"#80FF0033","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Caribe<br> Promedio: 165.8<br> N: 5","Caribe<br> Promedio: 172.2<br> N: 5","Caribe<br> Promedio: 181.8<br> N: 5","Caribe<br> Promedio: 161.2<br> N: 5","Caribe<br> Promedio: 149.4<br> N: 5","Caribe<br> Promedio: 163.6<br> N: 5"],"frame":null},{"fillcolor":["#00FF4033"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[169.98552821997106,180.67293777134589,174.94356005788711,172.56150506512301,172.63531114327063,151.91592920353983],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Manizales","line":{"color":"#00FF4033","width":2},"marker":{"color":"#00FF4033","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Manizales<br> Promedio: 169.986<br> N: 691","Manizales<br> Promedio: 180.673<br> N: 691","Manizales<br> Promedio: 174.944<br> N: 691","Manizales<br> Promedio: 172.562<br> N: 691","Manizales<br> Promedio: 172.635<br> N: 691","Manizales<br> Promedio: 151.916<br> N: 691"],"frame":null},{"fillcolor":["#00FFFF33"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[181.6478494623656,197.53629032258064,190.63844086021504,183.94892473118279,183.84677419354838,155.78404401650619],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Medellín","line":{"color":"#00FFFF33","width":2},"marker":{"color":"#00FFFF33","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Medellín<br> Promedio: 181.648<br> N: 744","Medellín<br> Promedio: 197.536<br> N: 744","Medellín<br> Promedio: 190.638<br> N: 744","Medellín<br> Promedio: 183.949<br> N: 744","Medellín<br> Promedio: 183.847<br> N: 744","Medellín<br> Promedio: 155.784<br> N: 744"],"frame":null},{"fillcolor":["#0040FF33"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[172.85714285714286,182.98214285714286,174.91071428571428,175.89285714285714,176.98214285714286,159.12962962962962],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Orinoquía","line":{"color":"#0040FF33","width":2},"marker":{"color":"#0040FF33","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Orinoquía<br> Promedio: 172.857<br> N: 56","Orinoquía<br> Promedio: 182.982<br> N: 56","Orinoquía<br> Promedio: 174.911<br> N: 56","Orinoquía<br> Promedio: 175.893<br> N: 56","Orinoquía<br> Promedio: 176.982<br> N: 56","Orinoquía<br> Promedio: 159.13<br> N: 56"],"frame":null},{"fillcolor":["#8000FF33"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[157.09142857142857,163.17714285714285,157.37714285714284,163.47714285714287,163.38571428571427,142.9378698224852],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Palmira","line":{"color":"#8000FF33","width":2},"marker":{"color":"#8000FF33","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Palmira<br> Promedio: 157.091<br> N: 350","Palmira<br> Promedio: 163.177<br> N: 350","Palmira<br> Promedio: 157.377<br> N: 350","Palmira<br> Promedio: 163.477<br> N: 350","Palmira<br> Promedio: 163.386<br> N: 350","Palmira<br> Promedio: 142.938<br> N: 350"],"frame":null},{"fillcolor":["#FF00BF33"],"fill":"toself","mode":"markers+lines","type":"scatterpolar","r":[156.42857142857142,163.14285714285714,142.35714285714286,157.21428571428572,157.35714285714286,162.42857142857142],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Tumaco","line":{"color":"#FF00BF33","width":2},"marker":{"color":"#FF00BF33","size":6,"line":{"color":"#787878","width":1}},"opacity":1,"hoverinfo":["text","text","text","text","text","text"],"text":["Tumaco<br> Promedio: 156.429<br> N: 14","Tumaco<br> Promedio: 163.143<br> N: 14","Tumaco<br> Promedio: 142.357<br> N: 14","Tumaco<br> Promedio: 157.214<br> N: 14","Tumaco<br> Promedio: 157.357<br> N: 14","Tumaco<br> Promedio: 162.429<br> N: 14"],"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Plot.Radar(
  datos     = ejSaberPro2020,
  categoria = SEDE_NOMBRE_ADM,
  variables = vars(
    PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
    PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  rango     = c(0, NaN),
  libreria  = "echarts"
)
#> Warning: ¡Se usará como estadístico la media muestral ('mean') por defecto!
#> Joining with `by = join_by(SEDE_NOMBRE_ADM)`

{"x":{"theme":"","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"radar":[{"indicator":[{"name":"PUNTAJE_GLOBAL","max":"197.74"},{"name":"PUNT_RAZO_CUANT","max":"197.74"},{"name":"PUNT_INGLES","max":"197.74"},{"name":"PUNT_LECT_CRIT","max":"197.74"},{"name":"PUNT_COMP_CIUD","max":"197.74"},{"name":"PUNT_COMU_ESCR","max":"197.74"}],"shape":"polygon"}],"series":[{"type":"radar","data":[{"value":[154.209,159.884,157.488,162.628,152.395,145.415],"name":"Amazonía"},{"value":[187.133,192.51,197.74,189.957,189.833,168.383],"name":"Bogotá"},{"value":[165.8,172.2,181.8,161.2,149.4,163.6],"name":"Caribe"},{"value":[169.986,180.673,174.944,172.562,172.635,151.916],"name":"Manizales"},{"value":[181.648,197.536,190.638,183.949,183.847,155.784],"name":"Medellín"},{"value":[172.857,182.982,174.911,175.893,176.982,159.13],"name":"Orinoquía"},{"value":[157.091,163.177,157.377,163.477,163.386,142.938],"name":"Palmira"},{"value":[156.429,163.143,142.357,157.214,157.357,162.429],"name":"Tumaco"}],"radarIndex":0}],"legend":{"data":["Amazonía","Bogotá","Caribe","Manizales","Medellín","Orinoquía","Palmira","Tumaco"],"show":true,"type":"plain","right":null},"tooltip":{"trigger":"item"},"title":[{"text":"","subtext":""}]},"dispose":true},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
misColores <- c(
  "#29ABE2", # AZUL CLARO  | Amazonia
  "#8CC63F", # VERDE       | Bogota
  "#CC241D", # ROJO        | Caribe
  "#0071BC", # AZUL VIVO   | Manizales
  "#F15A24", # NARANJA     | Medellin
  "#FBB03B", # AMARILLO    | Orinoquia
  "#93278F", # MORADO      | Palmira
  "#8A381A" # GRIS        | Tumaco
)
Msj <- "Gr\u00e1fico de radar para representar los puntajes multivariados de la prueba Saber Pro."

Plot.Radar(
  datos       = ejSaberPro2020,
  categoria   = SEDE_NOMBRE_ADM,
  variables   = vars(
    PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
    PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  estadistico = "SD",
  colores     = misColores,
  rango       = c(0, NaN),
  titulo      = "SPIDER PLOT",
  libreria    = "plotly",
  estilo      = list(
    ply.LegendTitle = "SEDE:", ply.LegendPosition = list(x = 0, y = -0.15, orientation = "h"),
    ply.Relleno = "tonext", ply.Opacidad = 0.8, ply.Credits = list(x = 0.8, y = -0.1, text = Msj)
  )
)
#> Joining with `by = join_by(SEDE_NOMBRE_ADM)`

{"x":{"visdat":{"6a7832416b7e":["function () ","plotlyVisDat"]},"cur_data":"6a7832416b7e","attrs":{"6a7832416b7e":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar"},"6a7832416b7e.1":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[24.698716121958437]],[[38.3779508264127]],[[25.740807044201659]],[[38.477589801643262]],[[39.104950079353749]],[[30.522922214096816]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Amazonía"}],"line":{"color":"#29ABE2","width":2},"marker":{"color":"#29ABE2","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#29ABE2"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Amazonía<br> SD: 24.699<br> N: 43","Amazonía<br> SD: 38.378<br> N: 43","Amazonía<br> SD: 25.741<br> N: 43","Amazonía<br> SD: 38.478<br> N: 43","Amazonía<br> SD: 39.105<br> N: 43","Amazonía<br> SD: 30.523<br> N: 43"],"inherit":true},"6a7832416b7e.2":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[20.399226851724666]],[[29.113586668119002]],[[33.018659481070145]],[[23.82064748254745]],[[28.73161160237748]],[[38.84591292319864]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Bogotá"}],"line":{"color":"#8CC63F","width":2},"marker":{"color":"#8CC63F","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#8CC63F"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Bogotá<br> SD: 20.399<br> N: 2993","Bogotá<br> SD: 29.114<br> N: 2993","Bogotá<br> SD: 33.019<br> N: 2993","Bogotá<br> SD: 23.821<br> N: 2993","Bogotá<br> SD: 28.732<br> N: 2993","Bogotá<br> SD: 38.846<br> N: 2993"],"inherit":true},"6a7832416b7e.3":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[17.894133116750865]],[[27.270863572685045]],[[34.780741797724787]],[[26.414011433328337]],[[33.783131885602316]],[[32.608281156785921]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Caribe"}],"line":{"color":"#CC241D","width":2},"marker":{"color":"#CC241D","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#CC241D"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Caribe<br> SD: 17.894<br> N: 5","Caribe<br> SD: 27.271<br> N: 5","Caribe<br> SD: 34.781<br> N: 5","Caribe<br> SD: 26.414<br> N: 5","Caribe<br> SD: 33.783<br> N: 5","Caribe<br> SD: 32.608<br> N: 5"],"inherit":true},"6a7832416b7e.4":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[20.926020667788112]],[[27.717103073847145]],[[29.705530613788909]],[[25.188855829936998]],[[27.463179524173135]],[[33.775744226108131]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Manizales"}],"line":{"color":"#0071BC","width":2},"marker":{"color":"#0071BC","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#0071BC"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Manizales<br> SD: 20.926<br> N: 691","Manizales<br> SD: 27.717<br> N: 691","Manizales<br> SD: 29.706<br> N: 691","Manizales<br> SD: 25.189<br> N: 691","Manizales<br> SD: 27.463<br> N: 691","Manizales<br> SD: 33.776<br> N: 691"],"inherit":true},"6a7832416b7e.5":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[21.506383807933204]],[[32.494103827125031]],[[35.096746514028673]],[[24.289473960454949]],[[29.631758186346016]],[[35.988387607345956]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Medellín"}],"line":{"color":"#F15A24","width":2},"marker":{"color":"#F15A24","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#F15A24"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Medellín<br> SD: 21.506<br> N: 744","Medellín<br> SD: 32.494<br> N: 744","Medellín<br> SD: 35.097<br> N: 744","Medellín<br> SD: 24.289<br> N: 744","Medellín<br> SD: 29.632<br> N: 744","Medellín<br> SD: 35.988<br> N: 744"],"inherit":true},"6a7832416b7e.6":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[16.744528084697425]],[[26.001741949638383]],[[22.118915628290384]],[[20.638603345477332]],[[23.826258296584978]],[[31.415082482899951]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Orinoquía"}],"line":{"color":"#FBB03B","width":2},"marker":{"color":"#FBB03B","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#FBB03B"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Orinoquía<br> SD: 16.745<br> N: 56","Orinoquía<br> SD: 26.002<br> N: 56","Orinoquía<br> SD: 22.119<br> N: 56","Orinoquía<br> SD: 20.639<br> N: 56","Orinoquía<br> SD: 23.826<br> N: 56","Orinoquía<br> SD: 31.415<br> N: 56"],"inherit":true},"6a7832416b7e.7":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[18.817406004201935]],[[24.268614817615358]],[[30.193654675184483]],[[25.150738517463115]],[[26.673496959205529]],[[28.627749774824203]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Palmira"}],"line":{"color":"#93278F","width":2},"marker":{"color":"#93278F","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#93278F"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Palmira<br> SD: 18.817<br> N: 350","Palmira<br> SD: 24.269<br> N: 350","Palmira<br> SD: 30.194<br> N: 350","Palmira<br> SD: 25.151<br> N: 350","Palmira<br> SD: 26.673<br> N: 350","Palmira<br> SD: 28.628<br> N: 350"],"inherit":true},"6a7832416b7e.8":{"fill":"tonext","mode":"markers+lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatterpolar","r":[[[13.443410085438691]],[[21.643464612662189]],[[24.336130603431037]],[[26.17449895612895]],[[24.827426344691848]],[[24.206528953074702]]],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":[{"SEDE_NOMBRE_ADM":"Tumaco"}],"line":{"color":"#8A381A","width":2},"marker":{"color":"#8A381A","size":6,"line":{"width":1,"color":"#787878"}},"fillcolor":{"color":"#8A381A"},"opacity":0.80000000000000004,"hoverinfo":"text","text":["Tumaco<br> SD: 13.443<br> N: 14","Tumaco<br> SD: 21.643<br> N: 14","Tumaco<br> SD: 24.336<br> N: 14","Tumaco<br> SD: 26.174<br> N: 14","Tumaco<br> SD: 24.827<br> N: 14","Tumaco<br> SD: 24.207<br> N: 14"],"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b>SPIDER PLOT<\/b>","font":{"family":"Old Standard TT","size":24,"color":"#333333"},"y":0.995},"autosize":true,"showlegend":true,"polar":{"radialaxis":{"visible":true,"range":0}},"legend":{"x":0,"y":-0.14999999999999999,"orientation":"h","title":{"text":"<b>SEDE:<\/b>","font":{"family":"Open Sans","size":14,"color":"#525252"}},"traceorder":"normal"},"annotations":[{"x":0.80000000000000004,"y":-0.10000000000000001,"text":"Gráfico de radar para representar los puntajes multivariados de la prueba Saber Pro.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}},{"x":0.80000000000000004,"y":-0.10000000000000001,"text":"Gráfico de radar para representar los puntajes multivariados de la prueba Saber Pro.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}}],"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"fillcolor":"rgba(31,119,180,0.5)","fill":"tonext","mode":"markers+lines","type":"scatterpolar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"line":{"color":"rgba(31,119,180,1)"},"frame":null},{"fillcolor":["#29ABE2"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[24.698716121958437,38.3779508264127,25.740807044201659,38.477589801643262,39.104950079353749,30.522922214096816],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Amazonía","line":{"color":"#29ABE2","width":2},"marker":{"color":"#29ABE2","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Amazonía<br> SD: 24.699<br> N: 43","Amazonía<br> SD: 38.378<br> N: 43","Amazonía<br> SD: 25.741<br> N: 43","Amazonía<br> SD: 38.478<br> N: 43","Amazonía<br> SD: 39.105<br> N: 43","Amazonía<br> SD: 30.523<br> N: 43"],"frame":null},{"fillcolor":["#8CC63F"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[20.399226851724666,29.113586668119002,33.018659481070145,23.82064748254745,28.73161160237748,38.84591292319864],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Bogotá","line":{"color":"#8CC63F","width":2},"marker":{"color":"#8CC63F","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Bogotá<br> SD: 20.399<br> N: 2993","Bogotá<br> SD: 29.114<br> N: 2993","Bogotá<br> SD: 33.019<br> N: 2993","Bogotá<br> SD: 23.821<br> N: 2993","Bogotá<br> SD: 28.732<br> N: 2993","Bogotá<br> SD: 38.846<br> N: 2993"],"frame":null},{"fillcolor":["#CC241D"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[17.894133116750865,27.270863572685045,34.780741797724787,26.414011433328337,33.783131885602316,32.608281156785921],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Caribe","line":{"color":"#CC241D","width":2},"marker":{"color":"#CC241D","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Caribe<br> SD: 17.894<br> N: 5","Caribe<br> SD: 27.271<br> N: 5","Caribe<br> SD: 34.781<br> N: 5","Caribe<br> SD: 26.414<br> N: 5","Caribe<br> SD: 33.783<br> N: 5","Caribe<br> SD: 32.608<br> N: 5"],"frame":null},{"fillcolor":["#0071BC"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[20.926020667788112,27.717103073847145,29.705530613788909,25.188855829936998,27.463179524173135,33.775744226108131],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Manizales","line":{"color":"#0071BC","width":2},"marker":{"color":"#0071BC","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Manizales<br> SD: 20.926<br> N: 691","Manizales<br> SD: 27.717<br> N: 691","Manizales<br> SD: 29.706<br> N: 691","Manizales<br> SD: 25.189<br> N: 691","Manizales<br> SD: 27.463<br> N: 691","Manizales<br> SD: 33.776<br> N: 691"],"frame":null},{"fillcolor":["#F15A24"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[21.506383807933204,32.494103827125031,35.096746514028673,24.289473960454949,29.631758186346016,35.988387607345956],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Medellín","line":{"color":"#F15A24","width":2},"marker":{"color":"#F15A24","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Medellín<br> SD: 21.506<br> N: 744","Medellín<br> SD: 32.494<br> N: 744","Medellín<br> SD: 35.097<br> N: 744","Medellín<br> SD: 24.289<br> N: 744","Medellín<br> SD: 29.632<br> N: 744","Medellín<br> SD: 35.988<br> N: 744"],"frame":null},{"fillcolor":["#FBB03B"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[16.744528084697425,26.001741949638383,22.118915628290384,20.638603345477332,23.826258296584978,31.415082482899951],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Orinoquía","line":{"color":"#FBB03B","width":2},"marker":{"color":"#FBB03B","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Orinoquía<br> SD: 16.745<br> N: 56","Orinoquía<br> SD: 26.002<br> N: 56","Orinoquía<br> SD: 22.119<br> N: 56","Orinoquía<br> SD: 20.639<br> N: 56","Orinoquía<br> SD: 23.826<br> N: 56","Orinoquía<br> SD: 31.415<br> N: 56"],"frame":null},{"fillcolor":["#93278F"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[18.817406004201935,24.268614817615358,30.193654675184483,25.150738517463115,26.673496959205529,28.627749774824203],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Palmira","line":{"color":"#93278F","width":2},"marker":{"color":"#93278F","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Palmira<br> SD: 18.817<br> N: 350","Palmira<br> SD: 24.269<br> N: 350","Palmira<br> SD: 30.194<br> N: 350","Palmira<br> SD: 25.151<br> N: 350","Palmira<br> SD: 26.673<br> N: 350","Palmira<br> SD: 28.628<br> N: 350"],"frame":null},{"fillcolor":["#8A381A"],"fill":"tonext","mode":"markers+lines","type":"scatterpolar","r":[13.443410085438691,21.643464612662189,24.336130603431037,26.17449895612895,24.827426344691848,24.206528953074702],"theta":["PUNTAJE_GLOBAL","PUNT_RAZO_CUANT","PUNT_INGLES","PUNT_LECT_CRIT","PUNT_COMP_CIUD","PUNT_COMU_ESCR"],"name":"Tumaco","line":{"color":"#8A381A","width":2},"marker":{"color":"#8A381A","size":6,"line":{"color":"#787878","width":1}},"opacity":0.80000000000000004,"hoverinfo":["text","text","text","text","text","text"],"text":["Tumaco<br> SD: 13.443<br> N: 14","Tumaco<br> SD: 21.643<br> N: 14","Tumaco<br> SD: 24.336<br> N: 14","Tumaco<br> SD: 26.174<br> N: 14","Tumaco<br> SD: 24.827<br> N: 14","Tumaco<br> SD: 24.207<br> N: 14"],"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Plot.Radar(
  datos       = ejSaberPro2020,
  categoria   = SEDE_NOMBRE_ADM,
  variables   = vars(
    PUNTAJE_GLOBAL, PUNT_RAZO_CUANT, PUNT_INGLES,
    PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  estadistico = "CV",
  colores     = misColores,
  rango       = c(0, 0.25),
  titulo      = "RADAR CHART",
  libreria    = "echarts",
  estilo      = list(
    e.Credits = Msj, e.Forma = "circle", e.Tema = 10,
    e.LegType = "scroll", e.LegLoc = 0
  )
)
#> Joining with `by = join_by(SEDE_NOMBRE_ADM)`

{"x":{"theme":"dark","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"radar":[{"indicator":[{"name":"PUNTAJE_GLOBAL","max":"0.25"},{"name":"PUNT_RAZO_CUANT","max":"0.25"},{"name":"PUNT_INGLES","max":"0.25"},{"name":"PUNT_LECT_CRIT","max":"0.25"},{"name":"PUNT_COMP_CIUD","max":"0.25"},{"name":"PUNT_COMU_ESCR","max":"0.25"}],"shape":"circle"}],"series":[{"type":"radar","data":[{"value":[0.16,0.24,0.163,0.237,0.257,0.21],"name":"Amazonía"},{"value":[0.109,0.151,0.167,0.125,0.151,0.231],"name":"Bogotá"},{"value":[0.108,0.158,0.191,0.164,0.226,0.199],"name":"Caribe"},{"value":[0.123,0.153,0.17,0.146,0.159,0.222],"name":"Manizales"},{"value":[0.118,0.164,0.184,0.132,0.161,0.231],"name":"Medellín"},{"value":[0.097,0.142,0.126,0.117,0.135,0.197],"name":"Orinoquía"},{"value":[0.12,0.149,0.192,0.154,0.163,0.2],"name":"Palmira"},{"value":[0.08599999999999999,0.133,0.171,0.166,0.158,0.149],"name":"Tumaco"}],"radarIndex":0}],"legend":{"data":["Amazonía","Bogotá","Caribe","Manizales","Medellín","Orinoquía","Palmira","Tumaco"],"show":true,"type":"scroll","right":0},"tooltip":{"trigger":"item"},"title":[{"text":"RADAR CHART","subtext":"Gráfico de radar para representar los puntajes multivariados de la prueba Saber Pro."}],"color":["#29ABE2","#8CC63F","#CC241D","#0071BC","#F15A24","#FBB03B","#93278F","#8A381A"]},"dispose":true},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
# Ejemplo usando el caso estático (fmsb)
Plot.Radar(
  datos     = ejSaberPro2020,
  categoria = TIPO_COL,
  variables = vars(
    PUNT_RAZO_CUANT, PUNT_INGLES, PUNT_LECT_CRIT, PUNT_COMP_CIUD, PUNT_COMU_ESCR
  ),
  estadistico = "SD",
  colores  = c("#89D8FF", "#9CFF86", "#FFA568", "#FF7F7F"),
  titulo   = "RADAR CHART DE LA DESVIACI\u00d3N EST\u00c1NDAR\nPOR COMPONENTE EVALUADO",
  # rango    = c(10, 40),
  estatico = TRUE,
  estilo   = list(
    gg.Range = TRUE, gg.plty = 5, gg.plwd = 4, gg.cglwd = 2, gg.cglcol = "#856AA1"
  )
)
#> Joining with `by = join_by(TIPO_COL)`
```
