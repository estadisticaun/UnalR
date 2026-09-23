# Cree una serie de tiempo dinámica/estática y flexible con tres diferentes paquetes

Esta función proporciona excelentes herramientas y opciones para la
visualización de series de tiempo dinámicas con el objetivo de estudiar
la evolución de una o varias variables a lo largo del tiempo. Dicha
serie interactiva se puede representar usando tres diferentes librerías
que son `Highcharter`, `Plotly` y `Dygraph`, las cuales usan
internamente `JavaScript`.

## Uso

``` r
Plot.Series(
  datos,
  tiempo,
  valores,
  categoria,
  freqRelativa = FALSE,
  invertir = FALSE,
  ylim,
  colores,
  titulo = "",
  labelX = "Periodo",
  labelY = "",
  libreria = c("highcharter", "plotly", "dygraphs"),
  estilo = NULL,
  estatico = FALSE
)
```

## Argumentos

- datos:

  Un data frame, no un objeto clase serie de tiempo o vector numérico.

- tiempo:

  Lista de variable(s) tanto numéricas como categóricas que se
  concatenaran para crear un único periodo temporal (*ordenado
  ascendentemente*).

- valores:

  Variable numérica que contiene los valores que desea graficar.

- categoria:

  Una variable categórica dentro del data frame ingresado en `datos`.

- freqRelativa:

  Si es `FALSE` (*valor predeterminado*) la serie graficada representará
  las frecuencias absolutas (*conteo*) más no las relativas
  (*porcentaje*).

- invertir:

  Si es `FALSE` (*valor predeterminado*) no se invertirá el eje `Y`.
  Establézcalo en `TRUE` si desea que en el eje `Y` el número más alto
  sea el más cercano al origen.

- ylim:

  Vector numérico que especifica el límite inferior y superior,
  respectivamente, del eje `Y`. Si no se introduce algún valor se
  mostrará todo el rango disponible para dicho eje.

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

- libreria:

  Cadena de caracteres que indica el paquete con el cual se realizará la
  serie. Los valores permitidos son `"highcharter"` (*valor
  predeterminado*), `"plotly"` o `"dygraphs"`. Los valores se
  emparejarán parcialmente.

- estilo:

  Lista compuesta por varios parámetros, los cuales van a ser usados de
  acuerdo con la librería especificada para graficar la serie y cuyo
  objetivo es personalizar pequeños detalles de ésta.

  - `LegendTitle`: Cadena de caracteres indicando un título para la
    leyenda (*diferentes niveles del argumento `categorias`*). Se
    utilizará tanto en el paquete `Highcharter` como en `Plotly`.

  - `hc.Tema`: Modifica el tema con el cual se creará la serie. Los
    posibles valores son un número entero entre \\\[1, 10\]\\ el cual
    hace referencia a diferentes temas disponibles en dicha librería
    (`ffx`, `google`, `tufte`, `538`, `ggplot2`, `economist`,
    `sandsignika`, `ft`, `superheroes` y `flatdark`, respectivamente).
    El tema por defecto, al no ingresar valor alguno, es
    `hc_theme_flat()`.

  - `hc.Slider`: Si es `TRUE` agrega un deslizador/navegador dinámico en
    la parte inferior de la serie. Proporciona herramientas para acercar
    y alejar partes de la serie, así como para desplazarse por el
    conjunto de datos. El valor por defecto es `FALSE`.

  - `hc.BoxInfo`: Si es `TRUE` (*valor predeterminado*) la información
    concerniente a cada punto se visualiza conjuntamente en un cuadro, o
    de forma individual (`FALSE`) al pasar el cursor sobre él.

  - `hc.Credits`: Cadena de caracteres indicando un subtítulo o etiqueta
    de créditos debajo del título principal.

  - `ply.LegendPosition`: Lista que especifica la posición y orientación
    de la leyenda. Los valores por defecto la ubican centrada
    verticalmente a la derecha del plot, es decir,
    `c(x = 1, y = 0.5, orientation = "v")`.

  - `ply.Interaction`: Cadena de caracteres que determina el modo de las
    interacciones de desplazamiento. Los valores permitidos son
    `"x unified"` (*valor predeterminado*), `"y unified"`, `"closest"`,
    `"x"`, `"y"` y `FALSE`.

  - `ply.Credits`: Lista que especifica la posición y texto para añadir
    un subtítulo o etiqueta de créditos a la serie principal, por
    ejemplo, `c(x = 0.2, y = 1, text = "https://...")`.

  - `dyg.LegendWidth`: Número que indica el ancho (*en píxeles*) que
    ocupará la leyenda. El valor por defecto es `250`.

  - `dyg.Resaltar`: Si es `FALSE` (*valor predeterminado*) no se
    resaltará la serie en que se sitúa el cursor.

  - `gg.Tema`: Modifica el tema con el cual se creará la serie. Los
    posibles valores son un número entero entre \\\[1, 11\]\\ el cual
    hace referencia a diferentes temas disponibles para `ggplot2`
    (`theme_light`, `theme_bw`, `theme_classic`, `theme_linedraw`,
    `theme_gray`, `theme_hc`, `theme_pander`, `theme_gdocs`,
    `theme_fivethirtyeight`, `theme_economist` y `theme_solarized`
    respectivamente). El tema por defecto, al no ingresar valor alguno,
    es el construido por el departamento `theme_DNPE`.

  - `gg.Legend`: Lista que especifica la posición y orientación de la
    leyenda. Los valores por defecto la ubican verticalmente a la
    derecha del plot. Algunos valores aceptados para `legend.position`
    son `"none"`, `"left"`, `"top"`, `"right"`, `"bottom"` y
    `c(CoordX, CoordY)`. Para `legend.direction` solo se acepta
    `"vertical"` u `"horizontal"`.

  - `gg.Linea`: Una lista de parámetros admitidos por la función
    [geom_line()](https://ggplot2.tidyverse.org/reference/geom_path.html)).

  - `gg.Punto`: Una lista de parámetros admitidos por la función
    [geom_point()](https://ggplot2.tidyverse.org/reference/geom_point.html))

  - `gg.Texto`: Una lista cuyos valores admitidos y usados son
    `subtitle`, `caption` y `tag`.

  - `gg.Repel`: Una lista de parámetros admitidos por la función
    [geom_text_repel()](https://ggrepel.slowkow.com/reference/geom_text_repel.html))

- estatico:

  Si es `FALSE` (*valor predeterminado*) el gráfico a retornar será
  dinámico (*dependiendo de la librería seleccionada*), en caso
  contrario se retornará un gráfico estático construido con `ggplot2`.

## Valor

Retorna la serie (*objeto widget de HTML*) creada. La clase del objeto
retornado será un "htmlwidget" y dependiendo de la librería usada
pertenecerá adicionalmente a la clase "highchart", "plotly" o
"dygraphs".

## Detalles

Al usar el paquete `Highcharter` y usar las opciones de descarga, el
nombre del archivo descargado será la concatenación del plot graficado y
la categoría usada, así, por ejemplo, si se graficó la serie de tiempo
para la categoría "Sede" el nombre será `PlotSeries_Sede.png`.

Tenga en cuenta que la librería `"dygraphs"` solo la podrá usar si
dentro del argumento tiempo ingresa las dos variables (`YEAR`,
`SEMESTRE`) para asemejar su estructura a los agregados clásicos. En
caso contrario le arrojara un error.

Recuerde que puede usar más temas (*cualquiera de hecho*) de los que se
proporcionan para `ggplot2`. Por ejemplo, los de
[hrbrthemes](https://github.com/hrbrmstr/hrbrthemes) o
[ggtech](https://github.com/ricardo-bion/ggtech).

## Nota

A continuación, se consolida en una tabla amigable el listado, uso y
disposición de todas las opciones para el parámetro `estilo`,
dependiendo del tipo de gráfico (*dinámico o estático*) y la librería
usada (*en el caso de que sea dinámico*).

|               |           |               |               |                    |
|---------------|-----------|---------------|---------------|--------------------|
| **PARÁMETRO** | **VALOR** | **PARÁMETRO** | **VALOR**     | **PARÁMETRO**      |
|               | \*        |               | •             | gg.Tema            |
|               | \*        |               | •             | gg.Legend          |
|               | *TRUE*    |               | •             | gg.Linea           |
|               | \*        |               | •             | gg.Punto           |
|               | \*        |               | •             | gg.Texto           |
|               | \*        |               | •             | gg.Repel           |
|               | O         | ~             | *highcharter* | hc.Tema            |
| **estatico**  | O         | \_            | ¦             | hc.BoxInfo         |
|               | O         | \_            | ¦             | hc.Slider          |
|               | O         | \_            | ¦             | hc.Credits         |
|               | *FALSE*   | **libreria**  | *plotly*      | ply.LegendPosition |
|               | O         | \_            | °             | ply.Credits        |
|               | O         | \_            | °             | ply.Interaction    |
|               | O         | \_            | *dygraphs*    | dyg.LegendWidth    |
|               | O         | ~             | L             | dyg.Resaltar       |

## Lista de argumentos de estilo

Sabemos que puede ser abrumador el número de argumentos dentro del
parámetro `estilo`, pero es necesario si queremos ofrecer al usuario la
máxima personalización dentro de cada función usando cualquier librería.
Por tal razón, a continuación, se detalla el listado completo de
argumentos, usados al especificar la librería y en qué función están
presentes (*marcado con una × si lo posee*).

|  |  |  |  |  |  |  |  |  |  |
|----|----|----|----|----|----|----|----|----|----|
| **Librería** | **estilo\$** | `Plot.Series()` | [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md) | [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md) | [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md) | [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md) | [`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md) | [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md) | [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md) |
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
# library("tibble"); library("dplyr")
set.seed(42)
Blood <- tibble(
  Year    = rep(2000:2001, each = 100),
  Quarter = sample(c("I", "II", "III", "IV"), size = 200, replace = TRUE),
  Week    = sample(c("1rt", "2nd", "3rd"), size = 200, replace = TRUE),
  Group   = sample(
    c("O", "A", "B", "AB"), size = 200, prob = c(.5, .3, .16, .4), replace = TRUE
  ),
  RH      = sample(c("+", "-"), size = 200, replace = TRUE),
  Prevalence = round(runif(200)*100)
)
Plot.Series(
  datos     = Blood,
  tiempo    = vars(Year, Quarter, Week),
  valores   = Prevalence,
  categoria = RH,
  labelX    = ""
)
#> Warning: ¡Se usará la librería 'highcharter' por defecto para realizar el plot!
#> Warning: 
#>     ¡Ha ingresado un dataframe que no está de forma condensada, es decir,
#>     para cada categoría existe más de un valor para un mismo punto del eje X!
#>     Se sumará los valores por defectos para dichos puntos que gocen de +1 valor
#>            
#> Warning: `unite_()` was deprecated in tidyr 1.2.0.
#> ℹ Please use `unite()` instead.
#> ℹ The deprecated feature was likely used in the highcharter package.
#>   Please report the issue at <https://github.com/jbkunst/highcharter/issues>.

{"x":{"hc_opts":{"chart":{"reflow":true,"type":"datetime","zoomType":"x"},"title":{"text":"","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"","offset":70,"style":{"fontWeight":"bold","fontSize":"18px","color":"black"}},"type":"linear","reversed":false,"lineColor":"#787878","opposite":false,"lineWidth":1,"labels":{"format":"{value}","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"credits":{"enabled":false},"exporting":{"enabled":true,"filename":"PlotSeries_RH"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":true},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"line":{"marker":{"enabled":false,"symbol":"square","radius":1}}},"series":[{"name":"+","data":[{"Fecha":"2000-I-1rt","Clase":"+","Y":337,"Extra":58.8,"y":337,"name":"2000-I-1rt"},{"Fecha":"2000-I-2nd","Clase":"+","Y":162,"Extra":42.6,"y":162,"name":"2000-I-2nd"},{"Fecha":"2000-I-3rd","Clase":"+","Y":154,"Extra":35.2,"y":154,"name":"2000-I-3rd"},{"Fecha":"2000-II-1rt","Clase":"+","Y":107,"Extra":29.6,"y":107,"name":"2000-II-1rt"},{"Fecha":"2000-II-2nd","Clase":"+","Y":279,"Extra":47.3,"y":279,"name":"2000-II-2nd"},{"Fecha":"2000-II-3rd","Clase":"+","Y":239,"Extra":71.09999999999999,"y":239,"name":"2000-II-3rd"},{"Fecha":"2000-III-1rt","Clase":"+","Y":131,"Extra":40.9,"y":131,"name":"2000-III-1rt"},{"Fecha":"2000-III-2nd","Clase":"+","Y":178,"Extra":58.6,"y":178,"name":"2000-III-2nd"},{"Fecha":"2000-III-3rd","Clase":"+","Y":76,"Extra":21.5,"y":76,"name":"2000-III-3rd"},{"Fecha":"2000-IV-1rt","Clase":"+","Y":239,"Extra":39.9,"y":239,"name":"2000-IV-1rt"},{"Fecha":"2000-IV-2nd","Clase":"+","Y":164,"Extra":36.4,"y":164,"name":"2000-IV-2nd"},{"Fecha":"2000-IV-3rd","Clase":"+","Y":228,"Extra":62.8,"y":228,"name":"2000-IV-3rd"},{"Fecha":"2001-I-1rt","Clase":"+","Y":49,"Extra":14.6,"y":49,"name":"2001-I-1rt"},{"Fecha":"2001-I-2nd","Clase":"+","Y":300,"Extra":65.09999999999999,"y":300,"name":"2001-I-2nd"},{"Fecha":"2001-I-3rd","Clase":"+","Y":291,"Extra":67.7,"y":291,"name":"2001-I-3rd"},{"Fecha":"2001-II-1rt","Clase":"+","Y":151,"Extra":30.2,"y":151,"name":"2001-II-1rt"},{"Fecha":"2001-II-2nd","Clase":"+","Y":260,"Extra":58.8,"y":260,"name":"2001-II-2nd"},{"Fecha":"2001-II-3rd","Clase":"+","Y":131,"Extra":27.9,"y":131,"name":"2001-II-3rd"},{"Fecha":"2001-III-1rt","Clase":"+","Y":73,"Extra":27,"y":73,"name":"2001-III-1rt"},{"Fecha":"2001-III-2nd","Clase":"+","Y":271,"Extra":75.09999999999999,"y":271,"name":"2001-III-2nd"},{"Fecha":"2001-III-3rd","Clase":"+","Y":75,"Extra":26.3,"y":75,"name":"2001-III-3rd"},{"Fecha":"2001-IV-1rt","Clase":"+","Y":8,"Extra":8.800000000000001,"y":8,"name":"2001-IV-1rt"},{"Fecha":"2001-IV-2nd","Clase":"+","Y":434,"Extra":62.4,"y":434,"name":"2001-IV-2nd"},{"Fecha":"2001-IV-3rd","Clase":"+","Y":108,"Extra":52.9,"y":108,"name":"2001-IV-3rd"}],"type":"line","color":"#FF0000B3","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"-","data":[{"Fecha":"2000-I-1rt","Clase":"-","Y":236,"Extra":41.2,"y":236,"name":"2000-I-1rt"},{"Fecha":"2000-I-2nd","Clase":"-","Y":218,"Extra":57.4,"y":218,"name":"2000-I-2nd"},{"Fecha":"2000-I-3rd","Clase":"-","Y":284,"Extra":64.8,"y":284,"name":"2000-I-3rd"},{"Fecha":"2000-II-1rt","Clase":"-","Y":255,"Extra":70.40000000000001,"y":255,"name":"2000-II-1rt"},{"Fecha":"2000-II-2nd","Clase":"-","Y":311,"Extra":52.7,"y":311,"name":"2000-II-2nd"},{"Fecha":"2000-II-3rd","Clase":"-","Y":97,"Extra":28.9,"y":97,"name":"2000-II-3rd"},{"Fecha":"2000-III-1rt","Clase":"-","Y":189,"Extra":59.1,"y":189,"name":"2000-III-1rt"},{"Fecha":"2000-III-2nd","Clase":"-","Y":126,"Extra":41.4,"y":126,"name":"2000-III-2nd"},{"Fecha":"2000-III-3rd","Clase":"-","Y":278,"Extra":78.5,"y":278,"name":"2000-III-3rd"},{"Fecha":"2000-IV-1rt","Clase":"-","Y":360,"Extra":60.1,"y":360,"name":"2000-IV-1rt"},{"Fecha":"2000-IV-2nd","Clase":"-","Y":286,"Extra":63.6,"y":286,"name":"2000-IV-2nd"},{"Fecha":"2000-IV-3rd","Clase":"-","Y":135,"Extra":37.2,"y":135,"name":"2000-IV-3rd"},{"Fecha":"2001-I-1rt","Clase":"-","Y":286,"Extra":85.40000000000001,"y":286,"name":"2001-I-1rt"},{"Fecha":"2001-I-2nd","Clase":"-","Y":161,"Extra":34.9,"y":161,"name":"2001-I-2nd"},{"Fecha":"2001-I-3rd","Clase":"-","Y":139,"Extra":32.3,"y":139,"name":"2001-I-3rd"},{"Fecha":"2001-II-1rt","Clase":"-","Y":349,"Extra":69.8,"y":349,"name":"2001-II-1rt"},{"Fecha":"2001-II-2nd","Clase":"-","Y":182,"Extra":41.2,"y":182,"name":"2001-II-2nd"},{"Fecha":"2001-II-3rd","Clase":"-","Y":338,"Extra":72.09999999999999,"y":338,"name":"2001-II-3rd"},{"Fecha":"2001-III-1rt","Clase":"-","Y":197,"Extra":73,"y":197,"name":"2001-III-1rt"},{"Fecha":"2001-III-2nd","Clase":"-","Y":90,"Extra":24.9,"y":90,"name":"2001-III-2nd"},{"Fecha":"2001-III-3rd","Clase":"-","Y":210,"Extra":73.7,"y":210,"name":"2001-III-3rd"},{"Fecha":"2001-IV-1rt","Clase":"-","Y":83,"Extra":91.2,"y":83,"name":"2001-IV-1rt"},{"Fecha":"2001-IV-2nd","Clase":"-","Y":262,"Extra":37.6,"y":262,"name":"2001-IV-2nd"},{"Fecha":"2001-IV-3rd","Clase":"-","Y":96,"Extra":47.1,"y":96,"name":"2001-IV-3rd"}],"type":"line","color":"#00FFFFB3","zoomType":{"enabled":false},"resetZoomButton":true}],"xAxis":{"type":"category","title":{"text":"","offset":70,"style":{"fontWeight":"bold","fontSize":"18px","color":"black"}},"categories":["2000-I-1rt","2000-I-2nd","2000-I-3rd","2000-II-1rt","2000-II-2nd","2000-II-3rd","2000-III-1rt","2000-III-2nd","2000-III-3rd","2000-IV-1rt","2000-IV-2nd","2000-IV-3rd","2001-I-1rt","2001-I-2nd","2001-I-3rd","2001-II-1rt","2001-II-2nd","2001-II-3rd","2001-III-1rt","2001-III-2nd","2001-III-3rd","2001-IV-1rt","2001-IV-2nd","2001-IV-3rd"],"align":"center","lineColor":"#787878","opposite":false,"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","layout":"horizontal","title":{"text":"","style":{"textDecoration":"underline"}},"x":42,"y":0,"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"tooltip":{"crosshairs":true,"shared":true,"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}<\/b> ({point.Extra}%)<br/>","backgroundColor":"rgba(186,174,174,0.7)","borderColor":"#6D6666","borderWidth":5,"useHTML":true}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}Plot.Series(
  datos     = Blood,
  tiempo    = vars(Year, Quarter),
  valores   = Prevalence,
  categoria = Group,
  libreria  = "plotly"
)
#> Warning: 
#>     ¡Ha ingresado un dataframe que no está de forma condensada, es decir,
#>     para cada categoría existe más de un valor para un mismo punto del eje X!
#>     Se sumará los valores por defectos para dichos puntos que gocen de +1 valor
#>            

{"x":{"visdat":{"6a7821bc5624":["function () ","plotlyVisDat"]},"cur_data":"6a7821bc5624","attrs":{"6a7821bc5624":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[361,168,86,415,306,263,97,234],"text":[26,13,8.8000000000000007,29.399999999999999,25,18.600000000000001,10.6,23.600000000000001],"name":"A","type":"scatter","mode":"markers+lines","line":{"color":"#FF0000B3","width":3},"marker":{"color":"#FF0000B3","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7821bc5624.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[336,327,322,196,253,542,407,176],"text":[24.199999999999999,25.399999999999999,32.899999999999999,13.9,20.600000000000001,38.399999999999999,44.399999999999999,17.800000000000001],"name":"AB","type":"scatter","mode":"markers+lines","line":{"color":"#80FF00B3","width":3},"marker":{"color":"#80FF00B3","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7821bc5624.2":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[256,164,98,165,222,171,89,103],"text":[18.399999999999999,12.699999999999999,10,11.699999999999999,18.100000000000001,12.1,9.6999999999999993,10.4],"name":"B","type":"scatter","mode":"markers+lines","line":{"color":"#00FFFFB3","width":3},"marker":{"color":"#00FFFFB3","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7821bc5624.3":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[438,629,472,636,445,435,323,478],"text":[31.5,48.799999999999997,48.299999999999997,45,36.299999999999997,30.800000000000001,35.299999999999997,48.200000000000003],"name":"O","type":"scatter","mode":"markers+lines","line":{"color":"#8000FFB3","width":3},"marker":{"color":"#8000FFB3","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b><\/b>","font":{"family":"Open Sans","size":24,"color":"#333333"},"y":0.95999999999999996},"xaxis":{"domain":[0,1],"automargin":true,"title":"Periodo","zeroline":false,"showline":true,"showgrid":false,"showticklabels":true,"linecolor":"#787878","linewidth":2.5,"autotick":false,"ticks":"outside","tickwidth":2.5,"ticklen":10,"tickcolor":"#CCCCCC","tickangle":-45,"tickfont":{"family":"Old Standard TT, serif","size":16,"color":"#525252"},"type":"category","categoryorder":"array","categoryarray":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"","ticksuffix":"","zeroline":true,"autorange":[],"range":[],"showline":true,"showgrid":true,"showticklabels":true,"linecolor":"#787878","linewidth":3,"tickfont":{"family":"Old Standard TT, serif","size":16,"color":"#525252"},"separatethousands":true},"autosize":true,"showlegend":true,"legend":{"x":1,"y":0.5,"orientation":"v","traceorder":"normal","title":{"text":"<b><\/b>"}},"hovermode":"x unified","annotations":[{"x":0.20000000000000001,"y":1,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}},{"x":0.20000000000000001,"y":1,"text":"","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}}]},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[361,168,86,415,306,263,97,234],"text":[26,13,8.8000000000000007,29.399999999999999,25,18.600000000000001,10.6,23.600000000000001],"name":"A","type":"scatter","mode":"markers+lines","line":{"color":"#FF0000B3","width":3},"marker":{"color":"#FF0000B3","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[336,327,322,196,253,542,407,176],"text":[24.199999999999999,25.399999999999999,32.899999999999999,13.9,20.600000000000001,38.399999999999999,44.399999999999999,17.800000000000001],"name":"AB","type":"scatter","mode":"markers+lines","line":{"color":"#80FF00B3","width":3},"marker":{"color":"#80FF00B3","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[256,164,98,165,222,171,89,103],"text":[18.399999999999999,12.699999999999999,10,11.699999999999999,18.100000000000001,12.1,9.6999999999999993,10.4],"name":"B","type":"scatter","mode":"markers+lines","line":{"color":"#00FFFFB3","width":3},"marker":{"color":"#00FFFFB3","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2000-I","2000-II","2000-III","2000-IV","2001-I","2001-II","2001-III","2001-IV"],"y":[438,629,472,636,445,435,323,478],"text":[31.5,48.799999999999997,48.299999999999997,45,36.299999999999997,30.800000000000001,35.299999999999997,48.200000000000003],"name":"O","type":"scatter","mode":"markers+lines","line":{"color":"#8000FFB3","width":3},"marker":{"color":"#8000FFB3","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(214,39,40,1)"},"error_x":{"color":"rgba(214,39,40,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
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
Msj <- "Distribuci\u00f3n de estudiantes graduados (desde el 2009-I al 2021-I) por sede."
Txt <- "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEDE"
Plot.Series(
  datos        = ejConsolidadoGrad,
  categoria    = "SEDE_NOMBRE_ADM",
  freqRelativa = TRUE,
  ylim         = c(0, 75),
  colores      = misColores,
  titulo       = Txt,
  labelY       = "Frecuencia Relativa<br>(% de graduados)",
  libreria     = "highcharter",
  estilo       = list(LegendTitle = "SEDE:", hc.Tema = 10, hc.Slider = TRUE, hc.Credits = Msj)
)

{"x":{"hc_opts":{"chart":{"reflow":true,"type":"datetime","zoomType":"x"},"title":{"text":"EVOLUCIÓN DEL NÚMERO DE GRADUADOS POR SEDE","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Frecuencia Relativa<br>(% de graduados)","offset":70,"style":{"fontWeight":"bold","fontSize":"18px","color":"black"}},"type":"linear","reversed":false,"lineColor":"#787878","opposite":false,"lineWidth":1,"min":0,"max":75,"labels":{"format":"{value}%","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"credits":{"enabled":false},"exporting":{"enabled":true,"filename":"PlotSeries_Clase"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":true},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"line":{"marker":{"enabled":false,"symbol":"square","radius":1}}},"series":[{"name":"Amazonía","data":[{"Fecha":"2009-1","Clase":"Amazonía","Extra":1,"Y":0,"y":0,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Amazonía","Extra":5,"Y":0.2,"y":0.2,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Amazonía","Extra":7,"Y":0.1,"y":0.1,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Amazonía","Extra":8,"Y":0.2,"y":0.2,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Amazonía","Extra":10,"Y":0.2,"y":0.2,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Amazonía","Extra":12,"Y":0.4,"y":0.4,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Amazonía","Extra":3,"Y":0.1,"y":0.1,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Amazonía","Extra":3,"Y":0.1,"y":0.1,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Amazonía","Extra":4,"Y":0.1,"y":0.1,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Amazonía","Extra":11,"Y":0.3,"y":0.3,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Amazonía","Extra":5,"Y":0.1,"y":0.1,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Amazonía","Extra":4,"Y":0.1,"y":0.1,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Amazonía","Extra":2,"Y":0,"y":0,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Amazonía","Extra":13,"Y":0.3,"y":0.3,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Amazonía","Extra":2,"Y":0,"y":0,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Amazonía","Extra":15,"Y":0.3,"y":0.3,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Amazonía","Extra":16,"Y":0.3,"y":0.3,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Amazonía","Extra":20,"Y":0.4,"y":0.4,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Amazonía","Extra":21,"Y":0.4,"y":0.4,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Amazonía","Extra":17,"Y":0.3,"y":0.3,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Amazonía","Extra":9,"Y":0.3,"y":0.3,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Amazonía","Extra":42,"Y":0.6,"y":0.6,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Amazonía","Extra":41,"Y":0.8,"y":0.8,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Amazonía","Extra":18,"Y":0.5,"y":0.5,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Amazonía","Extra":29,"Y":0.7,"y":0.7,"name":"2021-1"}],"type":"line","color":"#29ABE2","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Bogotá","data":[{"Fecha":"2009-1","Clase":"Bogotá","Extra":3323,"Y":67.09999999999999,"y":67.09999999999999,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Bogotá","Extra":2042,"Y":65.7,"y":65.7,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Bogotá","Extra":3441,"Y":65.3,"y":65.3,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Bogotá","Extra":1924,"Y":57.2,"y":57.2,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Bogotá","Extra":2850,"Y":65.5,"y":65.5,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Bogotá","Extra":2382,"Y":71.8,"y":71.8,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Bogotá","Extra":1931,"Y":59,"y":59,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Bogotá","Extra":3158,"Y":65,"y":65,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Bogotá","Extra":1780,"Y":56,"y":56,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Bogotá","Extra":2764,"Y":62.8,"y":62.8,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Bogotá","Extra":2622,"Y":62.7,"y":62.7,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Bogotá","Extra":2716,"Y":62.4,"y":62.4,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Bogotá","Extra":2544,"Y":59.6,"y":59.6,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Bogotá","Extra":2824,"Y":61,"y":61,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Bogotá","Extra":2769,"Y":58.8,"y":58.8,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Bogotá","Extra":2822,"Y":60,"y":60,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Bogotá","Extra":3264,"Y":61.8,"y":61.8,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Bogotá","Extra":2755,"Y":57.4,"y":57.4,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Bogotá","Extra":3153,"Y":61.5,"y":61.5,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Bogotá","Extra":2867,"Y":56.2,"y":56.2,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Bogotá","Extra":1749,"Y":65,"y":65,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Bogotá","Extra":4200,"Y":60,"y":60,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Bogotá","Extra":3016,"Y":61.2,"y":61.2,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Bogotá","Extra":2449,"Y":64.2,"y":64.2,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Bogotá","Extra":2575,"Y":58.5,"y":58.5,"name":"2021-1"}],"type":"line","color":"#8CC63F","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Caribe","data":[{"Fecha":"2009-1","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Caribe","Extra":11,"Y":0.3,"y":0.3,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Caribe","Extra":1,"Y":0,"y":0,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Caribe","Extra":1,"Y":0,"y":0,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Caribe","Extra":5,"Y":0.1,"y":0.1,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Caribe","Extra":0,"Y":0,"y":0,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Caribe","Extra":4,"Y":0.1,"y":0.1,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Caribe","Extra":5,"Y":0.1,"y":0.1,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Caribe","Extra":6,"Y":0.1,"y":0.1,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Caribe","Extra":5,"Y":0.1,"y":0.1,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Caribe","Extra":12,"Y":0.3,"y":0.3,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Caribe","Extra":11,"Y":0.2,"y":0.2,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Caribe","Extra":16,"Y":0.3,"y":0.3,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Caribe","Extra":4,"Y":0.1,"y":0.1,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Caribe","Extra":15,"Y":0.3,"y":0.3,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Caribe","Extra":5,"Y":0.2,"y":0.2,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Caribe","Extra":24,"Y":0.3,"y":0.3,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Caribe","Extra":8,"Y":0.2,"y":0.2,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Caribe","Extra":7,"Y":0.2,"y":0.2,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Caribe","Extra":11,"Y":0.2,"y":0.2,"name":"2021-1"}],"type":"line","color":"#CC241D","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Manizales","data":[{"Fecha":"2009-1","Clase":"Manizales","Extra":468,"Y":9.4,"y":9.4,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Manizales","Extra":447,"Y":14.4,"y":14.4,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Manizales","Extra":485,"Y":9.199999999999999,"y":9.199999999999999,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Manizales","Extra":452,"Y":13.4,"y":13.4,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Manizales","Extra":427,"Y":9.800000000000001,"y":9.800000000000001,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Manizales","Extra":460,"Y":13.9,"y":13.9,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Manizales","Extra":399,"Y":12.2,"y":12.2,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Manizales","Extra":463,"Y":9.5,"y":9.5,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Manizales","Extra":395,"Y":12.4,"y":12.4,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Manizales","Extra":498,"Y":11.3,"y":11.3,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Manizales","Extra":407,"Y":9.699999999999999,"y":9.699999999999999,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Manizales","Extra":495,"Y":11.4,"y":11.4,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Manizales","Extra":484,"Y":11.3,"y":11.3,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Manizales","Extra":544,"Y":11.7,"y":11.7,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Manizales","Extra":582,"Y":12.4,"y":12.4,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Manizales","Extra":572,"Y":12.2,"y":12.2,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Manizales","Extra":641,"Y":12.1,"y":12.1,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Manizales","Extra":647,"Y":13.5,"y":13.5,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Manizales","Extra":613,"Y":12,"y":12,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Manizales","Extra":641,"Y":12.6,"y":12.6,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Manizales","Extra":269,"Y":10,"y":10,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Manizales","Extra":816,"Y":11.7,"y":11.7,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Manizales","Extra":606,"Y":12.3,"y":12.3,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Manizales","Extra":611,"Y":16,"y":16,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Manizales","Extra":613,"Y":13.9,"y":13.9,"name":"2021-1"}],"type":"line","color":"#0071BC","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Medellín","data":[{"Fecha":"2009-1","Clase":"Medellín","Extra":981,"Y":19.8,"y":19.8,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Medellín","Extra":412,"Y":13.3,"y":13.3,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Medellín","Extra":1153,"Y":21.9,"y":21.9,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Medellín","Extra":814,"Y":24.2,"y":24.2,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Medellín","Extra":867,"Y":19.9,"y":19.9,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Medellín","Extra":365,"Y":11,"y":11,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Medellín","Extra":839,"Y":25.6,"y":25.6,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Medellín","Extra":1065,"Y":21.9,"y":21.9,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Medellín","Extra":839,"Y":26.4,"y":26.4,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Medellín","Extra":925,"Y":21,"y":21,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Medellín","Extra":990,"Y":23.7,"y":23.7,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Medellín","Extra":931,"Y":21.4,"y":21.4,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Medellín","Extra":982,"Y":23,"y":23,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Medellín","Extra":1022,"Y":22.1,"y":22.1,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Medellín","Extra":1107,"Y":23.5,"y":23.5,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Medellín","Extra":1026,"Y":21.8,"y":21.8,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Medellín","Extra":1098,"Y":20.8,"y":20.8,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Medellín","Extra":1068,"Y":22.3,"y":22.3,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Medellín","Extra":1015,"Y":19.8,"y":19.8,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Medellín","Extra":1092,"Y":21.4,"y":21.4,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Medellín","Extra":486,"Y":18.1,"y":18.1,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Medellín","Extra":1562,"Y":22.3,"y":22.3,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Medellín","Extra":1053,"Y":21.4,"y":21.4,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Medellín","Extra":506,"Y":13.3,"y":13.3,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Medellín","Extra":907,"Y":20.6,"y":20.6,"name":"2021-1"}],"type":"line","color":"#F15A24","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Orinoquía","data":[{"Fecha":"2009-1","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Orinoquía","Extra":0,"Y":0,"y":0,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Orinoquía","Extra":4,"Y":0.1,"y":0.1,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Orinoquía","Extra":6,"Y":0.1,"y":0.1,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Orinoquía","Extra":15,"Y":0.4,"y":0.4,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Orinoquía","Extra":14,"Y":0.3,"y":0.3,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Orinoquía","Extra":15,"Y":0.3,"y":0.3,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Orinoquía","Extra":20,"Y":0.4,"y":0.4,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Orinoquía","Extra":19,"Y":0.4,"y":0.4,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Orinoquía","Extra":22,"Y":0.5,"y":0.5,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Orinoquía","Extra":28,"Y":0.5,"y":0.5,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Orinoquía","Extra":29,"Y":0.6,"y":0.6,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Orinoquía","Extra":8,"Y":0.3,"y":0.3,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Orinoquía","Extra":56,"Y":0.8,"y":0.8,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Orinoquía","Extra":29,"Y":0.6,"y":0.6,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Orinoquía","Extra":25,"Y":0.7,"y":0.7,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Orinoquía","Extra":29,"Y":0.7,"y":0.7,"name":"2021-1"}],"type":"line","color":"#FBB03B","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Palmira","data":[{"Fecha":"2009-1","Clase":"Palmira","Extra":183,"Y":3.7,"y":3.7,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Palmira","Extra":202,"Y":6.5,"y":6.5,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Palmira","Extra":187,"Y":3.5,"y":3.5,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Palmira","Extra":164,"Y":4.9,"y":4.9,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Palmira","Extra":184,"Y":4.2,"y":4.2,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Palmira","Extra":97,"Y":2.9,"y":2.9,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Palmira","Extra":101,"Y":3.1,"y":3.1,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Palmira","Extra":171,"Y":3.5,"y":3.5,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Palmira","Extra":161,"Y":5.1,"y":5.1,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Palmira","Extra":196,"Y":4.5,"y":4.5,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Palmira","Extra":157,"Y":3.8,"y":3.8,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Palmira","Extra":196,"Y":4.5,"y":4.5,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Palmira","Extra":236,"Y":5.5,"y":5.5,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Palmira","Extra":208,"Y":4.5,"y":4.5,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Palmira","Extra":226,"Y":4.8,"y":4.8,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Palmira","Extra":235,"Y":5,"y":5,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Palmira","Extra":233,"Y":4.4,"y":4.4,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Palmira","Extra":271,"Y":5.6,"y":5.6,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Palmira","Extra":290,"Y":5.7,"y":5.7,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Palmira","Extra":441,"Y":8.6,"y":8.6,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Palmira","Extra":164,"Y":6.1,"y":6.1,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Palmira","Extra":296,"Y":4.2,"y":4.2,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Palmira","Extra":173,"Y":3.5,"y":3.5,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Palmira","Extra":197,"Y":5.2,"y":5.2,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Palmira","Extra":235,"Y":5.3,"y":5.3,"name":"2021-1"}],"type":"line","color":"#93278F","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Tumaco","data":[{"Fecha":"2009-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2009-1"},{"Fecha":"2009-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2009-2"},{"Fecha":"2010-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2010-1"},{"Fecha":"2010-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2010-2"},{"Fecha":"2011-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2011-1"},{"Fecha":"2011-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2011-2"},{"Fecha":"2012-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2012-1"},{"Fecha":"2012-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2012-2"},{"Fecha":"2013-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2013-1"},{"Fecha":"2013-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2013-2"},{"Fecha":"2014-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2014-1"},{"Fecha":"2014-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2014-2"},{"Fecha":"2015-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2015-1"},{"Fecha":"2015-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2015-2"},{"Fecha":"2016-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2016-1"},{"Fecha":"2016-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2016-2"},{"Fecha":"2017-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2017-1"},{"Fecha":"2017-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2017-2"},{"Fecha":"2018-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2018-1"},{"Fecha":"2018-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2018-2"},{"Fecha":"2019-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2019-1"},{"Fecha":"2019-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2019-2"},{"Fecha":"2020-1","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2020-1"},{"Fecha":"2020-2","Clase":"Tumaco","Extra":0,"Y":0,"y":0,"name":"2020-2"},{"Fecha":"2021-1","Clase":"Tumaco","Extra":1,"Y":0,"y":0,"name":"2021-1"}],"type":"line","color":"#8A381A","zoomType":{"enabled":false},"resetZoomButton":true}],"xAxis":{"type":"category","title":{"text":"Periodo","offset":70,"style":{"fontWeight":"bold","fontSize":"18px","color":"black"}},"categories":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"align":"center","lineColor":"#787878","opposite":false,"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","layout":"horizontal","title":{"text":"SEDE:","style":{"textDecoration":"underline"}},"x":42,"y":0,"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"tooltip":{"crosshairs":true,"shared":true,"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}%<\/b> ({point.Extra})<br/>","backgroundColor":"rgba(186,174,174,0.7)","borderColor":"#6D6666","borderWidth":5,"useHTML":true},"navigator":{"height":15,"margin":5,"maskFill":"rgba(255,16,46,0.6)","enabled":true,"series":{"color":"#999999","lineWidth":30,"type":"areaspline","fillColor":"#999999"}},"rangeSelector":{"enabled":true,"inputEnabled":false,"labelStyle":{"display":"none"},"buttonPosition":{"align":"left"},"floating":false,"buttons":[{"type":"all","text":"Restaurar"}]},"subtitle":{"text":"Distribución de estudiantes graduados (desde el 2009-I al 2021-I) por sede.","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#34495e"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#46627f","lineColor":"#46627f","minorGridLineColor":"#BDC3C7","tickColor":"#46627f","tickWidth":1,"title":{"style":{"color":"#FFFFFF"}}},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#46627f","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#46627f","tickWidth":1,"title":{"style":{"color":"#FFFFFF"}}},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)","title":{"style":{"color":"#FFFFFF"}},"subtitle":{"style":{"color":"#666666"}},"legend":{"itemStyle":{"color":"#C0C0C0"},"itemHoverStyle":{"color":"#C0C0C0"},"itemHiddenStyle":{"color":"#444444"}}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Plot.Series(
  datos     = ejConsolidadoGrad,
  categoria = "SEDE_NOMBRE_ADM",
  invertir  = TRUE,
  colores   = misColores,
  titulo    = Txt,
  labelY    = "N\u00famero de Graduados",
  libreria  = "plotly",
  estilo    = list(
    LegendTitle = "SEDE:", ply.Interaction = "closest",
    ply.LegendPosition = list(x = 0.16, y = -0.25, orientation = "h"),
    ply.Credits = list(x = 0.5, y = 0.1, text = Msj)
  )
)

{"x":{"visdat":{"6a7828281855":["function () ","plotlyVisDat"]},"cur_data":"6a7828281855","attrs":{"6a7828281855":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[1,5,7,8,10,12,3,3,4,11,5,4,2,13,2,15,16,20,21,17,9,42,41,18,29],"text":[0,0.20000000000000001,0.10000000000000001,0.20000000000000001,0.20000000000000001,0.40000000000000002,0.10000000000000001,0.10000000000000001,0.10000000000000001,0.29999999999999999,0.10000000000000001,0.10000000000000001,0,0.29999999999999999,0,0.29999999999999999,0.29999999999999999,0.40000000000000002,0.40000000000000002,0.29999999999999999,0.29999999999999999,0.59999999999999998,0.80000000000000004,0.5,0.69999999999999996],"name":"Amazonía","type":"scatter","mode":"markers+lines","line":{"color":"#29ABE2","width":3},"marker":{"color":"#29ABE2","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[3323,2042,3441,1924,2850,2382,1931,3158,1780,2764,2622,2716,2544,2824,2769,2822,3264,2755,3153,2867,1749,4200,3016,2449,2575],"text":[67.099999999999994,65.700000000000003,65.299999999999997,57.200000000000003,65.5,71.799999999999997,59,65,56,62.799999999999997,62.700000000000003,62.399999999999999,59.600000000000001,61,58.799999999999997,60,61.799999999999997,57.399999999999999,61.5,56.200000000000003,65,60,61.200000000000003,64.200000000000003,58.5],"name":"Bogotá","type":"scatter","mode":"markers+lines","line":{"color":"#8CC63F","width":3},"marker":{"color":"#8CC63F","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.2":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[0,0,0,0,11,1,0,1,0,5,0,4,5,6,5,12,11,16,4,15,5,24,8,7,11],"text":[0,0,0,0,0.29999999999999999,0,0,0,0,0.10000000000000001,0,0.10000000000000001,0.10000000000000001,0.10000000000000001,0.10000000000000001,0.29999999999999999,0.20000000000000001,0.29999999999999999,0.10000000000000001,0.29999999999999999,0.20000000000000001,0.29999999999999999,0.20000000000000001,0.20000000000000001,0.20000000000000001],"name":"Caribe","type":"scatter","mode":"markers+lines","line":{"color":"#CC241D","width":3},"marker":{"color":"#CC241D","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.3":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[468,447,485,452,427,460,399,463,395,498,407,495,484,544,582,572,641,647,613,641,269,816,606,611,613],"text":[9.4000000000000004,14.4,9.1999999999999993,13.4,9.8000000000000007,13.9,12.199999999999999,9.5,12.4,11.300000000000001,9.6999999999999993,11.4,11.300000000000001,11.699999999999999,12.4,12.199999999999999,12.1,13.5,12,12.6,10,11.699999999999999,12.300000000000001,16,13.9],"name":"Manizales","type":"scatter","mode":"markers+lines","line":{"color":"#0071BC","width":3},"marker":{"color":"#0071BC","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.4":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[981,412,1153,814,867,365,839,1065,839,925,990,931,982,1022,1107,1026,1098,1068,1015,1092,486,1562,1053,506,907],"text":[19.800000000000001,13.300000000000001,21.899999999999999,24.199999999999999,19.899999999999999,11,25.600000000000001,21.899999999999999,26.399999999999999,21,23.699999999999999,21.399999999999999,23,22.100000000000001,23.5,21.800000000000001,20.800000000000001,22.300000000000001,19.800000000000001,21.399999999999999,18.100000000000001,22.300000000000001,21.399999999999999,13.300000000000001,20.600000000000001],"name":"Medellín","type":"scatter","mode":"markers+lines","line":{"color":"#F15A24","width":3},"marker":{"color":"#F15A24","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.5":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[0,0,0,0,0,0,0,0,0,0,4,6,15,14,15,20,19,22,28,29,8,56,29,25,29],"text":[0,0,0,0,0,0,0,0,0,0,0.10000000000000001,0.10000000000000001,0.40000000000000002,0.29999999999999999,0.29999999999999999,0.40000000000000002,0.40000000000000002,0.5,0.5,0.59999999999999998,0.29999999999999999,0.80000000000000004,0.59999999999999998,0.69999999999999996,0.69999999999999996],"name":"Orinoquía","type":"scatter","mode":"markers+lines","line":{"color":"#FBB03B","width":3},"marker":{"color":"#FBB03B","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[183,202,187,164,184,97,101,171,161,196,157,196,236,208,226,235,233,271,290,441,164,296,173,197,235],"text":[3.7000000000000002,6.5,3.5,4.9000000000000004,4.2000000000000002,2.8999999999999999,3.1000000000000001,3.5,5.0999999999999996,4.5,3.7999999999999998,4.5,5.5,4.5,4.7999999999999998,5,4.4000000000000004,5.5999999999999996,5.7000000000000002,8.5999999999999996,6.0999999999999996,4.2000000000000002,3.5,5.2000000000000002,5.2999999999999998],"name":"Palmira","type":"scatter","mode":"markers+lines","line":{"color":"#93278F","width":3},"marker":{"color":"#93278F","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true},"6a7828281855.7":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],"text":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"name":"Tumaco","type":"scatter","mode":"markers+lines","line":{"color":"#8A381A","width":3},"marker":{"color":"#8A381A","size":6,"line":{"width":1.2,"color":"#787878"}},"hovertemplate":"%{y} (%{text:.2s}%)","textposition":"outside","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"<b>EVOLUCIÓN DEL NÚMERO DE GRADUADOS POR SEDE<\/b>","font":{"family":"Open Sans","size":24,"color":"#333333"},"y":0.95999999999999996},"xaxis":{"domain":[0,1],"automargin":true,"title":"Periodo","zeroline":false,"showline":true,"showgrid":false,"showticklabels":true,"linecolor":"#787878","linewidth":2.5,"autotick":false,"ticks":"outside","tickwidth":2.5,"ticklen":10,"tickcolor":"#CCCCCC","tickangle":-45,"tickfont":{"family":"Old Standard TT, serif","size":16,"color":"#525252"},"type":"category","categoryorder":"array","categoryarray":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"Número de Graduados","ticksuffix":"","zeroline":true,"autorange":"reversed","range":[],"showline":true,"showgrid":true,"showticklabels":true,"linecolor":"#787878","linewidth":3,"tickfont":{"family":"Old Standard TT, serif","size":16,"color":"#525252"},"separatethousands":true},"autosize":true,"showlegend":true,"legend":{"x":0.16,"y":-0.25,"orientation":"h","traceorder":"normal","title":{"text":"<b>SEDE:<\/b>"}},"hovermode":"closest","annotations":[{"x":0.5,"y":0.10000000000000001,"text":"Distribución de estudiantes graduados (desde el 2009-I al 2021-I) por sede.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}},{"x":0.5,"y":0.10000000000000001,"text":"Distribución de estudiantes graduados (desde el 2009-I al 2021-I) por sede.","showarrow":false,"xref":"paper","yref":"paper","xanchor":"right","yanchor":"auto","xshift":0,"yshift":0,"font":{"size":12,"color":"#CCCCCC"}}]},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"locale":"es"},"data":[{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[1,5,7,8,10,12,3,3,4,11,5,4,2,13,2,15,16,20,21,17,9,42,41,18,29],"text":[0,0.20000000000000001,0.10000000000000001,0.20000000000000001,0.20000000000000001,0.40000000000000002,0.10000000000000001,0.10000000000000001,0.10000000000000001,0.29999999999999999,0.10000000000000001,0.10000000000000001,0,0.29999999999999999,0,0.29999999999999999,0.29999999999999999,0.40000000000000002,0.40000000000000002,0.29999999999999999,0.29999999999999999,0.59999999999999998,0.80000000000000004,0.5,0.69999999999999996],"name":"Amazonía","type":"scatter","mode":"markers+lines","line":{"color":"#29ABE2","width":3},"marker":{"color":"#29ABE2","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[3323,2042,3441,1924,2850,2382,1931,3158,1780,2764,2622,2716,2544,2824,2769,2822,3264,2755,3153,2867,1749,4200,3016,2449,2575],"text":[67.099999999999994,65.700000000000003,65.299999999999997,57.200000000000003,65.5,71.799999999999997,59,65,56,62.799999999999997,62.700000000000003,62.399999999999999,59.600000000000001,61,58.799999999999997,60,61.799999999999997,57.399999999999999,61.5,56.200000000000003,65,60,61.200000000000003,64.200000000000003,58.5],"name":"Bogotá","type":"scatter","mode":"markers+lines","line":{"color":"#8CC63F","width":3},"marker":{"color":"#8CC63F","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[0,0,0,0,11,1,0,1,0,5,0,4,5,6,5,12,11,16,4,15,5,24,8,7,11],"text":[0,0,0,0,0.29999999999999999,0,0,0,0,0.10000000000000001,0,0.10000000000000001,0.10000000000000001,0.10000000000000001,0.10000000000000001,0.29999999999999999,0.20000000000000001,0.29999999999999999,0.10000000000000001,0.29999999999999999,0.20000000000000001,0.29999999999999999,0.20000000000000001,0.20000000000000001,0.20000000000000001],"name":"Caribe","type":"scatter","mode":"markers+lines","line":{"color":"#CC241D","width":3},"marker":{"color":"#CC241D","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[468,447,485,452,427,460,399,463,395,498,407,495,484,544,582,572,641,647,613,641,269,816,606,611,613],"text":[9.4000000000000004,14.4,9.1999999999999993,13.4,9.8000000000000007,13.9,12.199999999999999,9.5,12.4,11.300000000000001,9.6999999999999993,11.4,11.300000000000001,11.699999999999999,12.4,12.199999999999999,12.1,13.5,12,12.6,10,11.699999999999999,12.300000000000001,16,13.9],"name":"Manizales","type":"scatter","mode":"markers+lines","line":{"color":"#0071BC","width":3},"marker":{"color":"#0071BC","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(214,39,40,1)"},"error_x":{"color":"rgba(214,39,40,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[981,412,1153,814,867,365,839,1065,839,925,990,931,982,1022,1107,1026,1098,1068,1015,1092,486,1562,1053,506,907],"text":[19.800000000000001,13.300000000000001,21.899999999999999,24.199999999999999,19.899999999999999,11,25.600000000000001,21.899999999999999,26.399999999999999,21,23.699999999999999,21.399999999999999,23,22.100000000000001,23.5,21.800000000000001,20.800000000000001,22.300000000000001,19.800000000000001,21.399999999999999,18.100000000000001,22.300000000000001,21.399999999999999,13.300000000000001,20.600000000000001],"name":"Medellín","type":"scatter","mode":"markers+lines","line":{"color":"#F15A24","width":3},"marker":{"color":"#F15A24","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(148,103,189,1)"},"error_x":{"color":"rgba(148,103,189,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[0,0,0,0,0,0,0,0,0,0,4,6,15,14,15,20,19,22,28,29,8,56,29,25,29],"text":[0,0,0,0,0,0,0,0,0,0,0.10000000000000001,0.10000000000000001,0.40000000000000002,0.29999999999999999,0.29999999999999999,0.40000000000000002,0.40000000000000002,0.5,0.5,0.59999999999999998,0.29999999999999999,0.80000000000000004,0.59999999999999998,0.69999999999999996,0.69999999999999996],"name":"Orinoquía","type":"scatter","mode":"markers+lines","line":{"color":"#FBB03B","width":3},"marker":{"color":"#FBB03B","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(140,86,75,1)"},"error_x":{"color":"rgba(140,86,75,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[183,202,187,164,184,97,101,171,161,196,157,196,236,208,226,235,233,271,290,441,164,296,173,197,235],"text":[3.7000000000000002,6.5,3.5,4.9000000000000004,4.2000000000000002,2.8999999999999999,3.1000000000000001,3.5,5.0999999999999996,4.5,3.7999999999999998,4.5,5.5,4.5,4.7999999999999998,5,4.4000000000000004,5.5999999999999996,5.7000000000000002,8.5999999999999996,6.0999999999999996,4.2000000000000002,3.5,5.2000000000000002,5.2999999999999998],"name":"Palmira","type":"scatter","mode":"markers+lines","line":{"color":"#93278F","width":3},"marker":{"color":"#93278F","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(227,119,194,1)"},"error_x":{"color":"rgba(227,119,194,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2009-1","2009-2","2010-1","2010-2","2011-1","2011-2","2012-1","2012-2","2013-1","2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2","2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1","2020-2","2021-1"],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],"text":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"name":"Tumaco","type":"scatter","mode":"markers+lines","line":{"color":"#8A381A","width":3},"marker":{"color":"#8A381A","size":6,"line":{"color":"#787878","width":1.2}},"hovertemplate":["%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)","%{y} (%{text:.2s}%)"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"error_y":{"color":"rgba(127,127,127,1)"},"error_x":{"color":"rgba(127,127,127,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
Plot.Series(
  datos     = ejConsolidadoGrad,
  categoria = "SEDE_NOMBRE_ADM",
  colores   = misColores,
  titulo    = Txt,
  labelY    = "N\u00famero de Graduados (k: miles)",
  libreria  = "dygraphs",
  estilo    = list(dyg.LegendWidth = 650, dyg.Resaltar = TRUE)
)

{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true,"axisLineWidth":4,"axisLabelFormatter":"function(d) {\n                      var monthNames = [\"I\", \"\", \"\", \"\", \"\", \"\",\"II\", \"\", \"\", \"\", \"\", \"\"];\n                      date = new Date(d);\n                      if (date.getMonth() == 0 || date.getMonth() == 6) {\n                        return date.getFullYear() + \"-\" + monthNames[date.getMonth()];\n                      } else {\n                        return \"\";\n                      }\n                   }"},"y":{"drawAxis":true,"axisLineWidth":4}},"colors":["#29ABE2","#8CC63F","#CC241D","#0071BC","#F15A24","#FBB03B","#93278F","#8A381A"],"title":"<span style='color:#333333;'>EVOLUCIÓN DEL NÚMERO DE GRADUADOS POR SEDE<\/span>","labels":["year","Amazonía","Bogotá","Caribe","Manizales","Medellín","Orinoquía","Palmira","Tumaco"],"retainDateWindow":false,"stackedGraph":false,"fillGraph":false,"fillAlpha":0.15,"stepPlot":false,"drawPoints":true,"pointSize":2,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":2,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":true,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"#787878","axisLineWidth":0.3,"axisLabelColor":"#525252","axisLabelFontSize":16,"axisLabelWidth":60,"drawGrid":true,"gridLineColor":"lightblue","gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false,"legend":"always","labelsDivWidth":650,"labelsShowZeroValues":true,"labelsSeparateLines":false,"xlabel":"Periodo","ylabel":"Número de Graduados (k: miles)","showRangeSelector":true,"rangeSelectorHeight":30,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"","interactionModel":"Dygraph.Interaction.defaultModel","highlightCircleSize":5,"highlightSeriesBackgroundAlpha":0.5,"highlightSeriesOpts":{"strokeWidth":2.5},"hideOverlayOnMouseOut":true},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["2009-01-01T00:00:00.000Z","2009-07-01T00:00:00.000Z","2010-01-01T00:00:00.000Z","2010-07-01T00:00:00.000Z","2011-01-01T00:00:00.000Z","2011-07-01T00:00:00.000Z","2012-01-01T00:00:00.000Z","2012-07-01T00:00:00.000Z","2013-01-01T00:00:00.000Z","2013-07-01T00:00:00.000Z","2014-01-01T00:00:00.000Z","2014-07-01T00:00:00.000Z","2015-01-01T00:00:00.000Z","2015-07-01T00:00:00.000Z","2016-01-01T00:00:00.000Z","2016-07-01T00:00:00.000Z","2017-01-01T00:00:00.000Z","2017-07-01T00:00:00.000Z","2018-01-01T00:00:00.000Z","2018-07-01T00:00:00.000Z","2019-01-01T00:00:00.000Z","2019-07-01T00:00:00.000Z","2020-01-01T00:00:00.000Z","2020-07-01T00:00:00.000Z","2021-01-01T00:00:00.000Z"],[1,5,7,8,10,12,3,3,4,11,5,4,2,13,2,15,16,20,21,17,9,42,41,18,29],[3323,2042,3441,1924,2850,2382,1931,3158,1780,2764,2622,2716,2544,2824,2769,2822,3264,2755,3153,2867,1749,4200,3016,2449,2575],[0,0,0,0,11,1,0,1,0,5,0,4,5,6,5,12,11,16,4,15,5,24,8,7,11],[468,447,485,452,427,460,399,463,395,498,407,495,484,544,582,572,641,647,613,641,269,816,606,611,613],[981,412,1153,814,867,365,839,1065,839,925,990,931,982,1022,1107,1026,1098,1068,1015,1092,486,1562,1053,506,907],[0,0,0,0,0,0,0,0,0,0,4,6,15,14,15,20,19,22,28,29,8,56,29,25,29],[183,202,187,164,184,97,101,171,161,196,157,196,236,208,226,235,233,271,290,441,164,296,173,197,235],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]],"fixedtz":false,"tzone":"UTC","plugins":{"Unzoom":"{}"}},"evals":["attrs.axes.x.axisLabelFormatter","attrs.interactionModel","plugins.Unzoom"],"jsHooks":[]}# ---------------------------------------------------------------------------
# Agrupando para eliminar el semestre
# library("dplyr")
df <- ejConsolidadoGrad |> group_by(Variable, YEAR, Clase) |>
  summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")

Msj <- "Comportamiento anual, considerando ambos semestres (exceptuando el caso del 2021)."
Plot.Series(
  datos     = df,
  categoria = "SEXO",
  ylim      = c(1000, 6000),
  colores   = c("#3360FF", "#F30081"),
  titulo    = "EVOLUCI\u00d3N DEL N\u00daMERO DE GRADUADOS POR SEXO",
  labelX    = "A\u00f1o",
  labelY    = "N\u00famero de Graduados",
  libreria  = "highcharter",
  estilo    = list(hc.Tema = 1, hc.Credits = Msj)
)

{"x":{"hc_opts":{"chart":{"reflow":true,"type":"datetime","zoomType":"x"},"title":{"text":"EVOLUCIÓN DEL NÚMERO DE GRADUADOS POR SEXO","style":{"fontWeight":"bold","fontSize":"22px","color":"#333333","useHTML":true}},"yAxis":{"title":{"text":"Número de Graduados","offset":70,"style":{"fontWeight":"bold","fontSize":"18px","color":"black"}},"type":"linear","reversed":false,"lineColor":"#787878","opposite":false,"lineWidth":1,"min":1000,"max":6000,"labels":{"format":"{value}","style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"credits":{"enabled":false},"exporting":{"enabled":true,"filename":"PlotSeries_Clase"},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":true},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}},"line":{"marker":{"enabled":false,"symbol":"square","radius":1}}},"series":[{"name":"Hombres","data":[{"Fecha":"2009","Clase":"Hombres","Y":4378,"Extra":54.3,"y":4378,"name":"2009"},{"Fecha":"2010","Clase":"Hombres","Y":4846,"Extra":56.1,"y":4846,"name":"2010"},{"Fecha":"2011","Clase":"Hombres","Y":4312,"Extra":56.2,"y":4312,"name":"2011"},{"Fecha":"2012","Clase":"Hombres","Y":4569,"Extra":56.2,"y":4569,"name":"2012"},{"Fecha":"2013","Clase":"Hombres","Y":4302,"Extra":56.8,"y":4302,"name":"2013"},{"Fecha":"2014","Clase":"Hombres","Y":4845,"Extra":56.8,"y":4845,"name":"2014"},{"Fecha":"2015","Clase":"Hombres","Y":5106,"Extra":57.4,"y":5106,"name":"2015"},{"Fecha":"2016","Clase":"Hombres","Y":5482,"Extra":58.3,"y":5482,"name":"2016"},{"Fecha":"2017","Clase":"Hombres","Y":5947,"Extra":59,"y":5947,"name":"2017"},{"Fecha":"2018","Clase":"Hombres","Y":5857,"Extra":57.3,"y":5857,"name":"2018"},{"Fecha":"2019","Clase":"Hombres","Y":5678,"Extra":58.6,"y":5678,"name":"2019"},{"Fecha":"2020","Clase":"Hombres","Y":5247,"Extra":60,"y":5247,"name":"2020"},{"Fecha":"2021","Clase":"Hombres","Y":2696,"Extra":61.3,"y":2696,"name":"2021"}],"type":"line","color":"#3360FF","zoomType":{"enabled":false},"resetZoomButton":true},{"name":"Mujeres","data":[{"Fecha":"2009","Clase":"Mujeres","Y":3686,"Extra":45.7,"y":3686,"name":"2009"},{"Fecha":"2010","Clase":"Mujeres","Y":3789,"Extra":43.9,"y":3789,"name":"2010"},{"Fecha":"2011","Clase":"Mujeres","Y":3354,"Extra":43.8,"y":3354,"name":"2011"},{"Fecha":"2012","Clase":"Mujeres","Y":3565,"Extra":43.8,"y":3565,"name":"2012"},{"Fecha":"2013","Clase":"Mujeres","Y":3276,"Extra":43.2,"y":3276,"name":"2013"},{"Fecha":"2014","Clase":"Mujeres","Y":3692,"Extra":43.2,"y":3692,"name":"2014"},{"Fecha":"2015","Clase":"Mujeres","Y":3793,"Extra":42.6,"y":3793,"name":"2015"},{"Fecha":"2016","Clase":"Mujeres","Y":3926,"Extra":41.7,"y":3926,"name":"2016"},{"Fecha":"2017","Clase":"Mujeres","Y":4134,"Extra":41,"y":4134,"name":"2017"},{"Fecha":"2018","Clase":"Mujeres","Y":4369,"Extra":42.7,"y":4369,"name":"2018"},{"Fecha":"2019","Clase":"Mujeres","Y":4008,"Extra":41.4,"y":4008,"name":"2019"},{"Fecha":"2020","Clase":"Mujeres","Y":3492,"Extra":40,"y":3492,"name":"2020"},{"Fecha":"2021","Clase":"Mujeres","Y":1704,"Extra":38.7,"y":1704,"name":"2021"}],"type":"line","color":"#F30081","zoomType":{"enabled":false},"resetZoomButton":true}],"xAxis":{"type":"category","title":{"text":"Año","offset":70,"style":{"fontWeight":"bold","fontSize":"18px","color":"black"}},"categories":["2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021"],"align":"center","lineColor":"#787878","opposite":false,"labels":{"style":{"fontWeight":"bold","color":"black","fontSize":"18px"}}},"legend":{"enabled":true,"align":"center","verticalAlign":"bottom","layout":"horizontal","title":{"text":"","style":{"textDecoration":"underline"}},"x":42,"y":0,"itemStyle":{"fontWeight":"bold","color":"black","fontSize":"18px"}},"tooltip":{"crosshairs":true,"shared":true,"pointFormat":"<span style=\"color:{series.color}\">● <\/span><b>{series.name}: {point.y}<\/b> ({point.Extra}%)<br/>","backgroundColor":"rgba(186,174,174,0.7)","borderColor":"#6D6666","borderWidth":5,"useHTML":true},"subtitle":{"text":"Comportamiento anual, considerando ambos semestres (exceptuando el caso del 2021).","align":"left","style":{"color":"#2B908F","fontWeight":"bold"}}},"theme":{"colors":["#00AACC","#FF4E00","#B90000","#5F9B0A","#CD6723"],"chart":{"backgroundColor":{"linearGradient":[0,0,0,150],"stops":[[0,"#CAE1F4"],[1,"#EEEEEE"]]},"style":{"fontFamily":"Open Sans"}},"title":{"align":"left"},"subtitle":{"align":"left"},"legend":{"align":"right","verticalAlign":"bottom"},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Menú Contextual del Gráfico","decimalPoint":",","downloadCSV":"Descargar CSV","downloadJPEG":"Descargar imagen JPEG","downloadPDF":"Descargar documento PDF","downloadPNG":"Descargar imagen PNG","downloadSVG":"Descargar imagen vectorial SVG","downloadXLS":"Descargar XLS","drillUpText":"<< Volver a {series.name}","exitFullscreen":"Exit from full screen","exportData":{"annotationHeader":"Annotations","categoryDatetimeHeader":"DateTime","categoryHeader":"Category"},"hideData":"Hide data table","invalidDate":null,"loading":"Cargando...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No hay información para mostrar","numericSymbolMagnitude":1000,"numericSymbols":["k","M","G","T","P","E"],"printChart":"Imprimir gráfico","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"shortWeekdays":["Sat","Sun","Mon","Tue","Wed","Thu","Fri"],"thousandsSep":".","viewData":"Ver tabla de datos","viewFullscreen":"Ver en pantalla completa","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":"Open+Sans","debug":false},"evals":[],"jsHooks":[]}# ---------------------------------------------------------------------------
# Ejemplo usando el caso estático (ggplot2)
# library("magick"); library("cowplot")
txtA <- "EVOLUCI\u00d3N DEL N.\u00ba DE GRADUADOS \u00d7 SEDE"
txtB <- "\nComportamiento anual (exceptuando el caso del 2021)."
fig1 <- Plot.Series(
  datos        = ejConsolidadoGrad,
  categoria    = "SEDE_NOMBRE_ADM",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(100, 2000),
  colores      = misColores,
  titulo       = txtA,
  labelY       = "N\u00famero de Graduados",
  estatico     = TRUE,
  estilo       = list(
    LegendTitle = "SEDE:", gg.Tema = 8,
    gg.Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
    gg.Linea  = list(linetype = 2, size = 0.1, arrow = grid::arrow()),
    gg.Punto  = list(alpha = 0.2, shape = 21, size = 2, stroke = 5),
    gg.Texto  = list(
      subtitle = txtB, caption = "\t\t Informaci\u00f3n Disponible desde 2009-1", tag = "\u00ae"
    )
  )
)
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the UnalR package.
#>   Please report the issue at <https://github.com/estadisticaun/UnalR/issues>.
# A continuación, se detalla el caso en el que quiera adicionar un logo a 'fig1'
# library("ggplot2"); library("magick"); require("cowplot")
URL <- "https://upload.wikimedia.org/wikipedia/commons/1/1e/UNAL_Logosimbolo.svg"
LogoUN <- magick::image_read_svg(URL)
#> Error in curl::curl_download(url, tmp, handle = h): HTTP response code said error [upload.wikimedia.org]:
#> The requested URL returned error: 429
ggdraw() +
  draw_image(LogoUN, scale = 0.15, x = 0.15, hjust = 1, halign = 1, valign = 0) +
  draw_plot(fig1 + theme(legend.background = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank()
                         )
  )
#> Error: objeto 'LogoUN' no encontrado
# ---------------------------------------------------------------------------
# A continuación, se detalla el caso en el que quiera anotaciones textuales repulsivas
#   * (1) Espacio vacío que se debe respetar alrededor de la caja delimitadora
#   * (2) Espacio vacío que se debe respetar alrededor de cada punto
#   * (3) Entre más bajo más flechas, entre más distancia menos flechas
Plot.Series(
  datos        = ejConsolidadoGrad,
  categoria    = "SEDE_NOMBRE_ADM",
  freqRelativa = FALSE,
  invertir     = FALSE,
  ylim         = c(100, 2000),
  colores      = misColores,
  titulo       = "EVOLUCI\u00d3N DEL N.\u00ba DE GRADUADOS \u00d7 SEDE",
  labelY       = "N\u00famero de Graduados",
  estatico     = TRUE,
  estilo       = list(
    gg.Tema  = 1,
    gg.Repel = list(
      direction = "both", seed = 42, nudge_y = 0.25,
      arrow = arrow(length = unit(0.01, "npc")), segment.colour = "#4C716B",
      box.padding   = 0.5 ,     # (1)
      point.padding = 0.25,     # (2)
      min.segment.length = 0.45 # (3)
    )
  )
)
#> Warning: Removed 107 rows containing missing values or values outside the scale range
#> (`geom_line()`).
#> Warning: Removed 122 rows containing missing values or values outside the scale range
#> (`geom_point()`).
#> Warning: Removed 122 rows containing missing values or values outside the scale range
#> (`geom_text_repel()`).
```
