# UnalR [![](reference/figures/Logo.png)](https://estadisticaun.github.io/UnalR/)

El paquete `UnalR` proporciona métodos y herramientas para la gestión y
disposición de estadísticas institucionales. Su objetivo principal es
disponer, facilitar y optimizar la disposición de microdatos y la
visualización de las cifras y estadísticas oficiales de la [Universidad
Nacional de Colombia](https://unal.edu.co) las cuales se encuentran
disponibles, dentro del [sistema de
planeación](https://planeacion.unal.edu.co/home/) institucional, en el
componente de [estadísticas
oficiales](https://estadisticas.unal.edu.co/home/). Contiene una
biblioteca de funciones gráficas, tanto estáticas como interactivas, que
ofrece numerosos tipos de gráficos con una sintaxis altamente
configurable y simple. Entre estos, encontramos la visualización de
tablas `HTML`, series, gráficos de barras y circulares, mapas, boxplots,
radar charts, treemaps, drilldown, etc. Todo lo anterior apoyado en
bibliotecas de `JavaScript`.

El paquete `UnalR` permanecerá en su estado actual y se mantendrá solo
con correcciones de errores.

## Instalación

Puede instalar la versión publicada de `UnalR` (*versión menor*) desde
[GitHub](https://github.com/) con:

``` r

# Versión oficial (estable)
install.packages("UnalR")
# Última versión (dev)
devtools::install_github("estadisticaun/UnalR")
remotes::install_github("estadisticaun/UnalR")
```

## Uso

Es difícil describir de manera sucinta cómo funciona `UnalR` porque
encarna una profunda filosofía de visualización que abarca las
principales librerías para representar de forma dinámica e interactiva
datos usando `JavaScript` (**htmlwidgets**).

## Ejemplo

``` r

library(UnalR)
example(topic = Plot.Apiladas, package = "UnalR")
```

![](reference/figures/ExampleApiladas1.png)![](reference/figures/ExampleApiladas2.png)

``` r

example(topic = Plot.Barras, package = "UnalR")
```

![](reference/figures/ExampleBarras1.png)![](reference/figures/ExampleBarras2.png)

``` r

example(topic = Plot.Boxplot, package = "UnalR")
```

![](reference/figures/ExampleBoxplot1.png)![](reference/figures/ExampleBoxplot2.png)![](reference/figures/ExampleBoxplot3.png)

``` r

example(topic = PPlot.Histograma, package = "UnalR")
```

![](reference/figures/ExampleHistograma1.jpeg)![](reference/figures/ExampleHistograma2.jpeg)

``` r

example(topic = Plot.Mapa, package = "UnalR")
```

![](reference/figures/ExampleMapa1.png)![](reference/figures/ExampleMapa2.png)

``` r

example(topic = Plot.Mundo, package = "UnalR")
```

![](reference/figures/ExampleMundo1.jpeg)![](reference/figures/ExampleMundo2.jpeg)![](reference/figures/ExampleMundo3.jpeg)

``` r

example(topic = Plot.Series, package = "UnalR")
```

![](reference/figures/ExampleSeries1.png)![](reference/figures/ExampleSeries2.png)

``` r

example(topic = Tabla, package = "UnalR")
```

![](reference/figures/ExampleTabla1.png)

``` r

example(topic = Plot.Treemap, package = "UnalR")
```

![](reference/figures/ExampleTreemap1.PNG)![](reference/figures/ExampleTreemap2.PNG)![](reference/figures/ExampleTreemap3.PNG)![](reference/figures/ExampleTreemap4.PNG)

## Licencia

Este paquete es un software gratuito y de código abierto, con licencia
GPL-3.

## Ayuda

Si requiere ayuda para usar `UnalR`:

- Para problemas de instalación, comuníquese a los correos electrónicos
  proporcionados en la documentación del paquete, normalmente
  respondemos con prontitud y usted también ayudará a futuros usuarios.

Si cree que ha encontrado un error:

- Instale la versión de desarrollo de `UnalR` usando `devtools` (*ver
  arriba*) y vea si eso ayuda.
- Consulte los [problemas de github](https://github.com/estadisticaun).

¡Gracias por tu interés en `UnalR`!
