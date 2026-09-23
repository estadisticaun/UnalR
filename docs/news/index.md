# Registro de cambios

## UnalR 1.0.2

v1.0.2 fue lanzada el 01/10/2026

### Correcciones (*bug fixes*)

- Se retiró la dependencia archivada `leaflet.extras`.
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)
  y
  [`Plot.Mundo()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mundo.md)
  conservan la búsqueda de lugares y el control de pantalla completa
  mediante una integración interna y limitada de Leaflet-search y
  Leaflet.fullscreen.
- Se reemplazó `"CartoDB.Positron"` por `"Esri.WorldGrayCanvas"` en la
  baldosa por defecto de
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)
  y
  [`Plot.Mundo()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mundo.md),
  ya que CARTO ahora exige una API key y mostraba una marca de agua en
  los mapas que no especificaban el argumento `baldosas`.

------------------------------------------------------------------------

## UnalR 1.0.1

Versión CRAN: 2025-09-19

v1.0.1 fue lanzada el 15/09/2025

### Correcciones (*bug fixes*)

- Se ajustó la implementación para garantizar compatibilidad con
  `ggplot2` `v4`, actualizando las pruebas afectadas en
  `test-Plot_Series.R` según los nuevos requisitos de clase y tipo de
  objeto. Estos cambios resuelven el problema reportado por el equipo de
  `ggplot2`
  ([tidyverse/ggplot2#6498](https://github.com/tidyverse/ggplot2/issues/6498))
  y aseguran que el paquete pase las validaciones con la próxima versión
  mayor de `ggplot2`.

------------------------------------------------------------------------

## UnalR 1.0.0

Versión CRAN: 2024-05-25

v1.0.0 fue lanzada el 03/06/2023

### Cambios

- En los casos aplicables, se reemplaza el operador de `magrittr` por el
  “native pipe” de `R` (`%>%` *por* `|>`), incluido en la versión
  `4.1.0`, reemplazando así la “tubería” la cual es el icono distintivo
  de `dplyr` y el `tidyverse`.

### Nuevas características (*new features*)

- Se modifica la función
  [`Agregar()`](https://estadisticaun.github.io/UnalR/reference/Agregar.md),
  ahora permite realizar múltiples agregados simultáneamente, es decir,
  especificando más de una variable de interés. Internamente realiza
  agregados individuales y los concatena uno debajo del otro (*por
  filas*).
- Para las funciones
  [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md),
  [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md)
  y
  [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md)
  se agrega el parámetro `estatico`, el cual ahora permite generar el
  gráfico de manera invariable mediante la librería `ggplot2`. Para lo
  cual se agregan una serie de parámetros específicos para dicha
  librería (*dentro de `estilo`, las podrá encontrar cómo* `gg.*`).

### Correcciones (*bug fixes*)

- En tablas pequeñas, con pocas columnas, se desplazaba el encabezado
  (*nombre de las columnas*) a la izquierda, esto sin importar el tamaño
  de la ventana. Se establece el argumento `scrollX` (fijado antes como
  `TRUE`) en las funciones
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md),
  [`Tabla.General()`](https://estadisticaun.github.io/UnalR/reference/Tabla.General.md)
  y
  [`Tabla.SaberPro()`](https://estadisticaun.github.io/UnalR/reference/Tabla.SaberPro.md).
- Se solicita la inclusión del argumento `escape = TRUE` para que las
  diversas funciones de tablas permitan la inclusión de entidades
  **HTML**, se advierte que al ponerlo como `FALSE` puede haber posibles
  problemas de seguridad cuando la tabla se representa en aplicaciones
  web dinámicas.

------------------------------------------------------------------------

## UnalR 0.1.0 (*minor version*)

v0.1.0 fue lanzada el 28/11/2021

### Cambios

- Se modificó la selección y el orden de algunos temas de highcharter
  usados en el argumento `hc.Tema`.
- Por cuestiones estéticas en
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)
  se redondea el valor de cualquier estadístico a tres cifras decimales.

### Nuevas características (*new features*)

- Se adicionó las funciones
  [`Agregar()`](https://estadisticaun.github.io/UnalR/reference/Agregar.md),
  [`Plot.Boxplot()`](https://estadisticaun.github.io/UnalR/reference/Plot.Boxplot.md),
  [`Plot.Radar()`](https://estadisticaun.github.io/UnalR/reference/Plot.Radar.md),
  [`Plot.Treemap()`](https://estadisticaun.github.io/UnalR/reference/Plot.Treemap.md),
  [`Plot.Drilldown()`](https://estadisticaun.github.io/UnalR/reference/Plot.Drilldown.md)
  y
  [`Plot.Apiladas()`](https://estadisticaun.github.io/UnalR/reference/Plot.Apiladas.md).
- Se incluyó nuevos argumentos en la función
  [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md)
  tales como `freqRelativa`, `invertir` y `ylim`.
- Se incorporó a la función
  [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md)
  el argumento `freqRelativa`.
- Se incorporó a la función
  [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md)
  el argumento `label`.
- Se añade en
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)
  los argumento `estadistico` (*para mostrar distintas estadísticas
  descriptivas*), `naTo0` y `colNA` además de agregar un nuevo
  estadístico (*coeficiente de variación* **CV**) en el argumento
  `estadistico`.
- Las nuevas funciones incorporan la gramática del tidyverse, la cual
  permite trabajar los datos como si fueran objetos reales en el espacio
  de trabajo. Para más información del framework tidy evaluation puede
  consultar
  [aquí](https://dplyr.tidyverse.org/articles/programming.html).
- La escritura de todas las funciones (*incluyendo la documentación y
  los ejemplos*) cumplen con la guía de estilos del tidyverse, la cual
  puede consultar [aquí](https://style.tidyverse.org).

### Correcciones (*bug fixes*)

- Se corrigieron algunos errores ortográficos en la documentación del
  paquete.
- Se modificó la forma de calcular el centroide de los polígonos en
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md),
  pues para aquellos municipios cuyo polígono espacial es altamente
  irregular su centroide caía fuera de éste.
- Para los municipios homónimos, en
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md),
  se subsanó el problema con estos, pues en la “Lupa” no era posible
  diferenciarlos y realizar la búsqueda de forma correcta.
- Se solvento el error en
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)
  presente en el argumento `centroideMapa`, en el cual al especificarlo
  el mapa sí iniciaba en dicha ubicación pero al momento de cliquear en
  el botón “Retornar” no se enviaba al centroide especificado.

------------------------------------------------------------------------

## UnalR 0.0.0.9000 (*development version*)

v0.0.0.9000 fue lanzada el 28/02/2021

- ¡Primera versión estable de implementación avanzada de `UnalR`!
- Muchas funciones escritas para hacer uso de métodos y clases. En
  consecuencia, se han implementado varias funciones y se han
  documentado muchos argumentos. Consulte la ayuda para obtener más
  detalles.
- Funciones añadidas
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md),
  [`Tabla.SaberPro()`](https://estadisticaun.github.io/UnalR/reference/Tabla.SaberPro.md),
  [`Plot.Series()`](https://estadisticaun.github.io/UnalR/reference/Plot.Series.md),
  [`Plot.Torta()`](https://estadisticaun.github.io/UnalR/reference/Plot.Torta.md),
  [`Plot.Barras()`](https://estadisticaun.github.io/UnalR/reference/Plot.Barras.md),
  [`Plot.Mapa()`](https://estadisticaun.github.io/UnalR/reference/Plot.Mapa.md)
  y
  [`StaticPlot()`](https://estadisticaun.github.io/UnalR/reference/StaticPlot.md).
- Se realizaron varios cambios para garantizar la compatibilidad.
