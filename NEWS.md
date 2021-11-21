# UnalR 0.1.0 (*minor version*)

v0.1.0 fue lanzada el 28/11/2021

## Cambios
  * Se modificó la selección y el orden de algunos temas de highcharter usados en el argumento `hc.Tema`.
  * Por cuestiones estéticas en `Plot.Mapa()` se redondea el valor de cualquier estadístico a tres cifras decimales.

## Nuevas características (*new features*)
  * Se adicionó las funciones `Agregar()`, `Plot.Boxplot()`, `Plot.Radar()`, `Plot.Treemap()`, `Plot.Drilldown()` y `Plot.Apiladas()`.
  * Se incluyó nuevos argumentos en la función `Plot.Series()` tales como `freqRelativa`, `invertir` y `ylim`.
  * Se incorporó a la función `Plot.Barras()` el argumento `freqRelativa`.
  * Se incorporó a la función `Plot.Torta()` el argumento `label`.
  * Se añade en `Plot.Mapa()` los argumento `estadistico` (*para mostrar distintas estadísticas descriptivas*), `naTo0` y `colNA` además de agregar un nuevo estadístico (*coeficiente de variación* **CV**) en el argumento `estadistico`.
  * Las nuevas funciones incorporan la gramática del tidyverse, la cual permite trabajar los datos como si fueran objetos reales en el espacio de trabajo. Para más información del framework tidy evaluation puede consultar [aquí](https://tidyeval.tidyverse.org).
  * La escritura de todas las funciones (*incluyendo la documentación y los ejemplos*) cumplen con la guía de estilos del tidyverse, la cual puede consultar [aquí](https://style.tidyverse.org).

## Correcciones (*bug fixes*)
  * Se corrigieron algunos errores ortográficos en la documentación del paquete.
  * Se modificó la forma de calcular el centroide de los polígonos en `Plot.Mapa()`, pues para aquellos municipios cuyo polígono espacial es altamente irregular su centroide caía fuera de éste.
  * Para los municipios homónimos, en `Plot.Mapa()`, se subsanó el problema con estos, pues en la "Lupa" no era posible diferenciarlos y realizar la búsqueda de forma correcta.
  * Se solvento el error en `Plot.Mapa()` presente en el argumento `centroideMapa`, en el cual al especificarlo el mapa sí iniciaba en dicha ubicación pero al momento de cliquear en el botón "Retornar" no se enviaba al centroide especificado.

___

# UnalR 0.0.0.9000 (*development version*)

v0.0.0.9000 fue lanzada el 28/02/2021

* ¡Primera versión estable de implementación avanzada de `UnalR`!
* Muchas funciones escritas para hacer uso de métodos y clases. En consecuencia,
  se han implementado varias funciones y se han documentado muchos argumentos.
  Consulte la ayuda para obtener más detalles.
* Funciones añadidas `Tabla()`, `Tabla.SaberPro()`, `Plot.Series()`, `Plot.Torta()`,
  `Plot.Barras()`, `Plot.Mapa()` y `StaticPlot()`.
* Se realizaron varios cambios para garantizar la compatibilidad.
