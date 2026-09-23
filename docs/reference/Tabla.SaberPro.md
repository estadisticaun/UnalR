# Cree fácilmente un widget para visualizar los resultados de la prueba Saber Pro en tablas HTML usando el paquete `DT`

**\[deprecated\]**

Esta función está diseñada para facilitar la creación de tablas para
informes y publicaciones produciendo un widget HTML para visualizar un
data frame utilizando el paquete `DT`. La forma en que esta función
maneja las cosas por usted significa que a menudo no tiene que
preocuparse por los pequeños detalles para obtener un resultado
impresionante y listo para usar.

## Uso

``` r
Tabla.SaberPro(
  datos,
  variable,
  encabezado = "Encabezados de los Niveles de la Categoría",
  leyenda,
  tituloPdf = NULL,
  mensajePdf = "",
  ajustarNiveles = TRUE,
  scrollX = TRUE,
  colorHead = "#FFFFFF",
  colorear = FALSE,
  estilo
)
```

## Argumentos

- datos:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- variable:

  Análogo al argumento `categoria` de la función
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- encabezado:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- leyenda:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)
  con la excepción de que, si no se introduce ningún valor, el valor por
  defecto será una nota explicando a qué hace referencia los valores y
  columnas de la tabla.

- tituloPdf:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- mensajePdf:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- ajustarNiveles:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- scrollX:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- colorHead:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- colorear:

  Igual uso que en
  [`Tabla()`](https://estadisticaun.github.io/UnalR/reference/Tabla.md)

- estilo:

  Una lista compuesta por dos parámetros:

  - `PaletaYear`: Vector de caracteres que especifica los colores de
    fondo para los años.

  - `PaletaCategoria`: Vector de caracteres que especifica los colores
    de fuente para las distintas categorías de la `variable`.

## Valor

Retorna la tabla creada mediante `DT` la cual pertenece a la clase
"datatables" y "htmlwidget".

## Detalles

Esta función se basa enteramente del paquete `DT`, el cual proporciona
una interfaz para `R` a la biblioteca `DataTables` de `JavaScript`. Los
data frames de `R` se pueden mostrar como tablas en páginas HTML,
proporcionando opciones de filtrado, paginación, clasificación y muchas
otras características en las tablas.

## Ejemplos

``` r
if (require("dplyr")) {
  VariosYears <- ejConsolidadoSaberPro2019 |>
    mutate(YEAR = replace(YEAR, YEAR==2019, 2020)) |>
    bind_rows(ejConsolidadoSaberPro2019)
}
Msj <- "\u00c9sta es una descripci\u00f3n de la tabla diferente al valor por default."
Tabla.SaberPro(
  datos      = VariosYears,
  variable   = "SEXO",
  encabezado = "PUNTAJES POR SEXO",
  leyenda    = Msj,
  colorHead  = "#FF5B5B",
  estilo     = list(
    PaletaYear = c("#F9CA00", "#F68118"),
    PaletaCategoria = c("#2458C5", "#F0006D", "#42C501")
  )
)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;2019&quot;,&quot;2020&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;F&quot;,&quot;M&quot;,&quot;Total IES&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1880\" data-max=\"232279\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","class":"compact nowrap hover row-border","extensions":["Buttons","KeyTable"],"caption":"<caption style=\"caption-side: bottom; text-align: center;\">\n  Nota: \n  <em>Ésta es una descripción de la tabla diferente al valor por default.<\/em>\n<\/caption>","data":[["2020","2020","2019","2019","2020","2019"],["M","F","M","F","Total IES","Total IES"],["191.26 (28.85)","182.72 (27.47)","191.26 (28.85)","182.72 (27.47)","146.95 (31.49)","146.95 (31.49)"],["180.98 (28.55)","185 (26.56)","180.98 (28.55)","185 (26.56)","149.32 (31.19)","149.32 (31.19)"],["176.26 (33.75)","176.19 (29.47)","176.26 (33.75)","176.19 (29.47)","143.26 (32.99)","143.26 (32.99)"],["187.99 (29.11)","185.64 (27.87)","187.99 (29.11)","185.64 (27.87)","152.78 (31.28)","152.78 (31.28)"],["157.59 (32.3)","160.93 (31.3)","157.59 (32.3)","160.93 (31.3)","148.67 (27.31)","148.67 (27.31)"],["178.82 (19.83)","178.1 (19.37)","178.82 (19.83)","178.1 (19.37)","148.2 (22.5)","148.2 (22.5)"],[3309,1880,3309,1880,232279,232279]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th rowspan=\"2\">Año<\/th>\n      <th rowspan=\"2\">Categoría<\/th>\n      <th colspan=\"6\">PUNTAJES POR SEXO<\/th>\n      <th rowspan=\"2\">N*<\/th>\n    <\/tr>\n    <tr>\n      <th>Cuantitativo<\/th>\n      <th>Lectura<\/th>\n      <th>Competencias<\/th>\n      <th>Inglés<\/th>\n      <th>Comunicación<\/th>\n      <th>Global<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"autoWidth":true,"columnDefs":[{"className":"dt-center","targets":[0,1,2,3,4,5,6,7,8]},{"targets":[2,3,4,5,6,7],"searchable":false},{"width":"65px","targets":0},{"name":"YEAR","targets":0},{"name":"Clase","targets":1},{"name":"Cuantitativo","targets":2},{"name":"Lectura","targets":3},{"name":"Competencias","targets":4},{"name":"Inglés","targets":5},{"name":"Comunicación","targets":6},{"name":"Global","targets":7},{"name":"n","targets":8}],"pageLength":8,"order":[[0,"desc"],[1,"asc"]],"dom":"Bfrtip","keys":true,"searchHighlight":true,"scrollX":true,"initComplete":"function(settings, json) {\n$(this.api().table().header()).css({'background-color':\n'#FF5B5B'\n, 'color': '#000000'});\n}","language":{"processing":"Procesando...","lengthMenu":"Mostrar _MENU_ registros","zeroRecords":"No se encontraron resultados","emptyTable":"Ningún dato disponible en esta tabla","info":"Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros","infoEmpty":"Mostrando registros del 0 al 0 de un total de 0 registros","infoFiltered":"(filtrado de un total de _MAX_ registros)","infoPostFix":"","search":"Buscar:","url":"","infoThousands":",","loadingRecords":"Cargando...","paginate":{"first":"Primero","last":"Último","next":"Siguiente","previous":"Anterior"},"aria":{"sortAscending":"Activar para ordenar la columna de manera ascendente","sortDescending":"Activar para ordenar la columna de manera descendente"}},"buttons":[{"extend":"copy","text":"Copiar"},"csv","excel",{"extend":"pdf","pageSize":"A4","filename":"pdf","message":"","title":"PUNTAJES POR SEXO"},{"extend":"print","text":"Imprimir","pageSize":"A4","message":"","title":"PUNTAJES POR SEXO"}],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[8,10,25,50,100],"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','background-color':value == \"2020\" ? \"#F9CA00\" : value == \"2019\" ? \"#F68118\" : null});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':value == \"M\" ? \"#2458C5\" : value == \"F\" ? \"#F0006D\" : value == \"Total IES\" ? \"#42C501\" : null});\n}"},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":["options.initComplete","options.rowCallback"],"jsHooks":[]}Tabla.SaberPro(
  datos      = VariosYears,
  variable   = "SEDE",
  encabezado = "PUNTAJES POR SEDE",
  leyenda    = Msj,
  colorHead  = "#F9CA00",
  estilo     = list(
    PaletaYear = c("#AEF133", "#19EE9F"),
    PaletaCategoria = c("#DD1C1A", "#FF6700", "#7E10DE","#0096F2", "#42C501")
  )
)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;2019&quot;,&quot;2020&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"factor\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"width: 100%; display: none;\">\n      <select multiple=\"multiple\" style=\"width: 100%;\" data-options=\"[&quot;Bogotá&quot;,&quot;Manizales&quot;,&quot;Medellín&quot;,&quot;Palmira&quot;,&quot;Total IES&quot;]\"><\/select>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"298\" data-max=\"232279\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","class":"compact nowrap hover row-border","extensions":["Buttons","KeyTable"],"caption":"<caption style=\"caption-side: bottom; text-align: center;\">\n  Nota: \n  <em>Ésta es una descripción de la tabla diferente al valor por default.<\/em>\n<\/caption>","data":[["2020","2020","2020","2020","2019","2019","2019","2019","2020","2019"],["Bogotá","Medellín","Manizales","Palmira","Bogotá","Medellín","Manizales","Palmira","Total IES","Total IES"],["189.13 (27.75)","195.77 (28.9)","180.52 (25.97)","163.28 (25.97)","189.13 (27.75)","195.77 (28.9)","180.52 (25.97)","163.28 (25.97)","146.95 (31.49)","146.95 (31.49)"],["188.47 (25.61)","179.54 (27.75)","166.71 (28.38)","160.92 (28.38)","188.47 (25.61)","179.54 (27.75)","166.71 (28.38)","160.92 (28.38)","149.32 (31.19)","149.32 (31.19)"],["183.13 (29.86)","172.01 (32.21)","158.99 (32.15)","153.65 (32.15)","183.13 (29.86)","172.01 (32.21)","158.99 (32.15)","153.65 (32.15)","143.26 (32.99)","143.26 (32.99)"],["193.18 (26.84)","185.54 (27.86)","171.94 (26.72)","159.31 (26.72)","193.18 (26.84)","185.54 (27.86)","171.94 (26.72)","159.31 (26.72)","152.78 (31.28)","152.78 (31.28)"],["162.59 (33.14)","155.73 (30.65)","150.39 (26.47)","147.21 (26.47)","162.59 (33.14)","155.73 (30.65)","150.39 (26.47)","147.21 (26.47)","148.67 (27.31)","148.67 (27.31)"],["183.3 (17.98)","177.73 (18.59)","165.71 (17.95)","156.86 (17.95)","183.3 (17.98)","177.73 (18.59)","165.71 (17.95)","156.86 (17.95)","148.2 (22.5)","148.2 (22.5)"],[3146,1161,584,298,3146,1161,584,298,232279,232279]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th rowspan=\"2\">Año<\/th>\n      <th rowspan=\"2\">Categoría<\/th>\n      <th colspan=\"6\">PUNTAJES POR SEDE<\/th>\n      <th rowspan=\"2\">N*<\/th>\n    <\/tr>\n    <tr>\n      <th>Cuantitativo<\/th>\n      <th>Lectura<\/th>\n      <th>Competencias<\/th>\n      <th>Inglés<\/th>\n      <th>Comunicación<\/th>\n      <th>Global<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"autoWidth":true,"columnDefs":[{"className":"dt-center","targets":[0,1,2,3,4,5,6,7,8]},{"targets":[2,3,4,5,6,7],"searchable":false},{"width":"65px","targets":0},{"name":"YEAR","targets":0},{"name":"Clase","targets":1},{"name":"Cuantitativo","targets":2},{"name":"Lectura","targets":3},{"name":"Competencias","targets":4},{"name":"Inglés","targets":5},{"name":"Comunicación","targets":6},{"name":"Global","targets":7},{"name":"n","targets":8}],"pageLength":8,"order":[[0,"desc"],[1,"asc"]],"dom":"Bfrtip","keys":true,"searchHighlight":true,"scrollX":true,"initComplete":"function(settings, json) {\n$(this.api().table().header()).css({'background-color':\n'#F9CA00'\n, 'color': '#000000'});\n}","language":{"processing":"Procesando...","lengthMenu":"Mostrar _MENU_ registros","zeroRecords":"No se encontraron resultados","emptyTable":"Ningún dato disponible en esta tabla","info":"Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros","infoEmpty":"Mostrando registros del 0 al 0 de un total de 0 registros","infoFiltered":"(filtrado de un total de _MAX_ registros)","infoPostFix":"","search":"Buscar:","url":"","infoThousands":",","loadingRecords":"Cargando...","paginate":{"first":"Primero","last":"Último","next":"Siguiente","previous":"Anterior"},"aria":{"sortAscending":"Activar para ordenar la columna de manera ascendente","sortDescending":"Activar para ordenar la columna de manera descendente"}},"buttons":[{"extend":"copy","text":"Copiar"},"csv","excel",{"extend":"pdf","pageSize":"A4","filename":"pdf","message":"","title":"PUNTAJES POR SEDE"},{"extend":"print","text":"Imprimir","pageSize":"A4","message":"","title":"PUNTAJES POR SEDE"}],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[8,10,25,50,100],"rowCallback":"function(row, data, displayNum, displayIndex, dataIndex) {\nvar value=data[0]; $(this.api().cell(row, 0).node()).css({'font-weight':'bold','background-color':value == \"2020\" ? \"#AEF133\" : value == \"2019\" ? \"#19EE9F\" : null});\nvar value=data[1]; $(this.api().cell(row, 1).node()).css({'font-weight':'bold','color':value == \"Bogotá\" ? \"#DD1C1A\" : value == \"Medellín\" ? \"#FF6700\" : value == \"Manizales\" ? \"#7E10DE\" : value == \"Palmira\" ? \"#0096F2\" : value == \"Total IES\" ? \"#42C501\" : null});\n}"},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":["options.initComplete","options.rowCallback"],"jsHooks":[]}
```
