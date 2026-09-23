# Obtenga la ruta de los archivos de Excel para la función `Agregar()`

UnalR viene con algunos archivos de ejemplo en su directorio
`inst/extdata`. Esta función facilita el acceso a ellos.

## Uso

``` r
read_example(ruta = NULL)
```

## Argumentos

- ruta:

  Nombre del archivo. Si es `NULL`, se enumerarán los archivos de
  ejemplo.

## Valor

Cadena de caracteres indicando la ruta absoluta (*no relativa a la
carpeta en donde se ubique*) en donde se encuentra el archivo
especificado dentro del paquete.

## Ejemplos

``` r
if (FALSE) { # all(FALSE)
read_example("TestConsolidado1.xlsx")
}
```
