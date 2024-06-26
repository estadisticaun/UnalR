% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Agregar.R
\name{Agregar}
\alias{Agregar}
\title{Cree el consolidado a partir de los microdatos con los que dispone, agrupando
por las variables temporales que considere necesario}
\usage{
Agregar(
  formula,
  frecuencia,
  datos,
  intervalo,
  textNA = "Sin Información",
  ask = TRUE
)
}
\arguments{
\item{formula}{Fórmula en la que el primer componente especificado (antes del
\eqn{\sim}) hace referencia a la variable de interés, y el segundo componente
(luego del \eqn{\sim}) a la(s) variable(s) temporales por la cuales se quiere
agrupar (separadas por cualquier operador como \code{+}, \code{*}, \code{:}, etc.).}

\item{frecuencia}{Vector o lista (dependiendo de la cantidad de variables temporales
especificadas) numérica con los periodos que debería tener cada una de éstas.}

\item{datos}{Un data frame con los microdatos.}

\item{intervalo}{Vector o lista (dependiendo de la cantidad de variables temporales
especificadas) numérica que contiene los períodos de inicio y fin con los cuales
se quiere realizar el filtro.}

\item{textNA}{Cadena de caracteres indicando el nombre que se dará a los registros
cuando éstos presenten algún dato faltante (en la variable seleccionada más no
en las variables temporales especificadas). El valor por defecto es "Sin Información".}

\item{ask}{Si es \code{TRUE} (valor predeterminado) mostrará un mensaje por consola
preguntándole al usuario cuál periodo desea que sea el último por considerar
(esperando una respuesta por consola). Si previamente se ha introducido el
argumento intervalo éste quedará inhabilitado y no se ejecutará.}
}
\value{
Un data frame o tibble perteneciente a las clases "tbl_df", "tbl" y "data.frame".
}
\description{
Esta función permite transformar desde la base de microdatos para obtener el
consolidado final con el cual trabaja la mayoría de las funciones gráficas
disponibles en \code{UnalR}.
}
