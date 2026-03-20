#' Conversión de grados decimales a grados y minutos decimales
#'
#' Convierte coordenadas en grados decimales al formato grados + minutos
#' decimales (GGG.MMmm), donde la parte entera son los grados y los dos
#' primeros decimales representan los minutos. Por ejemplo, `43.968°` →
#' `43 + (0.968 × 60) / 100` = `43.5808`.
#'
#' @details
#' El formato de salida es el utilizado internamente por el programa CAMP
#' (Harbour/Clipper) para almacenar coordenadas en los ficheros `.dbf`.
#' La función inversa es [gradec()], que convierte de GGG.MMmm a grados
#' decimales.
#'
#' @param x Vector numérico con coordenadas en grados decimales.
#'
#' @returns Vector numérico en formato GGG.MMmm (grados + minutos decimales).
#'
#' @seealso [gradec()], [gradms()]
#'
#' @family Conversión de unidades
#'
#' @examples
#' agradec(43.968)   # → 43.5808
#' agradec(c(43.968, -8.5))
#'
#' @export
agradec <- function(x) {
  if (!is.numeric(x)) stop("x debe ser numérico (grados decimales)")
  grados <- trunc(x)
  minutos_dec <- (x - grados) * 60
  return(grados + minutos_dec / 100)
}