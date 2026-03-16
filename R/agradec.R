#' Transforma grados decimales a grados y minutos decimales
#' Convierte valores expresados en grados decimales a un formato donde la parte
#' entera son los grados y la parte decimal representa los minutos decimales
#' (sin segundos). Por ejemplo, 43.968 se convierte en 43 grados y 0.968*60 minutos.
#' #' @param x Vector numérico con grados decimales.
#' #' @examples
#' #' agradec(43.968)
#' #' @family Conversión unidades
#' #' @export
agradec <- function(x) {
  if (!is.numeric(x)) { stop("x debe ser numérico (grados decimales)")
  }
  grados <- trunc(x)
  minutos_dec <- (x - grados) * 60
  # Resultado en formato GGG.MMmm (minutos decimales sin decimal)
  return(grados + minutos_dec / 100)
  }
