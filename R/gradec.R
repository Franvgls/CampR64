#'Transforma grados y minutos decimales a grados decimales
#'
#'Transforma grados y minutos decimales a grados decimales, avisa si el dato es incorrecto, es decir minutos más de 60
#'@param x vector con grados y minutos en formato decimal
#'@examples gradec(43.3080)
#'@family Conversion unidades
#'@export
gradec <- function(x) {
  if (!is.numeric(x)) stop("x debe ser numérico (grados y minutos decimales)")
  
  mins <- x - trunc(x)
  bad  <- mins > 0.60
  
  if (any(bad))
    warning("Hay valores con minutos > .60; se ponen a NA")
  
  out <- trunc(x) + mins * 100 / 60
  out[bad] <- NA_real_
  
  out
}
