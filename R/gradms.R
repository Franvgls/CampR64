#' Transforma grados, minutos y segundos a grados decimales
#'
#' Convierte valores expresados como \code{"GGG:MM:SS"} a grados decimales.
#' Avisa si los minutos o segundos son mayores de 60.
#'
#' @param x Vector de caracteres con formato \code{"GGG:MM:SS"}.
#' @examples
#' gradms("43:14:25")
#' @family Conversion unidades
#' @export
gradms <- function(x) {
  
  # Separar G, M, S en columnas
  dms <- do.call(rbind, strsplit(as.character(x), ":"))
  
  # Comprobación de formato correcto
  if (ncol(dms) != 3) {
    stop("El formato debe ser 'GGG:MM:SS'")
  }
  
  # Convertir a numérico
  G <- as.numeric(dms[, 1])
  M <- as.numeric(dms[, 2])
  S <- as.numeric(dms[, 3])
  
  # Validación de minutos y segundos
  if (any(M > 60 | S > 60, na.rm = TRUE)) {
    stop("Los minutos y segundos no pueden ser mayores de 60, revise datos")
  }
  
  # Conversión a grados decimales
  dec <- G + (M + S / 60) / 60
  
  return(dec)
}
