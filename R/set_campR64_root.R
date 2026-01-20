#' Establecer la ruta raíz de los datos para CampR64
#'
#' Permite cambiar dinámicamente entre diferentes ubicaciones de datos,
#' como "C:/camp" o "Z:/camp/datos". La función valida que la ruta existe
#' y que contiene las subcarpetas esperadas (arsa, cant, porc, medits, camp).
#'
#' @param ruta Cadena con la ruta raíz donde se encuentran las carpetas
#'             de datos de CampR64.
#'
#' @return Invisiblemente TRUE si la ruta es válida.
#'
#' @examples
#' \dontrun{
#' set_campR64_root("C:/camp")
#' set_campR64_root("Z:/camp/datos")
#' }
#'
#' @export
set_campR64_root <- function(ruta) {
  
  # 1. Validar existencia de la ruta
  if (!dir.exists(ruta)) {
    stop("La ruta no existe: ", ruta)
  }
  
  # 2. Subcarpetas esperadas
  subcarpetas <- c("arsa", "cant", "porc", "medits", "camp")
  
  faltan <- subcarpetas[!dir.exists(file.path(ruta, subcarpetas))]
  
  if (length(faltan) > 0) {
    warning(
      "La ruta existe pero faltan subcarpetas: ",
      paste(faltan, collapse = ", ")
    )
  }
  
  # 3. Guardar la ruta en las opciones del sistema
  options(campR64.data_root = ruta)
  
  message("CampR64 ahora usará los datos en: ", ruta)
  
  invisible(TRUE)
}
