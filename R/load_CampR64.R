#' Cargar el paquete CampR64 en modo desarrollo
#'
#' Ejecuta `devtools::load_all()` y muestra un mensaje indicando
#' que el entorno de desarrollo está listo.
#'
#' @return Invisiblemente TRUE.
#'
#' @export
load_CampR64 <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("El paquete 'devtools' no está instalado.")
  }
  
  devtools::load_all()
  message("CampR64 cargado en modo desarrollo.")
  invisible(TRUE)
}
