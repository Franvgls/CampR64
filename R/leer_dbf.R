#' Leer un archivo DBF con conversión de codificación (CP850 → UTF-8)
#'
#' Lee un archivo DBF de dBase III/Clipper y convierte automáticamente
#' todos los campos de texto desde CP850 (OEM/MS-DOS) a UTF-8.
#'
#' @param ruta Ruta completa al archivo .DBF.
#'
#' @return Un data.frame con los datos del DBF y texto en UTF-8.
#'
#' @import foreign
#' @export
leer_dbf <- function(ruta) {
  
  df <- foreign::read.dbf(ruta, as.is = TRUE)
  
  df[] <- lapply(df, function(x) {
    if (is.character(x)) iconv(x, from = "CP850", to = "UTF-8", sub = "")
    else x
  })
  
  df
}
