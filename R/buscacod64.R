#' Buscar códigos de grupo, especie y familia
#'
#' @export
buscacod64 <- function(nomb, dns = "Camp", buscfam = FALSE) {
  
  if (length(nomb) > 1)
    stop("Esta función no permite más de una especie por vez")
  
  fichero_especies <- get_camp_file("ESPECIES.DBF", dns)
  
  if (!file.exists(fichero_especies))
    stop("No encuentro el fichero de especies en: ", fichero_especies)
  
  especies <- data.table::as.data.table(leer_dbf(fichero_especies))
  
  if (buscfam) {
    idx <- suppressWarnings(
      grep(nomb, paste(especies$ESPECIE, especies$FAMILIA), ignore.case = TRUE)
    )
  } else {
    idx <- suppressWarnings(
      grep(nomb, especies$ESPECIE, ignore.case = TRUE)
    )
  }
  
  if (length(idx) == 0) {
    message("No se encontraron coincidencias para: ", nomb)
    return(especies[0, c("GRUPO", "ESP", "ESPECIE", "FAMILIA"), with = FALSE])
  }
  
  especies[idx, c("GRUPO", "ESP", "ESPECIE", "FAMILIA"), with = FALSE]
}
