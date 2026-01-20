#' Localizar un archivo DBF dentro de la estructura real de CampR64
#'
#' @param fichero Nombre del archivo DBF (por ejemplo "ESPECIES.DBF").
#' @param dns Origen de datos: "Camp", "Arsa", "Cant", "Porc", "Medits".
#'
#' @export
get_camp_file <- function(fichero, dns = "Camp") {
  
  root <- getOption("campR64.data_root", default = "C:/camp")
  
  dns <- tolower(dns)
  
  subcarpeta <- switch(
    dns,
    "camp"   = "",        # <<--- Camp está en la raíz
    "arsa"   = "arsa",
    "cant"   = "cant",
    "porc"   = "porc",
    "medits" = "medits",
    stop("Origen de datos desconocido: ", dns)
  )
  
  ruta <- if (subcarpeta == "") {
    file.path(root, fichero)
  } else {
    file.path(root, subcarpeta, fichero)
  }
  
  return(ruta)
}
