# R/shapes_utils.R

#' Ruta a un shape interno de CampR64
#'
#' @param type Tipo: "ices", "cant", "porc", "arsa", "iberia", "bathy"
#' @param file Nombre del fichero (con extension .gpkg)
#' @return Ruta completa al fichero
#' @export
get_shape_path <- function(type, file) {
  path <- system.file("shapes", type, file, package = "CampR64")
  if (path == "")
    stop("Shape no encontrado: shapes/", type, "/", file,
         "\nUsa list_shapes() para ver los disponibles.")
  path
}

#' Leer un shape interno de CampR64 como objeto sf
#'
#' @param type Tipo: "ices", "cant", "porc", "arsa", "iberia", "bathy"
#' @param file Nombre del fichero (con extension .gpkg)
#' @param quiet Suprimir mensajes (default TRUE)
#' @return Objeto sf
#' @export
read_shape <- function(type, file, quiet = TRUE) {
  sf::st_read(get_shape_path(type, file), quiet = quiet)
}

#' Listar shapes disponibles en CampR64
#'
#' @return Data.frame con columnas type y file
#' @export
list_shapes <- function() {
  base <- system.file("shapes", package = "CampR64")
  if (base == "") stop("Paquete CampR64 no instalado correctamente.")
  files <- list.files(base, recursive = TRUE,
                      full.names = FALSE, pattern = "\\.gpkg$")
  parts <- strsplit(files, "/")
  data.frame(
    type = sapply(parts, `[`, 1),
    file = sapply(parts, `[`, 2),
    stringsAsFactors = FALSE
  )
}