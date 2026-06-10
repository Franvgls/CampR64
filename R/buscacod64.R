#' Función de búsqueda del código del grupo y la familia
#'
#' Busca el código del grupo, especie y la familia a partir del nombre o fragmento de la especie. Es independiente de mayúsculas y minúsculas y puede sacar más de una especie si una parte de sus nombres coincide.
#' @param nomb Nombre científico de la especie o fragmento del nombre entre ""
#' @param zona Zona de la que se leen las especies. Determina la estructura de la tabla de especies, que difiere entre ARSA y el resto (Porcupine/Cantábrico): "cant", "porc", "arsa", "medi"
#' @param dns Origen de las bases de datos: "local" en el ordenador, "serv" en el servidor
#' @family datos_especies
#' @examples 
#' \dontrun{
#' buscacod64("sph","cant","local")
#' }
#' @export
buscacod64<- function(nomb,zona="cant",dns=c("local","serv")) {
  if (length(nomb)>1) stop("Esta función no permite más de una especie por vez")
  else especies<-readCampDBF("especies",zona,dns)
  print(especies[grep(nomb,especies$especie,ignore.case = T,perl = T),c("grupo","esp","especie","familia","aphia")])
  }