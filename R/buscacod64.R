#' Función de búsqueda del código del grupo y la familia
#'
#' Busca el código del grupo, especie y la familia a partir del nombre o fragmento de la especie. Es independiente de mayúsculas y minúsculas y puede sacar más de una especie si una parte de sus nombres coincide.
#' @param nomb Nombre científico de la especie o fragmento del nombre entre ""
#' @family datos_especies
#' @examples buscacod("sph")
#' @export
buscacod64<- function(nomb,zona="cant",dns="local") {
  if (length(nomb)>1) stop("Esta función no permite más de una especie por vez")
  else especies<-readCampDBF("especies",zona,dns)
  print(especies[grep(nomb,especies$especie,ignore.case = T,perl = T),c("grupo","esp","especie","familia","aphia")])
  }