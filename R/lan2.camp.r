#' Lances especiales de una campaña
#'
#' Busca y devuelve los lances especiales de una campaña 
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" 
#' @param dns elige el origen de los datos, si del ordenador ("local") o del servidor ("serv") si está habilitado 
#' @examples lan2.camp(camp="P01",zona="porc",serv="local")
#' @export
lan2.camp<-function(camp,zona="cant",dns=c("local","serv")) {
  if (length(camp)>1) stop("Sólo se puede seleccionar una campaña, proces individualmente")
  datlan.camp64(camp,zona,dns)[datlan.camp64(camp,zona,dns)[,3]==2,"lance"]
}
