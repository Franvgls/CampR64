#' Lances nulos en una campaña
#'
#' Busca y devuelve los lances nulos de una campaña 
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant" o Golfo de Cádiz "arsa"
#' @param dns elige de dónde se toman los datos, del ordenador personal ("local") o del servidor ("serv") si está configurado
#' @examples lan0.camp(camp="P01",zona="porc",serv="local")
#' @export
lan0.camp<-function(camp,zona="cant",dns=c("local","serv")) {
  if (length(camp)>1) stop("Sólo se puede seleccionar una campaña, proces individualmente")
  datlan.camp64(camp,zona,dns,incl0=TRUE)[datlan.camp64(camp,zona,dns,incl0=TRUE)[,3]==0,"lance"]
}
 