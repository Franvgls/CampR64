#' Capturas de un lance específico
#'
#' Muestra un listado de las especies capturadas en peso y número en un lance concreto. Se pueden seleccionar grupos de especies
#' @param camp Campaña de la que se extrae los datos del lance (lan) un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" 
#' @param dns Elige de dónde se toman los datos si del ordenador ("local") o del servidor ("serv")
#' @param lan Lance del que se extrae la faunística
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. Si NA, muestra todos los grupos a excepción de 6 desechos y otros no orgánicos
#' @return Devuelve un data.frame con columnas grupo,esp,especie,peso,numero
#' @seealso {\link{ListFauna.camps}}, {\link{ListFaunaTals.camps}}
#' @examples ListFauna.lan64("12C","cant","local",1,gr=c(1,2))
#' @export
ListFauna.lan64<- function(camp,zona="cant",dns=c("local","serv"),lan,gr=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  # ch1<-RODBC::odbcConnect(dsn=dns)
  # RODBC::odbcSetAutoCommit(ch1, FALSE)
  #lan<-format(lan,width=3,justify="r")
  listsps<-readCampDBF("fauna",zona,camp,dns)
  listsps<-listsps[listsps$lance==lan,]
  #listsps<-RODBC::sqlQuery(ch1,paste("select grupo,esp,lance,peso_gr,numero from FAUNA",camp," where lance='",lan,"'",sep=""))
  if (nrow(listsps)==0) {stop(paste("Sin capturas en el lance",lan))}
  if (any(!is.na(gr))) listsps<-listsps[listsps$grupo %in% gr,]
  if (nrow(listsps)==0) {stop(paste("Sin capturas del grupo",gr," en el lance",lan))}
  listsps$especie=NA
  listsps$peso=listsps$peso_gr/1000
  for (i in 1:nrow(listsps)) {
    listsps$especie[i]=buscaesp64(listsps$grupo[i],listsps$esp[i],zona,dns) }
  listsps[order(-as.numeric(listsps$grupo),listsps$peso_gr,decreasing=TRUE),c(1,2,6,7,5,4)]
}