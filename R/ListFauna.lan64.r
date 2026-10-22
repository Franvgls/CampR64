#' Capturas de uno, varios o todos los lances de una campaña
#'
#' Muestra un listado de las especies capturadas en peso y número en un lance concreto, en varios o en todos los lances de la campaña. Se pueden seleccionar grupos de especies. Sustituye también a \code{ListFauna.lans} de CampR: con \code{lan=NA} saca todos los lances de la campaña.
#' @param camp Campaña de la que se extrae los datos del lance (lan) un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige de dónde se toman los datos si del ordenador ("local") o del servidor ("serv")
#' @param lan Lance o vector de lances de los que se extrae la faunística. Si NA (por defecto) se sacan todos los lances de la campaña, teniendo en cuenta \code{excl.sect} e \code{incl2}
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros no orgánicos. Si NA, muestra todos los grupos
#' @param excl.sect Sólo con \code{lan=NA}: sectores o estratos a excluir como vector de caracteres, i.e. c("A","B"). NA no excluye ninguno
#' @param incl2 Sólo con \code{lan=NA}: si TRUE se incluyen los lances especiales
#' @return Devuelve un data.frame con columnas lance,grupo,esp,especie,peso (kg),numero ordenado por lance, grupo y peso decreciente
#' @family ListadosFauna
#' @examples
#' \dontrun{
#' ListFauna.lan64("N12","cant","local",1,gr=c(1,2))
#' ListFauna.lan64("N12","cant","local",c(1,5,10))
#' ListFauna.lan64("N12","cant","local",gr=1,excl.sect="E")
#' }
#' @export
ListFauna.lan64<- function(camp,zona="cant",dns=c("local","serv"),lan=NA,gr=NA,excl.sect=NA,incl2=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  dns<-match.arg(dns)
  if (all(is.na(lan))) {
    lan<-datlan.camp64(camp,zona,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2)$lance
  }
  listsps<-readCampDBF("fauna",zona,camp,dns)
  listsps<-listsps[listsps$lance %in% lan,]
  if (nrow(listsps)==0) {stop(paste("Sin capturas en el lance",paste(lan,collapse=",")))}
  if (any(!is.na(gr))) listsps<-listsps[listsps$grupo %in% gr,]
  if (nrow(listsps)==0) {stop(paste("Sin capturas del grupo",paste(gr,collapse=","),"en el lance",paste(lan,collapse=",")))}
  sinlan<-setdiff(lan,listsps$lance)
  if (length(lan)>1 & length(sinlan)>0) message("Lances sin capturas",if (any(!is.na(gr))) " del grupo seleccionado",": ",paste(sinlan,collapse=","))
  especies<-readCampDBF("especies",zona,dns=dns)
  listsps$especie<-especies$especie[match(paste(listsps$grupo,listsps$esp),paste(especies$grupo,especies$esp))]
  listsps$especie[is.na(listsps$especie)]<-"ERROR CODIGO DESCONOCIDO"
  listsps$peso<-listsps$peso_gr/1000
  listsps$lance<-as.numeric(listsps$lance)
  listsps<-listsps[order(listsps$lance,as.numeric(listsps$grupo),-listsps$peso_gr),c("lance","grupo","esp","especie","peso","numero")]
  rownames(listsps)<-NULL
  listsps
}
