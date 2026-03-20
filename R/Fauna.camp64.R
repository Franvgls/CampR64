#' Listado de especies por campaña sin  datos de las especies del grupo, sólo nombres permite añadir los lances especiales
#'
#' @details Muestra un listado de todas las especies capturadas del grupo gr. se puede utilizar para sacar listado de todos los lances o sólo de los estándares. Si hay especies con código desonocido aparecen al final del listado#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros.
#' @param camp Campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 si T incluye los lances especiales con listado de especies en los lances especiales y en los estándar
#' @param getaphia si T busca en internet (debe estar disponible) los códigos AphiaID de las especies en cuestión
#' @param verbose si T saca en pantalla, si F no salen en pantalla
#' @return Devuelve un data.frame con el listado de taxones (especies) del grupo gr encontradas en la campaña
#' @examples Fauna.camp(gr=1,camp="N12",dns="Cant",excl.sect=NA,getaphia=TRUE)
#' @family ListadosFauna
#' @export
Fauna.camp<- function(gr="1",camp,zona="cant",dns=c("local","serv"),excl.sect=NA,incl2=TRUE,verbose=FALSE,getaphia=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(gr)>1 | gr==9) {stop("no se pueden mezclar grupos en esta función")}
  listsps<-readCampDBF("fauna",zona,camp,dns)
  listsps<-listsps[listsps$grupo==gr,c("esp","lance")]
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2==incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector","validez")]
  lan$lance<-format(lan$lance,width=3,justify = "r")
  dumb<-merge(listsps,lan)
  if (any(!is.na(excl.sect))) {
    dumb$sector<-gsub("NA","N",dumb$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
    dumb$sector<-factor(as.character(dumb$sector))
  }
  listaesp<-unique(dumb$esp)
  ndat<-length(listaesp)
  # dumbtap<-tapply(dumb$esp,dumb[,c("esp","validez")],length)
  dumbres<-data.frame(gr=NA,esp=NA,especie=NA,AphiaID=NA)
  for (i in 1:ndat) {
    tempesp<-buscaesp64(gr,listaesp[i],zona,dns)
    dumbres<-rbind(dumbres,data.frame(gr=gr,esp=listaesp[i],especie=tempesp,AphiaID=NA)                                )
  }
  dumbres$especie<-as.character(dumbres$especie)
  if (any(!is.na(excl.sect))) print(paste("Excluidos los sectores/estratos",excl.sect))
  if (getaphia) {
    dumbresa<-dplyr::filter(dumbres,especie=="ERROR CODIGO DESCONOCIDO")
    dumbres<-dplyr::filter(dumbres,especie!="ERROR CODIGO DESCONOCIDO")
    for (i in 1:nrow(dumbres)) dumbres$AphiaID[i]<- worrms::wm_name2id(dumbres$especie[i])
    rbind(dumbres[order(as.character(dumbres$especie,decreasing=FALSE)),],dumbresa)
    } 
  else dumbres[order(as.character(dumbres$especie,decreasing=F)),c("gr","esp","especie")]#dumbres<-rbind(dumbres,dumbresa)
  }
