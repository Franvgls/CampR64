#' Capturas medias por lance de cada especie de uno o varios grupos en una o varias campañas
#'
#' Muestra un listado de la biomasa y número medio de todas las especies capturadas de los grupos gr en una campaña o en una serie de campañas. Sustituye también a \code{ListFauna.camps} y \code{ListFauna.groups} de CampR.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. Admite varios grupos, i.e. c(1,2), y 9 para todos los grupos del 1 al 5 (como ListFauna.groups)
#' @param camp Campaña o vector de campañas de las que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX". Admite una serie, i.e. Nsh[25:27]
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa". 
#' @param dns Elige si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 Si T incluye los lances especiales "2"
#' @param kg No se usa, se mantiene por compatibilidad
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr. Columnas: camp (sólo si se piden varias campañas),gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp). Se ordena por campaña (en el orden dado en camp), grupo y peso decreciente. Los valores NaN en las abundancias corresponden a especies que sólo han aparecido en los lances especiales, y que no puede calcularse la abundancia estratificada al no contar con áreas para los estratos en que aparecen
#' @examples
#' \dontrun{
#' ListFauna.camp64(gr=1,camp="N12",zona="cant",dns="local",excl.sect=NA,incl2=FALSE)
#' ListFauna.camp64(gr=1,camp=Nsh[25:27],zona="cant",dns="local")
#' ListFauna.camp64(gr=9,camp="N25",zona="cant",dns="local")
#' }
#' @family ListadosFauna
#' @export
ListFauna.camp64<- function(gr="1",camp,zona,dns=c("local","serv"),cor.time=TRUE,excl.sect=NA,incl2=FALSE,kg=TRUE) {
  if (any(gr==9)) gr<-1:5
  dns<-match.arg(dns)
  if (length(camp)>1) {
    dumb<-do.call(rbind,lapply(camp,function(x) {
      res<-ListFauna.camp64(gr,x,zona,dns,cor.time=cor.time,excl.sect=excl.sect,incl2=incl2,kg=kg)
      if (nrow(res)==0) return(NULL)
      cbind(camp=x,res)
    }))
    dumb<-dumb[order(match(dumb$camp,camp),as.numeric(dumb$gr),-dumb$peso),]
    rownames(dumb)<-NULL
    return(dumb)
  }
  if (length(gr)>1) {
    dumb<-do.call(rbind,lapply(gr,function(x) {
      res<-ListFauna.camp64(x,camp,zona,dns,cor.time=cor.time,excl.sect=excl.sect,incl2=incl2,kg=kg)
      if (nrow(res)==0) return(NULL)
      res
    }))
    if (is.null(dumb)) return(data.frame())
    dumb<-dumb[order(as.numeric(dumb$gr),-dumb$peso),]
    rownames(dumb)<-NULL
    return(dumb)
  }
  listsps<-readCampDBF("fauna",zona,camp,dns)
  listsps<-listsps[listsps$grupo==gr,c("esp","lance","peso_gr","numero")]
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector","validez","arsect","weight.time")]
  dumb<-merge(listsps,lan)
  if (any(!is.na(excl.sect))) {
    dumb$sector<-gsub("NA","N",dumb$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
    dumb$sector<-factor(as.character(dumb$sector))
  }
  listaesp<-levels(factor(dumb$esp))
  ndat<-length(listaesp)
  dumbtap<-tapply(dumb$esp,dumb$esp,length)
  dumbres<-data.frame(gr=NULL,esp=NULL,especie=NULL,peso=NULL,numero=NULL,nlan=NULL)
  for (i in seq_len(ndat)) {
    dumbmedio <- CV.camps64(gr=gr, esp=listaesp[i], 
                            camps=camp,              # ← fix
                            zona=zona, dns=dns, 
                            excl.sect=excl.sect, 
                            cor.time=cor.time)
    dumbres <- rbind(dumbres, 
                     data.frame(
                       gr      = gr,
                       esp     = listaesp[i],
                       especie = buscaesp64(gr, listaesp[i], zona, dns),
                       peso    = dumbmedio$weight,
                       numero  = dumbmedio$number,
                       nlan    = dumbtap[as.vector(dimnames(dumbtap)[[1]]) == listaesp[i]]
                     ))
  }
  for (col in c("peso","numero","nlan")) {
    dumbres[, col] <- as.numeric(as.character(dumbres[, col]))
  }
  dumbres[order(as.numeric(as.character(dumbres[,4])),decreasing=TRUE),]
}
