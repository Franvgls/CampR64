#' Capturas medias por lance de cada especie de gr (grupo) en una campaña 
#'
#' Muestra un listado de la biomasa y número medio de todas las especies capturadas del grupo gr 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros.
#' @param camp Campaña de la que se extrae el listado de especies capturadas: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa". 
#' @param dns Elige si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 Si T incluye los lances especiales "2"
#' @return Devuelve un data.frame con las capturas medias por lance de cada especie capturada del grupo gr. Columnas: gr,esp,especie,peso(kg),número,nlan (nº de lances en que ha aparecido esp) Los valores NaN en las abundancias corresponden a especies que sólo han aparecido en los lances especiales, y que no puede calcularse la abundancia estratificada al no contar con áreas para los estratos en que aparecen
#' @examples ListFauna.camp64(gr=1,camp="N12",zona="cant",dns="local",excl.sect=FALSE,incl2=FALSE)
#' @family ListadosFauna
#' @export
ListFauna.camp64<- function(gr="1",camp,zona,dns=c("local","serv"),cor.time=TRUE,excl.sect=NA,incl2=FALSE,kg=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(gr)>1 | gr==9) {stop("no se pueden mezclar grupos en esta función")}
  listsps<-readCampDBF("fauna",zona,camp,dns)
  listsps<-listsps[listsps$grupo==gr,c("esp","lance","peso_gr","numero")]
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2==incl2,incl0=FALSE)
  #listsps<-RODBC::sqlQuery(ch1,paste("select esp,lance,peso_gr,numero from FAUNA",camp," where grupo='",gr,"'",sep=""))
  #lan<-datlan.camp(camp,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2,incl0=FALSE)
  lan<-lan[,c("lance","sector","validez","arsect","weight.time")]
  dumb<-merge(listsps,lan)
  #browser()
  if (any(!is.na(excl.sect))) {
    dumb$sector<-gsub("NA","N",dumb$sector) # print(datos)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
    dumb$sector<-factor(as.character(dumb$sector))
  }
  # str(listsps)
  listaesp<-levels(factor(dumb$esp))
  ndat<-length(listaesp)
  #print(ndat)
  dumbtap<-tapply(dumb$esp,dumb$esp,length)
  dumbres<-data.frame(gr=NULL,esp=NULL,especie=NULL,peso=NULL,numero=NULL,nlan=NULL)
  #browser()
  for (i in 1:ndat) {
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
