#' Datos de biomasa y abundancia para una especie y campaña
#'
#' Función de acceso a datos:
#' Extrae los datos de peso y número de una especie o conjunto de especies a partir de las faunísticas de una campaña
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @return Devuelve un data.frame con información del lance, latitud, longitud, profundidad, peso (gramos) y numero 
#' @examples datgr.camp(1,50,"P10","Porc",cor.time=TRUE,incl2=FALSE)
#' @export
datgr.camp64<- function(gr,esp,camp,zona,dns="local",cor.time=TRUE,incl2=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  #esp<-format(esp,width=3,justify="r")
  # ch1<-RODBC::odbcConnect(dsn=dns)
  # RODBC::odbcSetAutoCommit(ch1, FALSE)
  fauna<-readCampDBF("fauna",zona,camp,dns)  #fauna<-RODBC::sqlFetch(ch1,paste("FAUNA",camp,sep=""))
  fauna$esp<-as.numeric(as.character(fauna$esp))
  if (length(esp)==1) {
    if (gr!="9" & esp!="999") {
      absp<-fauna[fauna$grupo==as.integer(gr) & fauna$esp==as.integer(esp),c(1,4:5)] }
    if (gr!="9" & esp=="999") {
      absp<-fauna[fauna$grupo==as.integer(gr),c(1,4:5)] }
    if (gr=="9" & esp=="999") {
      absp<-fauna[fauna$grupo!="6",c(1,4:5)] }
  }
  else {
    absp<-fauna[fauna$grupo==gr & fauna$esp %in% as.integer(esp),c(1,4:5)]
  }
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,incl2=incl2)
  #RODBC::odbcClose(ch1)
  if (length(lan)==1) {
    mm<-data.frame(lan=0,lat=0,long=0,prof=0,numero=0,peso.gr=0)
  }
  else {
    names(absp)<-gsub("_",".",tolower(names(absp)))
    lan<-lan[,c("lance","lat","long","prof","weight.time")]
    names(lan)<-c("lan","lat","long","prof","weight.time")
    if (any((gr=="9" | esp=="999" | length(esp)>1))) {
      absp<-data.frame(lance=names(tapply(absp$peso.gr,absp$lance,sum)),peso.gr=tapply(absp$peso.gr,absp$lance,sum),
                       numero=tapply(absp$numero,absp$lance,sum)) }
    if (nrow(absp)==0) {absp<-data.frame(lance=lan$lan,peso.gr=0,numero=0)}
    mm<-try(merge(lan,absp,by.x="lan",by.y="lance",all.x=TRUE),silent=TRUE)
    #		browser()
    if (!identical(as.numeric(which(is.na(mm[,7]))),numeric(0))) {
      mm[which(is.na(mm[,6])),6:7]<-0
    }
  if (any(cor.time,camp=="N83",camp=="N84")) {
    if (any(mm$weight.time==0)) {
      mm$weight.time[mm$weight.time==0]=.1
      warning("Hay lances con duración 0 minutos, revisa validez")
    }
    mm$peso.gr<-mm$peso.gr/mm$weight.time
    mm$numero<-round(mm$numero/mm$weight.time,0)
    }
  mm<-mm[,c("lan","lat","long","prof","peso.gr","numero")]
  }
  mm
}