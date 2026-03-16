#' Datos de abundancia y biomasa de una especie 
#'
#' Función de acceso a datos: 
#' Extrae los datos de abundancia y biomasa estratificados (solo lances con validez 1) de una especie o conjunto de especies a partir de las faunísticas de una campaña. Crea una tabla con información del sector(número) y estrato (letra), lance, peso, número y arsect
#' Funciones para obtener data.frame de especies concretas a partir de los ficheros del Camp
#' @param gr grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T datos en kgs, si F en gramos 
#' @param verbose si T avisa de que hay más de una especie y los datos mezclados pueden ser engañosos
#' @return Devuelve un data.frame con los datos de la especie por lance: sector,lance,peso,numero,arsect (área del sector al que pertence el lance)
#' @details Saca los datos de los lances estratificados, por ello se produce un error si encuentra un lance con validez 1 y estrato o sector sin información.
#' @examples datos.camp(1,50,"P10","Porc",kg=FALSE, cor.time=TRUE)
#' @export
datos.camp64<-function(gr,esp,camp,zona,dns="local",cor.time=TRUE,kg=TRUE,verbose=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  fauna<-readCampDBF("fauna",zona,camp,dns)
  if (length(esp)==1) {
    if (gr!="9" & esp!="999") {
      absp<-fauna[fauna$grupo==as.integer(gr) & fauna$esp==as.integer(esp),c(1,4:5)] }
    if (gr!="9" & esp=="999") {
      absp<-fauna[fauna$grupo==as.integer(gr),c(1,4:5)] }
    if (gr=="9" & esp=="999") {
      absp<-fauna[fauna$grupo!=6,c(1,4:5)] }
  }
  else {
    absp<-fauna[fauna$grupo==gr & fauna$esp %in% as.integer(esp),c(1,4:5)]
  }
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,incl2=FALSE,incl0=FALSE)
  if (any(is.na(lan$sector) | is.na(lan$estrato))) stop(paste("Lances con validez 1 fuera estratificación en campaña: ",camp,". Revise: lance",camp,".dbf lance: ",lan[is.na(lan$estrato),"lance"],sep=""))
  lan<-lan[,c("lance","sector","weight.time")]
  names(absp)<-gsub("_",".",tolower(names(absp)))
  if (any((gr=="9" | esp=="999" | length(esp)>1))) {
    absp<-data.frame(lance=names(tapply(absp$peso.gr,absp$lance,sum)),peso.gr=tapply(absp$peso.gr,absp$lance,sum),
                     numero=tapply(absp$numero,absp$lance,sum)) }
  absp$lance<-as.numeric(as.character(absp$lance))
  area<-NULL
  dumb<-readCampDBF("camp",zona,camp[1],dns)
  # for (i in 21:45) {
  #   area<-paste(area,dumb[i],sep=",")
  # }
  # area<-substr(area,2,nchar(area))
  area<-dumb[,21:45]    #RODBC::sqlQuery(ch1,paste("select ",area," from CAMP",camp,sep=""))
  #browser()
  if ((sum(is.na(area))/length(area))==1) {
    stop(paste("El fichero",paste("camp",camp,".dbf,",sep=""),"está vacío, define la estratificación de la campaña",camp))
  }
  #browser()
  if (zona=="Cant" & (sum(is.na(area))/length(area))>.4) {
    warning("Muchos NAs en fichero camp",camp,", está definida la campaña ",camp,"? Revisar antes de aceptar el resultado")
  }
  area<-area[-which(is.na(area) | area==0)]
  area<-as.data.frame(cbind(substr(names(area),2,3),as.numeric(t(area))))
  names(area)<-c("sector","arsect")
  area$sector<-toupper(area$sector)
  #RODBC::odbcClose(ch1)
  names(lan)<-c("lance","sector","weight.time")
  names(absp)<-c("lance","peso","numero")
  especial<-sum(absp$peso,na.rm=T)
  if (kg) { absp$peso<-absp$peso/1000 }
  mm<-merge(lan,absp,by.x="lance",by.y="lance",all.x=TRUE)
  mm$numero[which(is.na(mm$numero))]<-0
  mm$peso[which(is.na(mm$peso))]<-0
  if (any(cor.time,camp=="N83",camp=="N84")) {
    if (any(mm$weight.time==0)) {
      mm$weight.time[mm$weight.time==0]=.1
      warning("Hay lances con duración 0 minutos, revisa validez")
    }
    mm$peso<-mm$peso/mm$weight.time
    mm$numero<-mm$numero/mm$weight.time
  }
#  mm<-mm[,-3]
  datos<-merge(mm,area,by.x="sector",by.y="sector")
  datos$arsect<-as.numeric(as.character(datos$arsect))
  #browser()
  if (especial>0 & sum(mm$numero)==0) {message(paste("campaña ",camp,","," capturas en lances especiales pero no en los lances válidos estandarizados",sep="")) }
  if (length(esp)>1 & verbose) {print(c("Códigos de especie: ",esp))}
  #print(lan[order(lan$lance),])
  datos[order(datos$lance),]
}
