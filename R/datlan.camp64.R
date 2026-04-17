#' Características del lance
#'
#' Función de acceso a datos:
#' Extrae las características de los lances para una campaña determinada
#'
#' Un problema que ocurre al utilizar el CampR con ficheros dbf de las primeras campañas
#' puede ser que al fichero lanceXXX.dbf le falte algún campo, habitualmente
#' el campo **ESTN** utilizado en las últimas versiones del **CAMP** para ligar lances con las estaciones de CTD.
#' El error usual es **$ operator is invalid for atomic vectors**
#' Si se detecta este error revisar la estructura de lanceXXX.dbf con la de
#' otros ficheros de lances de los últimos años
#'
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las odbcs
#' @param dns Elige de dónde se leen los datos, "local" para el ordenador, "serv" para el servidor
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param incl0 Si T se incluyen los lances nulos
#' @param outhidro si T saca los datos del fichero hidro al final de todo el proceso como salida
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param redux Si T elimina datos de longitud y latitud de virada y muestra la media de las profundidades de largada y virada
#' @param year si T incluye una columna con el año al final de los datos
#' @param quarter si T incluye una columna con el trimestre de los datos teniendo en cuenta la fecha del lance, puede cambiar a mitad de la campaña, cuidado con campañas IBTS adscritas a un trimestre particular.
#' @param bio reduce el data.frame a los datos para los proyectos de biología, con datos en formato decimal y hexadecimal y las zonas ICES
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux. En cualquier caso incluye variables weight.time con el factor de calibración para lances con menos tiempo del estándar y arsect: el área del sector al que corresponde el lance dentro del muestreo
#' @seealso {\link{MapLansGPS}}
#' @examples
#'   print(datlan.camp64(Nsh[24:28],"cant","local",hidro=FALSE,excl.sect=c("A")))
#'   print(datlan.camp64("P16","porc","local",bio=T))
#' @export
datlan.camp64<-function(camp,zona,dns=c("local","serv"),incl2=TRUE,incl0=FALSE,excl.sect=NA,redux=FALSE,year=TRUE,quarter=TRUE,bio=FALSE) {
  lan<-readCampDBF("lance",zona,camp[1],dns)
  dumb<-readCampDBF("camp",zona,camp[1],dns)
  names(lan)<-tolower(names(lan))
  lan<- lan[,c("lance","validez","latitud_l","latitud_v","longitud_l","longitud_v","prof_l","prof_v","velocidad",
                            "sector","estrato","cable","malletas","dista_p","abert_h","abert_v","recorrido","fecha","ewl","ewv","nsl","nsv","cuadricula","hora_l","hora_v","rumbo","dir_viento",
                                    "vel_viento","est_mar","temp","sali","estn","arte")]
  lan$camp<-camp[1]
  lan$haul.mins<-dumb$durlan
  lan$barco<-dumb$barco
  lan$sector<-paste0(lan$sector,toupper(lan$estrato))
  if(quarter==T) lan$quarter=substr(quarters(as.Date(lan$fecha)),2,2)
  if(year==T) lan$year=lubridate::year(lan$fecha)

  # Función auxiliar: convierte hora numérica DBF (H.MM) a "HH:MM"
  hora_fmt <- function(x) {
    h <- trunc(x)
    m <- round((x - h) * 100)
    sprintf("%02d:%02d", h, m)
  }

  foop<-function(camp,dns,incl2=incl2,incl0=incl0) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    lan<-readCampDBF("lance",zona,camp,dns)
    names(lan)<-tolower(names(lan))
    lan<- lan[,c("lance","validez","latitud_l","latitud_v","longitud_l","longitud_v","prof_l","prof_v","velocidad",
                 "sector","estrato","cable","malletas","dista_p","abert_h","abert_v","recorrido","fecha","ewl","ewv","nsl","nsv","cuadricula","hora_l","hora_v","rumbo","dir_viento",
                 "vel_viento","est_mar","temp","sali","estn","arte")]
    lan$lance<-as.integer(lan$lance)
    lan$validez<-as.integer(lan$validez)
    lan$sector<-as.integer(lan$sector)
    lan$estn<-as.integer(lan$estn)
    lan$arte<-as.integer(lan$arte)
    lan$camp<-camp
    dumb<-readCampDBF("camp",zona,camp,dns)
    lan$haul.mins<-dumb$durlan
    lan$barco<-dumb$barco
    lan$sector<-paste0(lan$sector,lan$estrato)
    area<-as.data.frame(cbind(sector=
                                as.character(substr(names(dumb[,21:45]),2,3)),arsect=as.numeric(t(dumb[,21:45]))))
    area<-area[!is.na(area$arsect),]
    area$arsect<-as.numeric(area$arsect);area$sector<-toupper(area$sector)
    if (any(!lan$nsl %in% c("N","S"))) message(paste("En el lance",lan[!lan$nsl %in% c("N","S"),"lance"],
                                                     "el campo nsl que debe ser N o S y es",lan[!lan$nsl %in% c("N","S"),"nsl"]))
    if (any(!lan$ewl %in% c("W","E"))) message(paste("En la estación",paste(lan[!lan$ewl %in% c("E","W"),"ewl"],collapse = ","),
                                                     "el campo ewl que debe ser E o W y es",lan[!lan$ewl %in% c("N","S"),"ewl"]))
    if (any(!lan$nsv %in% c("N","S"))) message(paste("En el lance",paste(lan[!lan$nsv %in% c("N","S"),"lance"],collapse=","),
                                                     "el campo nsv que debe ser N o S y es",lan[!lan$nsv %in% c("N","S"),"nsv"]))
    if (any(!lan$ewv %in% c("W","E"))) message(paste("En la estación",paste(lan[!lan$ewv %in% c("E","W"),"ewv"],collapse=","),
                                                     "el campo ewv que debe ser E o W y es",lan[!lan$ewv %in% c("N","S"),"ewv"]))
    lan$latitud_l<-round(sapply(lan$latitud_l,gradec)*ifelse(lan$nsl=="N",1,-1),4)
    lan$longitud_l<-round(sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="E",1,-1),4)
    lan$latitud_v<-round(sapply(lan$latitud_v,gradec)*ifelse(lan$nsv=="N",1,-1),4)
    lan$longitud_v<-round(sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="E",1,-1),4)
    lan$lat<-round((lan$latitud_l+lan$latitud_v)/2,4)
    lan$long<-round((lan$longitud_l+lan$longitud_v)/2,4)
    lan$prof<-(lan$prof_l+lan$prof_v)/2
    if (any(is.na(lan$zona))) {message(paste0("Al menos un lance: ",paste(lan$lance[is.na(lan$zona)],collapse = ","),
                                              " sin Zona ICES asignada, revise resultados",lan$camp[is.na(lan$zona)]))}
    # Comprobación de horas (hora_l/hora_v son numéricos H.MM en este punto)
    if (any(lan$hora_l > lan$hora_v)) {
      message(paste0("Al menos un lance ",
                     paste(lan[lan$hora_l > lan$hora_v, "lance"], collapse = ","),
                     " con hora de virada antes de hora de largada"))
    }
    if (any(is.na(lan$hora_l) | lan$hora_l < 0 | lan$hora_l >= 24)) {
      message(paste0("Al menos una hora de largada (lance: ",
                     paste(lan[is.na(lan$hora_l) | lan$hora_l < 0 | lan$hora_l >= 24, "lance"], collapse=","),
                     ") con hora inválida"))
    }
    if (any(is.na(lan$hora_v) | lan$hora_v < 0 | lan$hora_v >= 24)) {
      message(paste0("Al menos una hora de virada (lance: ",
                     paste(lan[is.na(lan$hora_v) | lan$hora_v < 0 | lan$hora_v >= 24, "lance"], collapse=","),
                     ") con hora inválida"))
    }
    lan$dista_p[lan$dista_p==0]<-NA
    lan$abert_v[lan$abert_v==0]<-NA
    lan$abert_h[lan$abert_h==0]<-NA
    lan$sali[lan$sali==0]<-NA
    lan$temp[lan$temp==0]<-NA
    lan$fecha<-as.Date(ifelse(lan$fecha < "1980-12-31", format(lan$fecha, "20%y-%m-%d"), format(lan$fecha)))
    # weight.time: cálculo con hora numérica (H.MM) antes de convertir a texto
    lan$weight.time<-ifelse(lan$haul.mins==60,1,2)*((trunc(lan$hora_v)+((lan$hora_v-trunc(lan$hora_v))/.6))-(trunc(lan$hora_l)+((lan$hora_l-trunc(lan$hora_l))/.6)))
    lan$weight.time<-round(lan$weight.time,3)
    # Convertir horas a formato "HH:MM" (robusto para Excel y exportación)
    lan$hora_l <- hora_fmt(lan$hora_l)
    lan$hora_v <- hora_fmt(lan$hora_v)
    longlab=c(paste0("A",0:3),paste0(rep(c("B","C","D","E","F","G","H","J","K","L"),each=10),0:9),paste0("M",0:8))
    lan$rectlong<-cut(lan$long,breaks=seq(from=-44,to=69,by=1),labels=longlab) # ,"D9","D8"
    lan$rectlat<-cut(lan$lat,breaks=seq(from=36.0,to=85,by=.5),labels=sprintf("%02d", 1:98))
    lan$StatRec<-paste0(lan$rectlat,lan$rectlong)
    lan$zona<-Area[match(lan$StatRec,Area$ICESNAME),"Area_27"]
    lan$zona<-gsub("\\.","",lan$zona)
    for (i in 1:nrow(lan)) {
      if (zona=="Medi" & lan$lat[i]>35.8 & lan$long[i]>c(-5.6556)) {
        lan$zona[i]<-"wm.37.1"
        lan$StatRec<-NA
        }
    }
    lan$StatRec <- ifelse(
      substr(lan$rectlong,1,1) == "E",
      paste0("#", lan$rectlat, lan$rectlong),
      paste0(lan$rectlat, lan$rectlong)
    )
    lan$rectlat <- NULL
    lan$rectlong <- NULL
    if(quarter==T) lan$quarter=substr(quarters(as.Date(lan$fecha)),2,2)
    if(year==T) lan$year=lubridate::year(lan$fecha)
    if (!incl0) {lan<-lan[c(lan$validez!=0),]}
    if (!incl2) {lan<-lan[c(as.numeric(lan$validez)<=1),]}
    datos<-dplyr::left_join(lan,area,by="sector",na_matches = "never")
    datos$arsect<-as.numeric(as.character(datos$arsect))
    datos[order(datos$lance),]
  }
  datos<-data.frame(camp=camp[1],foop(camp[1],dns=dns,incl2=incl2,incl0=incl0))
  if (length(camp)>1) {
    for (i in camp[2:length(camp)]) datos<-dplyr::bind_rows(datos,data.frame(foop(i,dns=dns,incl2=incl2,incl0=incl0),camp=i))
  }
  if (any(is.na(datos$zona))) {message(paste0("Al menos un lance: ",datos$lance[is.na(datos$zona)],
                                              " sin Zona ICES asignada, revise resultados"))}
  if (any(!is.na(excl.sect))) {
    datos$sector<-gsub("NA","N",datos$sector)
    for (i in 1:length(excl.sect)) {if (length(grep(excl.sect[i],as.character(datos$sector))>0)) datos<-datos[-grep(excl.sect[i],as.character(datos$sector)),]}
  }
  datos$sector<-as.character(datos$sector)
  dplyr::arrange(datos,year,lance)
  if (redux) {
    datos<-dplyr::select(datos,-c("longitud_v","longitud_l","latitud_v","latitud_l","prof_v","prof_l"))
    datos<-dplyr::relocate(datos,c("camp","lance","validez","lat","long","prof"))
  }
  if (!redux & !bio) {
    datos<-dplyr::relocate(datos,c("camp","lance","validez"))
  }
  if (bio) datos<-datos[,c("camp","lance","sector","validez","lat","long","prof","estrato","fecha","zona")]
  if (!is.null(datos$camp.1)) {datos<-dplyr::select(datos,-camp.1)}
  return(datos)
}
