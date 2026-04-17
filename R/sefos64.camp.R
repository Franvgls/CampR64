#' Crea datos en formato SEFOS a partir de Camp, dns
#'
#' Function to complete HH with ICESrect and area
#' @param gr Grupo de organismos, 1 peces 2 crustaceos, 3 moluscos, 4 equinodermos, 5 inv..
#' @param esp Código de especie camp
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "cant", Golfo de Cádiz "arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param dns Elige de dónde se toman los datos, si del ordenador "local" o del servidor "serv"
#' @param plus Cuál es la edad plus (en el formato SEFOS las opciones son 3+, 5+ o 12+ según el ciclo vital de la especie.
#' @param cor.time por defecto T, corrección del tiempo de arrastre al calcular las abundancias
#' @param mediahora =2, las abundancias tienen que ser por hora, commo el lance dura media hora se pone a dos, en el caso de ARSA debería ser 1
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @param AltAlk ALK alternativa tomada de un fichero de edad del camp edadxyy.dbf sin ruta ni extensión
#' @return A data.table con el formato de datos de SEFOS para la evaluación de caballa
#' @examples sefos64.camp(1,74,"N23","cant","local",plus=3)
#' @export
sefos64.camp<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),plus=8,cor.time=TRUE,AltAlk=NA,incl2=FALSE,mediahora=2) {
  datesp<-maphistage64(gr,esp,camp,zona,dns,0,plus,cor.time=cor.time,AltAlk=AltAlk,incl2=incl2,plot=FALSE,out.dat=T,mediahora = mediahora)
  datlan<-datlan.camp64(camp,zona,dns,incl2=incl2)
  DB<-datlan[,c("camp","lance","latitud_v")] 
  DB$Country<-rep("SP",nrow(datlan))
  DB$CruiseCode<-rep(camp,nrow(datlan))
  DB$camp<-NULL
  DB$lance<-NULL
  DB$latitud_v<-NULL
  DB$DateDay<-lubridate::day(datlan$fecha)
  DB$DateMth<- lubridate::month(datlan$fecha)
  DB$DateYr<-as.numeric(substr(lubridate::year(datlan$fecha),3,4))
  DB$HaulNb<- datlan$lance
  DB$ICESrect<- datlan$StatRec
  DB$LatDgr<- trunc(datlan$lat)
  DB$LatMin<- trunc((datlan$lat-trunc(datlan$lat))*60)
  DB$LonDgr<- abs(trunc(datlan$long))
  DB$LonMin<- trunc((abs(datlan$long)-trunc(abs(datlan$long)))*60)
  DB$EW<-ifelse(datlan$long>0,"E","W")
  DB$time<-datlan$hora_l
  DB$Depth<-datlan$prof
  DB$Temp<-datlan$temp
  DB$Sal<-datlan$sali
  DB<-cbind(DB,datesp[,5:ncol(datesp)])
  DB$Total<-rowSums(datesp[,5:ncol(datesp)])
  return(DB)
  }
