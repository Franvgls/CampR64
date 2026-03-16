#' Crea datos de capturas en formato para evaluaciones tipo Capros o modelos bayesianos (completar con datTalCatch.camp)
#'
#' Function para geographical - bayesian models
#' @param gr Grupo, 1 peces, 2 crustaceos...
#' @param esp Código de especie
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param cor.time Corrección del tiempo de arrastre al calcular las abundancias (mantener en T, *da datos por media hora de lance*)
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @return A data.table con el formato de datos para otras especies con formato de geográfico por lances
#' @examples # datCatches.camp(1,18,"N23","Cant")
#' @examples # datCatches.camp(1,18,"P23","Porc")
#' @export
datCatches.camp64<-function(gr,esp,camp,zona="cant",dns="local",cor.time=TRUE,incl2=FALSE) {
  datesp<-datgr.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,incl2=incl2)
  datlan<-datlan.camp64(camp,zona,dns,incl2=incl2)
  datlan<-dplyr::rename(datlan,lan=lance)
  datlan<-dplyr::select(datlan,camp,year,quarter,lan,lat,long,zona,StatRec,prof)
  DB<-merge(datlan,datesp)
  DB$camp<-ifelse(zona=="cant","SpanNGFS","SpanPorc")
  DB<- dplyr::rename(DB,Survey = camp, DateYr = year,Quarter=quarter,HaulNb=lan,latdec=lat,longdec=long,SubDiv=zona,
                     StatRec=StatRec,Depth=prof,N30=numero,Kg30=peso.gr)
  #if (dns=="Cant") {DB$StatRec<- paste0("#",sub(" ","",DB$StatRec),"#")}
  DB<-dplyr::select(DB,Survey,DateYr,Quarter,HaulNb,latdec,longdec,SubDiv,StatRec,Depth,N30,Kg30)
  DB$Kg30<-DB$Kg30/1000
  DB
}
