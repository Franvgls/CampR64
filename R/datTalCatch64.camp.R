#' Crea datos de capturas en formato para evaluaciones tipo Capros o modelos bayesianos (completar con datCatches.camp)
#'
#' Function to create geographical - bayesian models
#' @param gr Grupo, 1 peces, 2 crustaceos...
#' @param esp Código de especie
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona  "cant","porc","arsa","medi".
#' @param dns   "local" (por defecto) o "serv".
#' @param cor.time Corrección del tiempo de arrastre al calcular las abundancias (mantener en T, *da datos por media hora de lance*)
#' @param sex si T incluye la información por sexos, si F no tiene en cuenta la información por sexos y lo trata todo como indeterminados
#' @param incl2 Si T se incluyen los lances extra no incluidos para las abundancias o biomasas estratificadas
#' @return A data.table con el formato de datos para otras especies con formato de geográfico por lances
#' @examples # datCatches.camp64(1,18,"N23","Cant")
#' @examples # datCatches.camp64(1,18,"P23","Porc")
#' @export
datTalCatch64.camp<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),sex=TRUE,cor.time=TRUE,incl2=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  # 1. Detectamos modo
  # use_odbc <- tryCatch(get_camp_mode(), error = function(e) TRUE)
  abesp<-datgr.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,incl2=incl2)
#  datmap<-maphist(gr,esp,camp,dns,out.dat = T,plot=F,cor.time = cor.time)
  abesp$peso<-abesp$peso.gr/1000
  #LanDatr<-getICESarea(camp,dns,incl2 = incl2)
  #LanDatr<-dplyr::arrange(LanDatr,lance)
  result1<- datCatches.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,incl2=incl2)
  ntalls<-readCampDBF("ntall",zona,camp,dns)     
  ntalls<-ntalls[ntalls$grupo==gr & ntalls$esp==esp,c("lance","peso_gr","peso_m","talla","sexo","numer")]
#  DBI::dbGetQuery(ch1,paste0("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"' and esp='",esp,"'"))
  names(ntalls) <- tolower(names(ntalls))
  # Ajuste histórico: a veces peso_gr viene con guion bajo, lo pasamos a punto por compatibilidad
  names(ntalls) <- gsub("_", ".", names(ntalls))
  ntalls$lance<-as.numeric(ntalls$lance)
  dtlan<-datlan.camp64(camp,zona,dns,redux=T)
  if (any(cor.time,camp=="N83",camp=="N84")) {
    ntalls<-merge(ntalls,dtlan,by.x="lance",by.y="lance")
    ntalls$numer<-ntalls$numer/ntalls$weight.time
    ntalls<-ntalls[,1:6]
  }
  if (nrow(ntalls)==0 | sum(abesp$numero)==0) {ntalls<-data.frame(lance=abesp[1,"lance"],peso_gr=0,peso_m=.1,talla=1,sexo="3",numer=0,stringsAsFactors=FALSE)}
  ntalls$camp<-camp
  if (!sex) ntalls$sexo<-3
  ntalls$num<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  ntaltal<-aggregate(ntalls$num,FUN=sum,by=list(camp=ntalls$camp,lance=ntalls$lance,talla=ntalls$talla,sexo=ntalls$sexo))
  ntaltal$lance<-as.numeric(ntaltal$lance)
  ntaltal$Unit<-paste("TL",ifelse(unid.camp64(gr,esp,zona,dns)$MED==1,"cm","mm"))
  names(ntaltal)[5]<-"number"
  ntaltal<-merge(ntaltal,abesp[,c("lan","lat","long","prof")],by.x = "lance",by.y="lan")
  resulttal<-merge(ntaltal,result1[,c("Survey","HaulNb","DateYr","Quarter","SubDiv","StatRec")],by.x="lance",by.y = "HaulNb")
  if (sex) resulttal[,c("Survey","DateYr","Quarter","lance","lat","long","SubDiv","StatRec","prof","Unit","talla","sexo","number")]
  else resulttal[,c("Survey","DateYr","Quarter","lance","lat","long","SubDiv","StatRec","prof","Unit","talla","number")]
  }
