#'Valores absolutos en número por talla
#'
#'Da Valores absolutos (totales por lance o grupos de lances) en número por talla y sexo (si lo hay)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param lances Da la opción de escribir un número de lance y saca los valores solo para ese lance o grupo de lances.
#' @param sex Por defecto (F) suma todos los individuos como indet. T saca los datos por sexo si los hay, no afecta si sólo hay indeterminados (3)
#' @param muestr Por defecto (T) pondera los datos por el peso total en la captura del lance, si F coge los medidos realmente
#' @family Distribuciones de tallas
#' @examples 
#' dtallan64.camp(gr=1,esp=10,camp="N14",zona="cant",dns="local",lances=108,muestr=T)
#' dtallan64.camp(gr=1,esp=10,camp="N14",zona="cant",dns="local",lances=108,muestr=F)
#' dtallan64.camp(gr=1,esp=10,camp="N14",zona="cant",dns="local",lances=NA,muestr=F)
#' @export
dtallan.camp64<- function(gr,esp,camp,zona,dns,lances=NA,sex=FALSE,muestr=TRUE) {
  if (length(camp)>1) stop("Esta función sólo se puede utilizar para una sola campaña")
  #esp<-format(esp,width=3,justify="r")
  ntalls<-readCampDBF("ntall",zona,camp,dns)
  if (length(esp)==1) {
    if (esp!="999") {ntalls<-ntalls[ntalls$esp==esp & ntalls$grupo==gr,c("lance","peso_gr","peso_m","talla","sexo","numer")]}   
	  #RODBC::sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"' and esp='",esp,"'",sep=""))}
    if (esp=="999") {ntalls<-ntalls[ntalls$grupo==gr,c("lance","peso_gr","peso_m","talla","sexo","numer")]}   
      #ntalls<-RODBC::sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"'",sep=""))
      if (sex==TRUE) {
        warning("Con varias especies no se puede separar por sexos, resultados sin sexos")
        sex=F
        ntalls$sexo<-3
      }
    }
  if (length(esp)>1) {
    ntalls<-{ntalls<-ntalls[ntalls$esp %in% esp & ntalls$grupo==gr,
                            c("lance","peso_gr","peso_m","talla","sexo","numer")]}
	#RODBC::sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp," where grupo='",gr,"' and esp='",esp[1],"'",sep=""))
    # for (i in 2:length(esp)) {
      # ntalls<-rbind(ntalls,RODBC::sqlQuery(ch1,paste("select lance,peso_gr,peso_m,talla,sexo,numer from NTALL",camp,
                                              # " where grupo='",gr,"' and esp='",esp[i],"'",sep="")))
    # }
    if (sex==TRUE) {
      warning("Con varias especies no se puede separar por sexos, resultados sin sexos")
      sex=F
    }
    ntalls$sexo<-3
  }
  names(ntalls)<-gsub("_", ".",names(ntalls))
  ntalls$lance<-as.numeric(as.character(ntalls$lance))
  if (muestr) ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  if (any(!is.na(lances))) {
    ntalls<-ntalls[ntalls$lance %in% lances,]
  }
  if (nrow(ntalls)==0) ntalls<-data.frame(lance=0,peso.gr=0,peso.m=0,talla=0,sexo=3,numer=0)
  dtalln<-c("machos","hembras","indet")
  dumb<-ntalls
  dumb$lance<-as.numeric(dumb$lance)
  dumb$sexo<-factor(dumb$sexo,exclude=0)
  dumb1<-tapply(dumb$numer,dumb[,c(4,5)],sum,na.rm=TRUE)
  dumb1[which(is.na(dumb1))]<-0
  sxs<- match(c(1:3),dimnames(dumb1)$sexo)
  if (dim(dumb1)[2]>1) {
    dtall<-as.data.frame(dumb1)  
  }
  else dtall<-as.data.frame(dumb1)
  dtall<-as.data.frame(cbind(as.numeric(dimnames(dtall)[[1]]),dtall))
  names(dtall)<-c("V1",dtalln[which(!is.na(sxs))])
  dumb<-as.data.frame(c(1:(trunc(max(dtall[,1])/10)*10+10)))
  names(dumb)<-"talla"
  dtall<-merge(dumb,dtall,by.x="talla",by.y="V1",all.x=TRUE)
  for (i in 2:ncol(dtall)) {
    if (!identical(as.numeric(which(is.na(dtall[,i]))),numeric(0))) {
      dtall[which(is.na(dtall[,i])),i]<-0
    }
  }
  if (length(esp)>1 | any(esp=="999")) {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
  }
  if (!sex & ncol(dtall)>2) {
    dtall<-data.frame(talla=dtall[,1],numero=rowSums(dtall[,2:ncol(dtall)]))
  }
  if (!sex) {names(dtall)<-c("talla","numero")}
  if (sum(dtall[,-1])==0) {
    dtall<-dtall[1,]
    print(paste("Sin captura de",buscaesp(gr,esp),ifelse(length(lances)>1,"en estos lances","en este lance")))
  }
  as.data.frame(dtall)
}
