#' Calcula las abundancias estratificadas por edad 
#'  
#' Función de resultados: abundancias estratificadas por edad para cada estrato batimétrico a partir de los datos del camp.
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña de la que se extraen los datos un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param dns Elige de dónde se toman los datos del ordenador "local" del servidor "serv"
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param AltAlk ALK alternativa tomada de un fichero de edad del Camp edadXYY.dbf sin ruta ni extensión
#' @examples edadstr.camp(1,45,"P01","Porc",8)
#' @family edades
#' @export
edadstr.camp64<-function(gr,esp,camp,zona="cant",dns="local",plus=8,cor.time=TRUE,AltAlk=NA,n.ots=F) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("Sólo se puede incluir una especie en esta función")}
  ntalls<-readCampDBF("ntall",zona,camp,dns)   #,grupo==as.character(gr) & esp==as.character(esp))  #[,c("lance","peso_gr","peso_m","talla","sexo","numer")]
  names(ntalls)<-gsub("_", ".",names(ntalls))
  ntalls<-ntalls[ntalls$grupo==gr & ntalls$esp==esp,c("lance","peso.gr","peso.m","talla","sexo","numer")]
  ntalls$lance<-as.numeric(as.character(ntalls$lance))
  ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,incl0=FALSE,incl2=FALSE)
  lan<-lan[!is.na(lan$estrato),]
  lan<-lan[,c("lance","sector","weight.time")]
  ntalls<-ntalls[ntalls$lance %in% lan$lance,]
  if (any(cor.time,camp=="N83",camp=="N84")) {
    ntalls<-merge(ntalls,lan,by.x="lance",by.y="lance")
    if (any(ntalls$weight.time==0)) {
      ntalls$weight.time[ntalls$weight.time==0]=.1
      warning("Hay lances con duración 0 minutos, revisa validez")
    }
    ntalls$numer<-ntalls$numer/ntalls$weight.time
    ntalls<-ntalls[,1:6]
  }
  edad<-GetAlk.camp64(gr,esp,camp,zona,dns,plus,n.ots = FALSE,AltAlk,keep_sexo=TRUE)
  # identifica si la ALK est? hecha por sexos o conjunta
  agebysex<-ifelse(any(edad$sexo!=3),T,F)
  if (agebysex) {
    if (all(ntalls$sexo==3)) {
      print("ALK por sexos datos tallas no, simplifique la ALK",quote=FALSE)
      agebysex<-F
      b<-1
      stop("ALK por sexos datos tallas no, simplifique la ALK")
    }
    edadsx<-split(edad,factor(edad$sexo))
    ntallssx<-split(ntalls,factor(ntalls$sexo))
    talsdumb<-vector("list",length(ntallssx))
    edaddumb<-vector("list",length(edadsx))
    for (i in 1:length(ntallssx)) talsdumb[[i]]<-levels(factor(ntallssx[[i]][,4]))
    for (i in 1:length(edadsx)) edaddumb[[i]]<-levels(factor(edadsx[[i]][,1]))
    b<-vector("list",length(edadsx))
    for (i in 1:length(edadsx)) b[[i]]<-which(match(talsdumb[[i]],edaddumb[[i]],nomatch=0)==0,T)
    bb <- vector("list", length(edadsx))
    for (i in 1:length(edadsx)) bb[[i]]<-which(match(edaddumb[[i]],talsdumb[[i]],nomatch=0)==0,T)
  }
  else {
    a<-as.numeric(names(tapply(ntalls$numer,ntalls$talla,sum)))
    b<-which((match(a,edad$talla,nomatch=0)==0),T)
    bb <- vector("list",3)
  }
  if (any(sapply(b,length)>0)) { # | any(sapply(bb,length)>0)
    if (agebysex) {
      print("Tallas que no aparecen en ALK:",quote=FALSE)
      print(paste("sex",names(edadsx),b))
      print("Tallas en ALK que no aparecen en distribuci?n:",quote=FALSE)
      print(paste("sex",names(edadsx),bb))
    }
    else {
      print("Las tallas: ",quote=FALSE)
      print(a[b])
      print("no estan en la clave talla edad",quote=FALSE)
    }
  }
  else {
    sonedad<-which(substr(names(edad),1,1)=="E",T)
    for (i in sonedad) {edad[,i]<-edad[,i]/rowSums(edad[,sonedad])}
    lan<-datlan.camp64(camp,zona,dns,redux=TRUE,incl2=FALSE)
    lan<-lan[!is.na(lan$estrato),c("lance","sector")]
    area<-NULL
	dumb<-readCampDBF("camp",zona,camp[1],dns)
	 for (i in 21:45) {
      area<-paste(area,dumb[i],sep=",")
    }
    area<-substr(area,2,nchar(area))
    area<-dumb[,21:45]
    area<-area[-which(is.na(area) | area==0)]
    area<-as.data.frame(cbind(substr(names(area),2,3),as.numeric(t(area))))
    names(area)<-c("sector","arsect")
    area$sector<-toupper(area$sector)
    names(lan)<-c("lance","sector")
    lan<-merge(lan,area,by.x="sector",by.y="sector")
    dumbtal<-data.frame(talla=c(0:(trunc(max(ntalls[,4])/10)*10+10)))
    ntalls<-merge(dumbtal,ntalls,by.x="talla",by.y="talla",all.x=TRUE)
    edad<-merge(dumbtal,edad,by.x="talla",by.y="talla",all.x=TRUE)
    for (i in 1:ncol(ntalls)) {
      if (!identical(as.numeric(which(is.na(ntalls[,i]))),numeric(0))) {ntalls[which(is.na(ntalls[,i])),i]<-0}
    }
    for (i in 1:ncol(edad)) {
      if (!identical(as.numeric(which(is.na(edad[,i]))),numeric(0))) {edad[which(is.na(edad[,i])),i]<-0}
    }
    if (agebysex) {
      ntalls<-ntalls[ntalls$sexo>0,]
      sexos<-names(edadsx)
      if ("1" %in% sexos) {
        lantalmac<-tapply(ntalls$numer,ntalls[,c(1,2,5)],sum)[,,1]
        lantalmac[which(is.na(lantalmac))]<-0
        lanedadmac<-as.data.frame((as.matrix(t(lantalmac)) %*% as.matrix((edad[edad$sexo==1,sonedad]))))
        lanedad<-lanedadmac
      }
      if ("2" %in% sexos) {
        lantalhem<-tapply(ntalls$numer,ntalls[,c(1,2,5)],sum)[,,2]
        lantalhem[which(is.na(lantalhem))]<-0
        lanedadhem<-as.data.frame((as.matrix(t(lantalhem)) %*% as.matrix((edad[edad$sexo==2,sonedad]))))
        lanedad<-lanedadhem+lanedad
      }
      if ("3" %in% sexos) {
        lantalund<-tapply(ntalls$numer,ntalls[,c(1,2,5)],sum)[,,3]
        lantalund[which(is.na(lantalund))]<-0
        lanedadund<-as.data.frame((as.matrix(t(lantalund)) %*% as.matrix((edad[edad$sexo==3,sonedad]))))
        lanedad<-lanedadund+lanedad
      }
    }
    else {
      lantal<-tapply(ntalls$numer,ntalls[,c(1,2)],sum)
      lantal[which(is.na(lantal))]<-0
      lanedad<-as.data.frame((as.matrix(t(lantal)) %*% as.matrix((edad[,sonedad]))))
    }
    nedad<-substr(names(lanedad),2,nchar(names(lanedad)))
    lanedad<-cbind(as.numeric(as.character(dimnames(lanedad)[[1]])),lanedad)
    names(lanedad)<-c("lance",nedad)
    lanedad<-merge(lan,lanedad,by.x="lance",by.y="lance",all.x=TRUE)
    for (i in 1:ncol(lanedad)) {
      if (!identical(as.numeric(which(is.na(lanedad[,i]))),numeric(0))) {lanedad[which(is.na(lanedad[,i])),i]<-0}
    }
    lansect<-as.vector(tapply(lanedad$sector,lanedad$sector,length))
    areas<-as.vector(tapply(as.numeric(as.character(lanedad$arsect)),lanedad$sector,mean))
    taplan<- function(x) {tapply(x,lanedad$sector,mean)}
    edadsect<-apply(lanedad[,c(4:ncol(lanedad))],2,taplan)
    weiman<- function(x) {weighted.mean(x,areas)}
    edadsect<-as.data.frame(t(rbind(edadsect,apply(edadsect,2,weiman))))
    names(edadsect)<-c(names(edadsect)[c(1:(ncol(edadsect)-1))],"total")
    edadsect
    #		print(lanedad[order(as.numeric(as.character(lanedad$lance))),])
  }
}
