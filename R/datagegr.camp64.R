#' Extrae los datos del FAUNA de una especie en concreto en una edad. 
#' 
#' Función de acceso a datos: saca las abundancias por edad y lance de la especie 
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos: Sólo existen datos de edad para algunos peces y la cigala.
#' @param esp Codigo de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña o campañas a representar en el mapa
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa". 
#' @param dns elige si se trabja con archivos del ordenador ("local") o del servidor ("serv")
#' @param plus Edad plus: incluir la edad considerada como plus, solo afecta si se pide la edad solicitada que une todas las edades mayores
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param n.ots Valor interno para decir que no se saque el número de otolitos en la clave sino las proporciones
#' @param AltAlk Clave talla edad alternativa sin ruta ni extensión, NA por defecto usa la clave de la campaña edadXYY.dbf
#' @param incl2 si T coge los datos de lances especiales, si F los excluye
#' @param mediahora Valor para obtener abundancias por hora si media hora es mayor
#' @return Devuelve un data.frame con campos lance, lat, long y abundancia por edad 0,1,2...Plus de edad, lance, peso número subestrato...
#' @family edades
#' @export
datagegr.camp64<- function(gr,esp,camp,zona="cant",dns=c("local","serv"),plus=8,cor.time=TRUE,n.ots=FALSE,AltAlk=NA,incl2=TRUE,mediahora=1) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de m?s de una")}
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
  ntalls<-readCampDBF("ntall",zona,camp,dns)
  ntalls<-ntalls[ntalls$grupo==gr & ntalls$esp==esp,c("lance","peso_gr","peso_m","talla","sexo","numer")]
  names(ntalls)<-gsub("_", ".",names(ntalls))
  ntalls$lance<-as.numeric(as.character(ntalls$lance))
  ntalls$numer<-ntalls$numer*ntalls$peso.gr/ntalls$peso.m
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,incl2=incl2)
  lan<-lan[,c("lance","sector","weight.time")]
  ntalls<-ntalls[ntalls$lance %in% lan$lance,]
  if (any(cor.time,camp=="N83",camp=="N84")) {
    ntalls<-merge(ntalls,lan,by.x="lance",by.y="lance")
    ntalls$numer<-ntalls$numer/ntalls$weight.time
    ntalls<-ntalls[,1:6]
  }
  edad<-GetAlk.camp64(gr,esp,camp,zona,dns,plus,n.ots=n.ots,AltAlk=AltAlk)
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
    bb<- vector("list", length(edadsx))
    for (i in 1:length(edadsx)) bb[[i]]<-which(match(edaddumb[[i]],talsdumb[[i]],nomatch=0)==0,T)
  }
  else {		
    a<-as.numeric(names(tapply(ntalls$numer,ntalls$talla,sum)))
    b<-which((match(a,edad$talla,nomatch=0)==0),T)
    bb<- vector("list",3)
  }
  if (any(sapply(b,length)>0)) { # | any(sapply(bb,length)>0)
    if (agebysex) {
      print("Tallas que no aparecen en ALK:",quote=FALSE)
      print(paste("sex",names(edadsx),b))
      print("Tallas en ALK que no aparecen en distribución:",quote=FALSE)
      print(paste("sex",names(edadsx),bb))
    }
    else {
      print("Las tallas: ",quote=FALSE)
      print(a[b])
      print("no estan en la clave talla edad",quote=FALSE)
      stop()
    }
  }
  else {
    sonedad<-which(substr(names(edad),1,1)=="E",T)
    for (i in sonedad) {edad[,i]<-edad[,i]/rowSums(edad[,sonedad])}
    lan<-datlan.camp64(camp,zona,dns,incl2=incl2,redux=FALSE)[,c("lance","latitud_l","latitud_v","longitud_l","longitud_v","sector","estrato","ewl","ewv","weight.time")] 
    if (all(lan==-1)) {
      lanedad<-data.frame(lan=0,lat=0,long=0,weight.time=0,numero=0,peso.gr=0)
    }
    else {
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
        #print(dim(lantal))
        #print(dim(as.matrix(edad[,sonedad])))
        lanedad<-as.data.frame(t(as.matrix(lantal)) %*% as.matrix(edad[,sonedad]))*mediahora
        #print(lanedad)
      }
      nedad<-substr(names(lanedad),2,nchar(names(lanedad)))
      lanedad<-cbind(as.numeric(as.character(dimnames(lanedad)[[1]])),lanedad)
      names(lanedad)<-c("lance",nedad)
      #lan$latitud_l<-sapply(lan$latitud_l,gradec)
      #lan$longitud_l<-sapply(lan$longitud_l,gradec)*ifelse(lan$ewl=="W",-1,1)
      #lan$latitud_v<-sapply(lan$latitud_v,gradec)
      #lan$longitud_v<-sapply(lan$longitud_v,gradec)*ifelse(lan$ewv=="W",-1,1)
      lan[,"lat"]<-(lan[,"latitud_l"]+lan[,"latitud_v"])/2
      lan[,"long"]<-(lan[,"longitud_l"]+lan[,"longitud_v"])/2
      lan<-lan[,c("lance","lat","long","weight.time")]
      names(lan)<-c("lan","lat","long","weight.time")
      lanedad<-merge(lan,lanedad,by.x="lan",by.y="lance",all.x=TRUE)
      for (i in 1:ncol(lanedad)) {
        if (!identical(as.numeric(which(is.na(lanedad[,i]))),numeric(0))) {lanedad[which(is.na(lanedad[,i])),i]<-0}
      }
    }
  }
  lanedad
}
#datagegr.camp("1"," 50","N02","Cant",8,mediahora=2)
