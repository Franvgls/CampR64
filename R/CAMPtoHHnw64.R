#' Exporta datos de formato CAMP a formato DATRAs HH
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de los lances para una campaña y los transforma en formato DATRAS HH.
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns elige de dónde se toman los datos, si del ordenador ("local") o del servidor ("serv") si se ha configurado {\code{configurarCampR64}}
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @return Devuelve un data.table con datos de cada lance en el formato HH de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @seealso {\code{CAMPtoHL}}
#' @examples # CAMPtoHHnw64("P01","porc","local")
#' @export
CAMPtoHHnw64<-function(camp,zona="cant",dns=c("local","serv"),quart=T,incl2=F) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    DB<-datlan.camp64(camp,zona,dns,redux=F,incl0 = T,incl2=incl2)
    if (zona=="cant") {
      DB$Survey="G2784"
      DB$rectlong<-cut(DB$longitud_l,breaks=seq(from=-10,to=-1,by=1),labels=rev(c("E8","E7","E6","E5","E4","E3","E2","E1","E0"))) # ,"D9","D8"
       DB$rectlat<-cut(DB$latitud_l,breaks=seq(from=41.5,to=44.5,by=.5),labels=c(12:17))
       #Crea la columna del rectangulo ICES
       DB$icesrect<-paste(DB$rectlat,DB$rectlong)
       DB$Gear="BAK"
       DB$barco=ifelse(DB$barco=="29MO","29MO",ifelse(DB$barco=="MOL","29MO",ifelse(DB$barco=="CDS","29CS")))
       DB$WarpDiameter=ifelse(DB$barco=="CDS",22,24)
       DB$DoorType=ifelse(DB$barco=="CDS","WR","T4")
       DB$DoorSurface=ifelse(substr(DB$barco,1,3)=="CDS",3.6,1.8)
       DB$DoorWeight=ifelse(substr(DB$barco,1,3)=="CDS",650,350)
       if(quart) DB$quarter<-"4"
       DB$lance<-format(as.integer(DB$lance),width=3,justify = "r")
       DB$StationName<-format(as.integer(DB$cuadricula),width = 3,justify="r")
       codigos_nuevos <- c(
         "10" = "MF",
         "20" = "FE",
         "30" = "EP",
         "40" = "PA",
         "50" = "AB"
       )
       DB$SurveyIndexArea<- codigos_nuevos[as.character(DB$cuadricula)]
#       DB$StNo=DB$lance
    }
    if (zona=="porc") {
      DB$Survey="G5768"
      DB$rectlong<-cut(DB$longitud_l,breaks=seq(from=-15,to=-11,by=1),labels=rev(c("D8","D7","D6","D5"))) # ,"D9","D8"
       DB$rectlat<-cut(DB$latitud_l,breaks=seq(from=50.5,to=54,by=.5),labels=c(30:36))
       DB$icesrect<-paste0(DB$rectlat,DB$rectlong)
       DB$barco="29VE"
       DB$Gear="PORB"
       DB$DoorType="P"
       DB$DoorSurface=4.5
       DB$DoorWeight=850
       if(quart) DB$quarter<-"3"
       DB$WarpDiameter=20
       if(any(nchar(DB$lance)>2)) message("Lances con más de 2 carácteres, Porcupine no suele tener más de 99 lances, revise datos")
       DB$lance<-format(as.integer(DB$lance),width=2,justify="r")
       DB$StationName<-format(as.integer(DB$cuadricula),width = 3,justify="r")
       DB$estrato<-cut(DB$prof_l,breaks=c(120,300,450,800),labels=c("E","F","G"))
       DB$SurveyIndexArea<- substr(DB$sector,1,1)
    }
    if (zona=="arsa") {
      DB$Survey = ifelse(substr(camp, 1, 1) == "1", "G7511", "G4309")       #"SP-ARSA"
      DB$rectlong<-cut(DB$longitud_l,breaks=seq(from=-9,to=-6,by=1),labels=c("E1","E2","E3")) # ,"D9","D8"
      DB$rectlat<-paste0("0",cut(DB$latitud_l,breaks=seq(from=36.0,to=37.5,by=.5),labels=as.character(c(1:3))))
      DB$icesrect<-paste0(DB$rectlat,DB$rectlong)
      DB$Gear="BAK"
      DB$barco=ifelse(substr(DB$barco,1,3)=="COR","29CS",ifelse(DB$barco=="MOL","29MO",ifelse(DB$barco=="VIZ","29VE","Vir")))
      #      DB$barco=ifelse(substr(DB$barco,1,3)=="COR","29CS",ifelse(DB$barco=="MOL","29MO",ifelse(DB$barco=="VIZ","29VE","Vir")))
      DB$WarpDiameter=ifelse(DB$barco=="29CS",22,24)
      DB$DoorType=ifelse(DB$year<2008,"WR","T4")
      DB$DoorSurface=ifelse(DB$year<2008,3.6,1.8)
      DB$DoorWeight=ifelse(DB$year<2008,650,350)
      if(quart) DB$quarter<-ifelse(substr(camp,1,1)=="1","1","4")
      if(any(nchar(DB$lance)>2)) message("Lances con más de 2 carácteres, Arsa no suele tener más de 99 lances, revise datos")
      DB$lance<-formatC(as.integer(DB$lance),flag=0,width=3)
      DB$StationName=format(as.integer(DB$cuadricula),width = 3,justify="r")
      DB$estrato<-cut(DB$prof_l,breaks=c(1,30,100,200,500,770),labels=c("H1","H2","H3","H4","H5"))
      DB$SurveyIndexArea<- c(-9)
    }
    DB$StartTime<-paste0(formatC(as.numeric(substr(DB$hora_l,1,2)),flag=0,width=2),sprintf("%02s",substr(DB$hora_l,4,5)))
    DB$estn<-as.numeric(as.character(DB$estn))
    HH_north<- data.table::data.table(RecordHeader="HH",Quarter=DB$quarter,Country="ES",Platform=DB$barco,Gear=DB$Gear,
                                     SweepLength=DB$malletas,GearExceptions=-9,DoorType=DB$DoorType,StationName=DB$StationName,
                                     HaulNumber=DB$lance,Year=DB$year,Month=substr(DB$fecha,6,7),
                                     Day=substr(DB$fecha,9,10),StartTime=DB$StartTime,DepthStratum=DB$estrato,
                                     HaulDuration=round(DB$haul.mins*DB$weight.time),DayNight="D",ShootLatitude=DB$latitud_l,
                                     ShootLongitude=DB$longitud_l,HaulLatitude=DB$latitud_v,HaulLongitude=DB$longitud_v,StatisticalRectangle=DB$icesrect,
                                     BottomDepth=DB$prof_l,HaulValidity=ifelse(DB$validez==1,"V",ifelse(DB$validez==2 | DB$validez==3,"A","I")),
                                     HydrographicStationID=DB$estn,StandardSpeciesCode=1,BycatchSpeciesCode=0,DataType="R",NetOpening=round(DB$abert_v,1),
                                     Rigging=-9,Tickler=-9,Distance=round(DB$recorrido),WarpLength=DB$cable,WarpDiameter=DB$WarpDiameter,WarpDensity=-9,
                                     DoorSurface=DB$DoorSurface,DoorWeight=DB$DoorWeight,DoorSpread=trunc(DB$dista_p),WingSpread=round(DB$abert_h,1),
                                     Buoyancy=-9,KiteArea=-9,GroundRopeWeight=-9,TowDirection=formatC(DB$rumbo,flag=0,width=3),SpeedGround=DB$velocidad,
                                     SpeedWater=-9,SurfaceCurrentDirection=-9,SurfaceCurrentSpeed=-9,BottomCurrentDirection=-9,BottomCurrentSpeed=-9,WindDirection=DB$dir_viento,
                                     WindSpeed=DB$vel_viento,SwellDirection=-9,SwellHeight=DB$est_mar,SurfaceTemperature=-9,BottomTemperature=DB$temp,SurfaceSalinity=-9,
                                     BottomSalinity=DB$sali,ThermoCline=-9,ThermoClineDepth=-9,CodendMesh=-9,SecchiDepth=-9,Turbidity=-9,TidePhase=-9,TideSpeed=-9,
                                     PelagicSamplingType=-9,MinTrawlDepth=-9,MaxTrawlDepth=-9,SurveyIndexArea=DB$SurveyIndexArea,Survey=DB$Survey,EDMO=-9,ReasonHaulDisruption=-9)
    HH_north$DoorSpread[is.na(HH_north$DoorSpread)]<-c(-9)
    HH_north$NetOpening[is.na(HH_north$NetOpening)]<-c(-9)
    HH_north$WingSpread[is.na(HH_north$WingSpread)]<-c(-9)
    HH_north$Distance[is.na(HH_north$Distance)]<-c(-9)
    HH_north$WindDirection[is.na(HH_north$WindDirection)]<-c(-9)
    HH_north$WindSpeed[is.na(HH_north$WindSpeed)]<-c(-9)
    HH_north$SwellHeight[is.na(HH_north$SwellHeight)]<-c(-9)
    HH_north$BottomTemperature[is.na(HH_north$BottomTemperature)]<-c(-9)
    HH_north$BottomSalinity[is.na(HH_north$BottomSalinity)]<-c(-9)
    HH_north$HydrographicStationID[is.na(HH_north$HydrographicStationID)]<-c(-9)
    HH_north$TowDirection[is.na(HH_north$TowDirection)]<-c(-9)
#    HH_north$Quarter<- ifelse(is.na(quarter),HH_north$Quarter,quarter)
#    HH_north
    HH_north[order(as.numeric(HH_north$HaulNumber)),]
    }

