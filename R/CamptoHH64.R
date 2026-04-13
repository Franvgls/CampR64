#' Exporta datos de formato CAMP a formato DATRAs HH
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de los lances para una campaña y los transforma en formato DATRAS HH. 
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" 
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @return Devuelve un data.table con datos de cada lance en el formato HH de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @seealso {\code{CAMPtoHL}}
#' @examples # CAMPtoHH("P01","Porc")
#' @export
CAMPtoHH64<-function(camp,zona="cant",dns=c("local","serv"),quart=T,incl2=F) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    DB<-datlan.camp64(camp,zona,dns,redux=F,incl0 = T,incl2=incl2)
    if (zona=="cant") {
       DB$Survey<-"SP-NORTH"
       DB$rectlong<-cut(DB$longitud_l,breaks=seq(from=-10,to=-1,by=1),labels=rev(c("E8","E7","E6","E5","E4","E3","E2","E1","E0"))) # ,"D9","D8"
       DB$rectlat<-cut(DB$latitud_l,breaks=seq(from=41.5,to=44.5,by=.5),labels=c(12:17))
       #Crea la columna del rectangulo ICES
       DB$icesrect<-paste0(DB$rectlat,DB$rectlong)
       DB$Gear="BAK"
       DB$barco=ifelse(DB$barco=="MOL","29MO",ifelse(DB$barco=="CDS","CDS","29CD"))
       DB$Warpdia=ifelse(DB$barco=="CDS",22,24)
       DB$DoorType=ifelse(DB$barco=="CDS","WR","T4")
       DB$DoorSurface=ifelse(substr(DB$barco,1,3)=="CDS",3.6,1.8)
       DB$DoorWght=ifelse(substr(DB$barco,1,3)=="CDS",650,350)
       if(quart) DB$quarter<-"4"
       DB$lance<-formatC(DB$lance,flag=0,width=3)
       DB$StNo=DB$lance
    }
    if (zona=="porc") {
      DB$Survey<-"SP-PORC"
      DB$rectlong<-cut(DB$longitud_l,breaks=seq(from=-15,to=-11,by=1),labels=rev(c("D8","D7","D6","D5"))) # ,"D9","D8"
       DB$rectlat<-cut(DB$latitud_l,breaks=seq(from=50.5,to=54,by=.5),labels=c(30:36))
       DB$icesrect<-paste0(DB$rectlat,DB$rectlong)
       DB$barco="EZA"
       DB$Gear="PORB"
       DB$DoorType="P"
       DB$DoorSurface=4.5
       DB$DoorWght=850
       if(quart) DB$quarter<-"3"
       DB$Warpdia=20
       if(any(nchar(DB$lance)>2)) warning("Lances con más de 2 carácteres, Porcupine no suele tener más de 99 lances, revise datos")
       DB$lance<-formatC(DB$lance,flag=0,width=2)
       DB$StNo<-DB$cuadricula
       DB$estrato<-cut(DB$prof_l,breaks=c(120,300,450,800),labels=c("E","F","G"))
    }
    if (zona=="arsa") {
      DB$Survey<-"SP-ARSA"
      DB$rectlong<-cut(DB$longitud_l,breaks=seq(from=-9,to=-6,by=1),labels=rev(c("E1","E2","E3"))) # ,"D9","D8"
      DB$rectlat<-paste0("0",cut(DB$latitud_l,breaks=seq(from=36.0,to=37.5,by=.5),labels=as.character(c(1:3))))
      DB$icesrect<-paste0(DB$rectlat,DB$rectlong)
      DB$Gear="BAK"
      DB$barco=ifelse(substr(DB$barco,1,3)=="COR","CDS",ifelse(DB$barco=="MOL","29MO"))
      DB$Warpdia=ifelse(DB$barco=="CDS",22,24)
      DB$DoorType=ifelse(DB$year<2008,"WR","T4")
      DB$DoorSurface=ifelse(DB$year<2008,3.6,1.8)
      DB$DoorWght=ifelse(DB$year<2008,650,350)
      if(quart) DB$quarter<-ifelse(substr(camp,1,1)=="1","1","4")
      if(any(nchar(DB$lance)>2)) warning("Lances con más de 2 carácteres, Arsa no suele tener más de 99 lances, revise datos")
      DB$lance<-formatC(DB$lance,flag=0,width=2)
      DB$StNo=DB$lance
      DB$estrato<-cut(DB$prof_l,breaks=c(1,30,100,200,500,770),labels=c("H1","H2","H3","H4","H5"))
    }
    DB$TimeShot<-paste0(formatC(as.numeric(substr(DB$hora_l,1,2)),flag=0,width=2),sprintf("%02s",substr(DB$hora_l,4,5)))
    DB$estn<-as.numeric(as.character(DB$estn))
    HH_north<-data.table::data.table(RecordType="HH",Survey=DB$Survey,Quarter=DB$quarter,Country="ES",Ship=DB$barco,
                                     Gear=DB$Gear,SweepLngt=DB$malletas,GearEx=-9,DoorType=DB$DoorType,StNo=DB$StNo,
                                     HaulNo=DB$lance,Year=DB$year,Month=format(as.Date(DB$fecha), "%m"),
                                     Day=substr(DB$fecha,1,2),TimeShot=DB$TimeShot,DepthStratum=DB$estrato,HaulDur=round(DB$haul.mins*DB$weight.time),
                                     DayNight="D",ShootLat=DB$latitud_l,ShootLong=DB$longitud_l,HaulLat=DB$latitud_v,HaulLong=DB$longitud_v,
                                     StatRec=DB$icesrect,Depth=DB$prof_l,HaulVal=ifelse(DB$validez==1,"V",ifelse(DB$validez==2 | DB$validez==3,"A","I")),
                                     HydroStNo=DB$estn,StdSpecRecCode=1,BySpecRecCode=0,DataType="R",Netopening=round(DB$abert_v,1),Rigging=-9,Tickler=-9,
                                     Distance=round(DB$recorrido),Warplngt=DB$cable,Warpdia=DB$Warpdia,WarpDen=-9,DoorSurface=DB$DoorSurface,DoorWgt=DB$DoorWght,
                                     DoorSpread=trunc(DB$dista_p),WingSpread=round(DB$abert_h,1),Buoyancy=-9,KiteDim=-9,WgtGroundRope=-9,TowDir=formatC(DB$rumbo,flag=0,width=3),
                                     GroundSpeed=DB$velocidad,SpeedWater=-9,SurCurDir=-9,SurCurSpeed=-9,BotCurDir=-9,BotCurSpeed=-9,WindDir=DB$dir_viento,
                                     WindSpeed=DB$vel_viento,SwellDir=-9,SwellHeight=DB$est_mar,SurTemp=-9,BotTemp=DB$temp,SurSal=-9,BotSal=DB$sali,
                                     ThermoCline=-9,ThClineDepth=-9,CodendMesh=-9,SecchiDepth=-9,Turbidity=-9,TidePhase=-9,TideSpeed=-9,PelSampType=-9,
                                     MinTrawlDepth=-9,MaxTrawlDepth=-9,DateofCalculation=format(Sys.Date(), "%Y%m%d"))
    HH_north$DoorSpread[is.na(HH_north$DoorSpread)]<-c(-9)
    HH_north$Netopening[is.na(HH_north$Netopening)]<-c(-9)
    HH_north$WingSpread[is.na(HH_north$WingSpread)]<-c(-9)
    HH_north$Distance[is.na(HH_north$Distance)]<-c(-9)
    HH_north$WindDir[is.na(HH_north$WindDir)]<-c(-9)
    HH_north$WindSpeed[is.na(HH_north$WindSpeed)]<-c(-9)
    HH_north$SwellHeight[is.na(HH_north$SwellHeight)]<-c(-9)
    HH_north$BotTemp[is.na(HH_north$BotTemp)]<-c(-9)
    HH_north$BotSal[is.na(HH_north$BotSal)]<-c(-9)
    HH_north$HydroStNo[is.na(HH_north$HydroStNo)]<-c(-9)
    HH_north$TowDir[is.na(HH_north$TowDir)]<-c(-9)
    HH_north[order(HH_north$HaulNo),]               
    }
