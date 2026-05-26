#' Crea la salida para el CPUE por lance del Data call de cefalópodos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param gr Grupo SIEMPRE 3 da error si no
#' @param esp especie, se pueden incluir varias especies y da la suma de los datos de varias especies, es útil para sacar los datos de un género o una familia.
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa"
#' @param quarter si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @param incl0 si T se incluyen los lances sin captura de la especie, si F sólo salen los lances con capturas
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CephDataByH(3,10,"P14","Porc")
#' @export
CephDataByH64<-function(gr=3,esp,camp,zona="cant",dns=c("local","serv"),quarter=FALSE,incl2=FALSE,incl0=FALSE) {
    if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
    if (gr!=3) {stop("Función para dar salidas de Cefalópodos, no utilizar para otros grupos")}
    if (any(!esp %in% cefsps$CampCode)) {(stop("Especie de cefalópodo no contemplada, si quiere revise cefsps en los datos incluidos en la librería"))}
    DB1<-as.data.table(maphist64(3,esp,camp,zona,dns,cor.time = T,incl2=incl2,plot = F,out.dat = T))
    dlan<-datlan.camp64(camp,zona,dns,redux = F,bio=F,incl2 = T)
    dlan<-dplyr::rename(dlan,lan=lance,ICESDivision=zona)
    dlan$ICESDivision<-paste0("27.",dlan$ICESDivision)
    if(quarter==T) dlan$quarter=substr(quarters(as.Date(dlan$fecha)),2,2)
    DB1<-dplyr::left_join(DB1,dplyr::select(dlan,lan,ICESDivision),by="lan")
    DB1<-dplyr::rename(DB1,HaulNo = lan)
    DB1$HaulNo<-formatC(DB1$HaulNo,width = 3)
    DB<-as.data.table(CAMPtoHH64(camp,zona,dns,incl2 = T))
    DB<-dplyr::select(DB,"Survey","Quarter","HaulNo","Year","Month","HaulLat","HaulLong","StatRec")
    DB$HaulNo<-formatC(DB$HaulNo,width = 3)
    DB<-dplyr::left_join(DB1,DB,by="HaulNo")
    #DB<-dplyr::left_join(DB,dlan)
    DB$RecordType="SA"
    # DB$Survey="SP-NORTH"
    # DB$rectlong<-cut(dlan$longitud_l,breaks=seq(from=-10,to=-1,by=1),labels=rev(c("E8","E7","E6","E5","E4","E3","E2","E1","E0"))) # ,"D9","D8"
    # DB$rectlat<-cut(dlan$latitud_l,breaks=seq(from=41.5,to=44.5,by=.5),labels=c(12:17))
    # #Crea la columna del rectangulo ICES
    # DB$StatRec<-paste(DB$rectlat,DB$rectlong)
    # DB$Year<-camptoyear(camp)
    # DB$Quarter<-dlan$quarter
#    DB$ICESdiv<-dlan$zona
    DB$LatDgr<-formatC(trunc(abs(DB$HaulLat)),width=3,flag=0)
    DB$LatMin<-trunc((DB$HaulLat-trunc(DB$HaulLat))*60)
    DB$LongDgr<-formatC(trunc(abs(DB$HaulLong)),width=3,flag=0)
    DB$LongMin<-trunc((abs(DB$HaulLong)-trunc(abs(DB$HaulLong)))*60)
    if (length(esp)==1) DB$FAOAsphis<-cefsps[cefsps$CampCode==esp,"ASFIS_CODE"]
    else DB$FAOAsphis<-paste(cefsps[cefsps$CampCode==esp[1],"ASFIS_CODE"],"spp")
    DB$Abundance<-DB$numero*2
    DB$UnitAb<-"numbers per hour"
    DB$CPUE<-DB$peso.gr*2/1000
    DB$CPUEUnits<-"kgs per hour"
    DB<-DB[,c("RecordType","Survey","Year","Quarter","Month","ICESDivision","StatRec","LatDgr","LatMin","LongDgr","LongMin","FAOAsphis","Abundance","UnitAb","CPUE","CPUEUnits")]
    if (!incl0) DB<-dplyr::filter(DB,CPUE>0)
    if (length(esp)>1) {
        message("Los resultados son la suma de los datos de abundancia/CPUEs de varias especies:")
        esps=buscaesp64(gr,esp[1],zona,dns)
        for (i in esp[2:length(esp)]) {esps<-c(esps,buscaesp64(gr,i,zona,dns))}
        message(paste(esps,collapse = ", "),collapse="")

    }
    DB
    }

