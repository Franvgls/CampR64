#' Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" 
#' @param inclSpecie si T incluye el nombre de la especie y el Código, si no sólo el Aphia
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @param export Si T crea un fichero csv con todos los datos corregidos (APHIAs) en el directorio CAMP donde está el especies.dbf este es importable al especies.dbf con un append from deli with , quitando todos los peces grupo="1"
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CAMPtoHL("P14","Porc")
#' @export
CAMPtoHL64<-function(camp,zona="cant",dns=c("local","serv"),inclSpecie=FALSE,quart=TRUE,incl2=FALSE,export=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  DB<-data.table::as.data.table(datlan.camp64(camp,zona,dns,redux=F,incl0 = F,incl2=incl2))
  ntalls <- data.table::as.data.table(readCampDBF("ntall", zona=zona,dns=dns, camp=camp))
  names(ntalls) <- tolower(names(ntalls))  
  especies <- data.table::as.data.table(readCampDBF("especies", zona,dns))
  names(especies) <- tolower(names(especies))
  names(especies)<-tolower(names(especies))
  especies<-subset(especies,especies$grupo==1)
  especies<-dplyr::arrange(especies,esp)  
  especies %>% dplyr::mutate_if(is.factor,as.character) -> especies
  especies$especie[1]<-buscaesp64(especies$grupo[1],especies$esp[1],zona,dns)
  if (substr(x=especies$especie[1],start=nchar(especies$especie[1])-3,stop=nchar(especies$especie[1]))==" sp.") {
    especies$especie[1]<-sub(" sp.","",buscaesp64(especies$grupo[1],especies$esp[1],zona,dns),perl=T,useBytes = T)
  }
  if (is.na(especies$aphia[1])) especies$aphia[1]<-worrms::wm_name2id(as.character(especies$especie[1]))
  if (export) {
    for (i1 in 2:nrow(especies)) {
      if (is.na(especies$aphia[i1])) {
        especies$especie[i1]<-buscaesp64(especies$grupo[i1],especies$esp[i1],zona,dns)
        if (substr(x=especies$especie[i1],start=nchar(especies$especie[i1])-3,stop=nchar(especies$especie[i1]))==" sp.") {
          especies$especie[i1]<-sub(" sp.","",buscaesp64(especies$grupo[i1],especies$esp[i1],zona,dns),perl=T,useBytes = T)
        }
        especies$aphia[i1]<-worrms::wm_name2id(especies$especie[i1])
      }
    }
    export_path <- file.path(CampR64_paths[["local"]][[zona]], "peces.csv")
    write.csv(especies, export_path, row.names = FALSE)}
  if (substr(zona,1,4)=="cant") {
    DB$barco=ifelse(DB$barco=="MOL","29MO",ifelse(DB$barco=="CDS","CDS","CD29"))
    DB$Survey="SP-NORTH"
    DB$Gear="BAK"
    DB$GearEx=-9
    DB$DoorType=ifelse(DB$barco=="CDS","W","P")
    if(quart) DB$quarter<-"4"
    DB$lance<-formatC(DB$lance,flag=0,width=3)
    ntalls$lance<-formatC(ntalls$lance,flag=0,width=3)
    DB$StNo=DB$lance
  }
  if (substr(zona,1,4)=="porc") {
    DB$barco="EZA"
    DB$Survey="SP-PORC"
    DB$Gear="PORB"
    DB$GearEx=-9
    DB$DoorType="P"
    if(quart) DB$quarter<-"3"
    DB$lance<-formatC(DB$lance,flag=0,width=2)
    ntalls$lance<-formatC(ntalls$lance,flag=0,width=2)
    DB$StNo<-DB$cuadricula
  }
  if (substr(zona,1,4)=="arsa") {
    DB$barco=ifelse(substr(DB$barco,1,3)=="COR","CDS",ifelse(DB$barco=="MOL","29MO"))
    DB$Survey="SP-ARSA"
    DB$Gear="BAK"
    DB$GearEx=-9
    DB$DoorType=ifelse(substr(DB$barco,1,3)=="COR","W","P")
    if(quart) DB$quarter<-ifelse(substr(camp,1,1)=="1","1","4")
    DB$lance<-formatC(DB$lance,flag=0,width=2)
    ntalls$lance<-formatC(ntalls$lance,flag=0,width=2)
    DB$StNo=DB$lance
  }
  DB <- as.data.frame(DB)[,c("year","Survey","barco","quarter","Gear","malletas","GearEx","DoorType","lance","StNo")]
  ntalls <- as.data.frame(ntalls)
  ntalls <- ntalls[ntalls$lance %in% DB$lance, , drop=FALSE]
  ntalls <- ntalls[ntalls$grupo == 1, , drop=FALSE]
  ntalls$SubFact <- round(ntalls$peso_gr/ntalls$peso_m, 4)
  ntalls <- data.table::as.data.table(ntalls)
  ntalls <- as.data.frame(ntalls)
  dumb <- aggregate(numer ~ lance + esp + sexo + cate, data=ntalls, FUN=sum)
  names(dumb)[names(dumb)=="numer"] <- "NoMeas"
  dumb <- as.data.frame(dumb)[, c("lance","esp","sexo","cate","NoMeas")]
  ntallsdumb<-merge(ntalls,dumb,all.x=TRUE)
  ntallsdumb$SpecCode<-as.character(especies$aphia[match(ntallsdumb$esp,especies$esp)])
  ntallsdumb$Specie<-as.character(especies$especie[match(ntallsdumb$esp,especies$esp)])
  ntallsdumb$med<-as.character(especies$med[match(ntallsdumb$esp,especies$esp)])
  ntallsdumb$incr<-as.character(especies$increm[match(ntallsdumb$esp,especies$esp)])
  ntallsdumb$LngtCode<-NA
  ntallsdumb$LngtCode[ntallsdumb$med==1]<-"1"
  ntallsdumb$LngtCode[ntallsdumb$med==2]<-"."
  ntallsdumb$LngtCode[ntallsdumb$incr==5]<-"0"
  DB1<-merge(ntallsdumb,data.table::as.data.table(DB),all.x=T,by="lance")
  DB1$Sex<-as.character(factor(DB1$sexo,levels=as.character(1:3),labels=c("M","F","U")))
  if (inclSpecie==T) {
    HL_north<-data.table::data.table(RecordType="HL",Survey=DB1$Survey,Quarter=DB1$quarter,Country="ES",Ship=DB1$barco,Gear=DB1$Gear,
                                     SweepLngt=DB1$malletas,GearEx=DB1$GearEx,DoorType=DB1$DoorType,StNo=DB1$StNo,
                                     HaulNo=DB1$lance,Year=DB1$year,SpecCodeType="W",SpecCode=DB1$SpecCode,
                                     specie=DB1$Specie,SpecVal=1,Sex=DB1$Sex,TotalNo=round(DB1$NoMeas*DB1$SubFact,2),
                                     CatIdentifier=DB1$cate,NoMeas=DB1$NoMeas,SubFactor=DB1$SubFact,SubWgt=DB1$peso_m,
                                     CatCatchWgt=DB1$peso_gr,LngtCode=DB1$LngtCode,LngtClass=DB1$talla,
                                     HLNoAtLngt=DB1$numer,DevStage=-9,LenMeasType=-9,
                                     DateofCalculation=format(Sys.Date(), "%Y%m%d"),Valid_Aphia=DB1$SpecCode)
  }
  else HL_north<-data.table::data.table(RecordType="HL",Survey=DB1$Survey,Quarter=DB1$quarter,Country="ES",Ship=DB1$barco,Gear=DB1$Gear,
                                        SweepLngt=DB1$malletas,GearEx=DB1$GearEx,DoorType=DB1$DoorType,StNo=DB1$StNo,
                                        HaulNo=DB1$lance,Year=DB1$year,SpecCodeType="W",SpecCode=DB1$SpecCode,SpecVal=1,
                                        Sex=DB1$Sex,TotalNo=round(DB1$NoMeas*DB1$SubFact,2),CatIdentifier=DB1$cate,
                                        NoMeas=DB1$NoMeas,SubFactor=DB1$SubFact,SubWgt=DB1$peso_m,
                                        CatCatchWgt=DB1$peso_gr,LngtCode=DB1$LngtCode,LngtClass=DB1$talla,
                                        HLNoAtLngt=DB1$numer,DevStage=-9,LenMeasType=-9,
                                        DateofCalculation=format(Sys.Date(), "%Y%m%d"),Valid_Aphia=DB1$SpecCode)
  if (any(is.na(HL_north$SpecCode))) {
    HL_north[is.na(HL_north$SpecCode),]
    warning("Algunas especies no tienen código AphiaID, conversión incompleta, revise especies.dbf")
  }
  HL_north[order(HL_north$HaulNo,HL_north$SpecCode,HL_north$LngtClass),]               
}