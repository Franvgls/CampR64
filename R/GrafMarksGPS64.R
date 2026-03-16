#' Gráficos de dragas o ctds a partir de las marcas del pescawin
#'
#' Grafica todas las dragas o CTDs de una campaña siempre que se hayan marcado con el PescaWin durante la misma
#' Requiere que los ficheros de marcas del PescaWin estén en los directorios "C:/GPS/mrk/porcupine/" para Porcupine o "C:/GPS/mrk/Norte/" para Demersales y "C:/GPS/mrk/arsa/" para el Golfo de Cádiz
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" 
#' @param dns Elige de dónde se toman los datos, "local" del propio ordenador, "serv" del servidor si está configurado
#' @param event Elige el evento que se representa, dragas o CTDs
#' @param add Si T añade los puntos al gráfico previo, si F saca el gráfico nuevo
#' @param ti Si T incluye un título con el nombre de la campaña
#' @param years Si T cambia el código de campaña por el año de la campaña si tiene formato camp (ver parámetro camp)
#' @param ptbg Definido como NA, si tiene valor 1:4 o colores definidos en R cambia el color del punto de los datos.
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña 
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña  
#' @param cuadr Si T por defecto, incluye cuadrículas de 5x5 millas, si F no incluye las líneas
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param es Si T por defecto los carteles salen en español, en caso contrario en inglés
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @examples GrafMarksGPS64("N14","Cant")
#' @family mapas
#' @family PescaWin
#' @export
GrafMarksGPS<-function(camp,zona="cant",dns="local",event="Draga",add=FALSE,ti=TRUE,years=TRUE,pch=NA,ptbg=NA,xlims=NA,ylims=NA,cuadr=TRUE,es=T,places=TRUE,bw=FALSE) {
  if (substr(event,1,5)!="Draga" & event!="CTD") stop("Sólo se aceptan Dragas o CTD")
  if(dev.cur() == 1) dev.new()
  if (is.na(ptbg)) {ptbg<-ifelse(add,"red","blue")} else ptbg==ptbg
  if (substr(zona,1,4)=="Cant") {
    di<-"norte"
    fil<-paste0(di,"_",camp,".mrk")
    if (!add) {
      if (any(!is.na(xlims))) {MapNort64(xlims=xlims,ylims=ylims,cuadr=cuadr,es=es,places=places,bw=bw)} else MapNort64(cuadr=cuadr,es=es,places=places,bw=bw)
    }
    legend("bottomright",paste0("Dredge","s ",ifelse(years,camptoyear(camp),camp)),pch=ifelse(is.na(pch),25,pch),pt.bg=ptbg,inset=ifelse(add,c(.15,.15),c(.1,.15)),bty="n")
  }
    if (substr(zona,1,4)=="porc") {
    di<-"Porcupine"    
    fil<-paste0("Porcupin_",camp,".mrk")
    if (!add) {
      if (any(!is.na(xlims))) {mapporco64(xlims=xlims,ylims=ylims,cuadr=cuadr)} else mapporco64(cuadr=cuadr)
    }
    legend("bottomright",paste0(event,"s ",camp),pch=ifelse(is.na(pch),25,pch),pt.bg=ptbg,inset=ifelse(add,c(.1,.15),c(.15,.15)),bty="o",bg="white")
    }
  if (zona=="arsa") {
    if (zona=="arsa") {
      di<-"arsa"
      fil<-paste0("cadiz_",camp,".mrk")
      if (!add) {
        if (any(!is.na(xlims))) {MapArsa64(xlims=xlims,ylims=ylims,places=places,cuadr=cuadr,bw=bw)} else MapArsa()
      }
      legend("topright",paste0(event,"s ",camp),pch=ifelse(is.na(pch),25,pch),pt.bg=ptbg,inset=ifelse(add,c(.3,.05),c(.3,.15)),bty="o",bg="white")
    }
  }
  direc<-paste0('c:/gps/mrk/',di,"/",fil)
  mrks<-read.csv(direc,header=F)
  mrks$V3<-as.character(mrks$V3)
  if (nrow(mrks[substr(mrks$V3,1,nchar(event))==event,])==0) stop(paste0("No existen ",paste0(event,"s")," en ",camp))
  points(V2~V1,mrks[substr(mrks$V3,1,nchar(event))==event,],pch=ifelse(is.na(pch),25,pch),bg=ptbg,cex=1)
  if (event=="Draga" & !es) {event<-"Dredge"}
  print(paste0(event,"s ",nrow(mrks[substr(mrks$V3,1,nchar(event))==event,])))
  #if (ti & !add) title(paste0("Campaña ",camp))
  }
#  legend("bottomright","tonto B",pch=25,inset=c(.5,.55))