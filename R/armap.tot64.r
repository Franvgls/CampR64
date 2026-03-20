#' Mapa ampliado de Porcupine con las estaciones realizadas
#' 
#' Crea un mapa de Porcupine con las estaciones realizadas(al igua que "armap.camp") pero con la referencia de Irlanda para su mejor ubicación.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant" (ARSA para sacar datos al IBTS, no gráficos)
#' @param lwdl Anchura de las líneas
#' @param col Color de los puntos que representan estaciones
#' @param argr Grosor de las flechas de recorrido
#' @param arrow Si T añade flechas de reccorrido 
#' @param leg Si T añade leyenda
#' @param es Si T nombres en castellano, si F nombres en inglés
#' @param bw Si T mapa en blanco y negro, si F colorea los estratos y sectores
#' @param noval Si T añade las estaciones nulas
#' @param CTDs Si T añade las estaciones de CTD realizadas
#' @param strat strat Si T represent los estratos 
#' @return Saca mapa con el desarrollo de la campaña con la zona y estratificación incluyendo tierra (Porcupine)
#' @examples
#' armap.tot("P14",dns="Porc",noval=TRUE,CTDs=FALSE,bw=TRUE,strat=FALSE,leg=TRUE)
#' armap.tot("N14",dns="Cant",noval=TRUE,CTDs=FALSE,bw=FALSE,leg=TRUE)
#' @family mapas
#' @family resumen general
#' @export
armap.tot64<-function(camp,zona="porc",dns=c("local","serv"),lwdl=1,col=2,argr=2,arrow=FALSE,leg=FALSE,es=FALSE,bw=TRUE,noval=FALSE,
	CTDs=FALSE,strat=FALSE,new.dev=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
	lan<-datlan.camp64(camp,zona,dns,redux=T,incl2=TRUE,incl0=TRUE)
	if (CTDs) {
	  dathidro <- readCampDBF("hidro", zona, camp, dns)
	  if (is.null(dathidro) || nrow(dathidro) == 0) {
	    message(paste0("No existe fichero de CTDs para ", camp, " o está vacío"))
	    CTDs<-F
	    return(invisible(NULL))
	  }
	  dathidro$latitud<-gradec(hidro$latitud)
    dathidro$longitud<-gradec(hidro$longitud)*ifelse(hidro$eswe=="W",-1,1)
    }
	lan<-lan[,c("lance","lat","long","validez")]
	names(lan)<-c("lan","lat","long","val")
	if (zona=="porc") maparea64(es=es,leg=leg,bw=bw)
  else {
	  if (zona=="cant") MapNort64(strat=strat,bw=bw,es=es)
    else {
      if (zona=="arsa") MapArsa64(es=es,bw=bw)
      }
    }
	if (arrow) {
	arrows(lan[c(1:(length(lan[,1])-1)),c(3)],
		lan[c(1:(length(lan[,1])-1)),c(2)],
		lan[c(2:length(lan[,1])),c(3)],
		lan[c(2:length(lan[,1])),c(2)],0.05,col=col,lwd=argr)
		}
	if (noval) points(lan[lan$val==0,c(3,2)],pch=13,cex=1.5)
	if (CTDs) points(hidro[,c(2,1)],pch=25,cex=1,bg="lightblue")
	points(lan[lan$val==1,c(3,2)],pch=16)
	points(lan[lan$val>1,c(3,2)],pch=16,col=3)
	# print(str(hidro))
	if (leg) {
		l1<-c(ifelse(es,"Lances válidos","Valid tows"),ifelse(es,"Lances extra","Extra tows"))
		pts<-c(16,21)
		cexs<-c(1.3,1.3)
		bgs<-c(1,"green")
		if (noval) {
			l1<-c(l1,ifelse(es,"Lances nulos","Null tows"))
			pts<-c(pts,13)
			bgs<-c(bgs,1)
			cexs<-c(cexs,1.3)
			}
		if (CTDs) {
			l1<-c(l1,"CTD")
			pts<-c(pts,25)
			bgs<-c(bgs,"lightblue")
			cexs<-c(cexs,1.3)
			}
		legend("bottomright",l1,pch=pts,pt.bg=bgs,pt.cex=cexs,inset=.05,bg="white",cex=.9)
		}
	}
#armap.tot("P08","Porc",noval=FALSE,CTDs=TRUE,arrow=FALSE,leg=TRUE)
#armap.tot("P06",cols=FALSE,arrow=FALSE,leg=FALSE,es=TRUE,CTDs=FALSE)
#title("Porcupine 2006",line=2.5)
