#' Mapa general del banco de Porcupine
#' 
#' Mapa con la estratificación y el área general del banco de Porcupine incluyendo parte de Irlanda
#' @param es Textos en español, si F en inglés
#' @param leg incluye la leyenda con los colores/patrones de los estratos
#' @param bw Si T mapa en blanco y negro, si F colorea los estratos y sectores
#' @param dens si mayor de 0 las superficies de los estratos tienen patrones de líneas
#' @family mapas
#' @family Porcupine
#' @export
maparea64<-function(es=TRUE,leg=TRUE,bw=FALSE,dens=0,strat=FALSE,places=TRUE,
                    xlims=c(-15.5,-8.2),ylims=c(50.5,54.5)) {
  maps::map("worldHires",c("ireland","UK:Northern Ireland"),ylim=ylims,xlim=xlims,
		fill=TRUE,col="saddlebrown",type="n")
	box()
	rect(xlims[1]-0.5,ylims[1]-0.5,xlims[2]+0.5,ylims[2]+0.5,col=ifelse(bw,"white","lightblue1"))
	abline(v=c(-15:-6),h=c(51:54),lty=3,col=gray(.2))
	maps::map("worldHires",c("ireland","UK:Northern Ireland"),fill=TRUE,col=ifelse(!bw,"wheat","gray95"),add=TRUE)
	detach("package:mapdata")
	if (places) {
	  points(-(9+.0303/.6),(53+.1623/.6),pch=16,col=1)
	  text(-(9+.0303/.6),(53+.1623/.6),label="Galway",pos=3,cex=.7,font=2)
	  text(-(8.95),(52.2),label=ifelse(es,"IRLANDA","IRELAND"),cex=1.3,font=2)
  }	
	if (!bw) colrs=c("Steelblue2","Steelblue2","Steelblue","blue4","green","darkgreen",gray(.7))
	else {
		colrs=c("white","white","white","white","white","white",gray(.7))
		#dens=0
		}
	if (strat) {
	  maps::map(Porc.map,add=TRUE,fill=TRUE,col=colrs)
	} else {
	  maps::map(Porc.map,add=TRUE,fill=FALSE,col=gray(.5),lwd=0.8)
	}
	if (strat) {
	if (dens>0) {
		polygon(maps::map(Porc.map,"1Aa",plot=FALSE)$x,maps::map(Porc.map,"1Aa",plot=FALSE)$y,density=dens)
		polygon(maps::map(Porc.map,"1Ab",plot=FALSE)$x,maps::map(Porc.map,"1Ab",plot=FALSE)$y,density=dens)
		polygon(maps::map(Porc.map,"1B",plot=FALSE)$x,maps::map(Porc.map,"1B",plot=FALSE)$y,density=dens,angle=0)
		polygon(maps::map(Porc.map,"2B",plot=FALSE)$x,maps::map(Porc.map,"2B",plot=FALSE)$y,density=dens,angle=0)
		polygon(maps::map(Porc.map,"2C",plot=FALSE)$x,maps::map(Porc.map,"2C",plot=FALSE)$y,density=dens,angle=135)
		polygon(maps::map(Porc.map,"1C",plot=FALSE)$x,maps::map(Porc.map,"1C",plot=FALSE)$y,density=dens,angle=135)
	   }
	if (leg) {
		rect(-13.2,50.7,-10.3,51.3,col="white")
		rect(-13.05,51.05,-12.8,51.2,col=colrs[1])
		rect(-12.8,51.05,-12.55,51.2,col=colrs[3])
		rect(-12.55,51.05,-12.30,51.2,col=colrs[4])
		rect(-12.9,50.8,-12.65,50.95,col=colrs[5])
		rect(-12.65,50.8,-12.4,50.95,col=colrs[6])
		if (dens>0) {
			rect(-13.05,51.05,-12.8,51.2,col=1,density=15)
			rect(-12.8,51.05,-12.55,51.2,col=1,density=15,angle=0)
			rect(-12.55,51.05,-12.30,51.2,col=1,density=15,angle=135)
			rect(-12.9,50.8,-12.65,50.95,col=1,density=15,angle=0)
			rect(-12.65,50.8,-12.4,50.95,col=1,density=15,angle=135)
		   }
		text(-12.3,(51.2+51.05)/2,label=ifelse(es,"Sector 1 (norte) E, F y G","Sector 1: E, F & G"),pos=4,cex=.8,font=2)
		text(-12.3,(50.8+50.95)/2,label=ifelse(es,"Sector 2 (sur) F & G","Sector 2: F & G"),pos=4,cex=.8,font=2)
	}
	}
	box()
	axis(1,at=seq(-16,-8,by=1),labels=paste(abs(seq(-16,-8,by=1)),"º",sep=""),cex.axis=.8)
	axis(2,at=seq(51,54,by=1),labels=paste(seq(51,54,by=1),"º",sep=""),cex.axis=.8,las=1)
	axis(3,at=seq(-16,-8,by=1),labels=paste(abs(seq(-16,-8,by=1)),"º",sep=""),cex.axis=.8)
	axis(4,at=seq(51,54,by=1),labels=paste(seq(51,54,by=1),"º",sep=""),cex.axis=.8,las=1)
	}
