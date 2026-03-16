#' Mapa distribución entre tallas tmin y tmax
#'
#' Saca mapas de distribución de una especie entre tallas tmin y tmax para varias campañas  
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados. 6 Desechos y otros inorgánicos no tiene sentido sacar tallas, sólo recogidas en peces, crustáceos decápodos y algunos moluscos
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camps Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param tmin Talla mínima del intervalo de tallas a incluir
#' @param tmax Talla máxima del intervalo de tallas a incluir
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param incl2 Si F no presenta los lances especiales, si T si los tiene en cuenta, pero puede dar problemas por que no puede calcular las abundancias estratificadas
#' @param ind Permite elegir entre número "n" o peso "p" peso sólo funciona cuando existen a y b en especies.dbf y se elige sólo una especie
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param bw gráfico en blanco en negro si T o en color si F
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param sub Añade un subtítulo debajo del gráfico, sin texto por defecto.
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la funcion es la figura en pantalla, pero los datos en objeto
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples maphistal(1,50,Psh[1:12],"Porc",1,23,layout=c(4,3),out.dat=TRUE)
#' @family mapas
#' @family tallas
#' @export
maphistal64<-function(gr,esp,camps,zona="cant",dns="local",tmin=0,tmax=999,cor.time=TRUE,incl2=TRUE,ind="n",sex=NA,bw=TRUE,ti=TRUE,sub=NULL,
  plot=TRUE,out.dat=FALSE,idi="l",layout=NA,leg=TRUE,ceros=TRUE,escmult=.25,cexleg=1,years=TRUE) {
  options(scipen=2)
#	if (plot) 
  if (length(esp)>1 | any(esp=="999")) {
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
    if (!is.na(sex)) {
      stop("No se pueden seleccionar sexo para más de una especie")
      }
    if (ind=="p") {
      stop("No se pueden calcular capturas en peso de un rango de tallas para más de una especie")
      }
    medida<-c("cm")
    }
  else { medida<-ifelse(unid.camp64(gr,esp,zona,dns)[1]==1,"cm","mm") }
  esp<-format(esp,width=3,justify="r")
	ndat<-length(camps)
	dumb<-NULL
	for (i in 1:ndat) {
	  if (!is.null(dattalgr.camp64(gr,esp,camps[i],zona,dns,tmin,tmax,cor.time,incl2=incl2,sex,ind))) dumb<-rbind(dumb,data.frame(dattalgr.camp64(gr,esp,camps[i],zona,dns,tmin,tmax,cor.time=cor.time,incl2=incl2,sex,ind),camp=camps[i]))
    }
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
    }
	dumb$camp<-factor(dumb$camp)
  if (ind=="n") {
    leyenda<-signif(max(dumb$numero,na.rm=TRUE)*.9,1)
    escala<-signif(max(dumb$numero,na.rm=TRUE),1)*escmult
    }
  else {
    leyenda<-signif(max(dumb$peso,na.rm=TRUE)*.9,1)
    escala<-signif(max(dumb$peso,na.rm=TRUE),1)*escmult
    }
	if (is.logical(ti)) {
		if (ti) {
      titulo<-list(label=paste(buscaesp64(gr,esp,zona,dns,id=idi),"\n",tmin,"-",tmax,medida,ifelse(is.na(sex),"",sex)),
        font=ifelse(c(idi=="l" & gr!="9" & esp!="999"),4,2))
      }
		else {titulo<-NULL}
		}
	else {
    if(is.list(ti)) titulo<-ti
    else titulo<-list(label=ti)
    }
	if (bw) {
    colo=gray(.2)
    lattice::trellis.par.set(strip.background = list(col = grey(7:1/8)))
    }
	else {
    colo=4
    lattice::trellis.par.set(lattice::col.whitebg())
    }
  if (any(is.na(layout))) {
		if (ndat!=4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
		}
  if (substr(zona,1,4)=="pnew" | substr(zona,1,4)=="porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
			ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=FALSE,tck=c(1,0),
      cex=cexleg,x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
				lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=gray(.7)))
          if (ind=="n") {
    				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
		  				lattice::panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
			   			lattice::ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			       }
            lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
						  pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
            }
          if (ind=="p") {
    				if (leg & max(dumb$peso[subscripts],na.rm=TRUE)>0) {
		   				lattice::panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
			   			lattice::ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			        }
            lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
						  pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
            }
					})
			}
	if (substr(zona,1,4)=="cant" | substr(zona,1,4)=="cnew") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),main=titulo,sub=sub,xlab=NULL,ylab=NULL,subscripts=TRUE,
			ylim=c(41.82,44.3),aspect=asp,par.strip.text=list(cex=cexleg,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,
			x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
				lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=gray(.8)))
        if (ind=="n") {
				  if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
				    lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt(leyenda/escala),pch=16,col=colo)
		        lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
				    }
  				lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
	         pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
	        }
        else {
          if (leg & max(dumb$peso[subscripts],na.rm=TRUE)>0) {
	   			  lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt(leyenda/escala),pch=16,col=colo)
	 	        lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="n","ind.","kg")),pos=4,offset=1.1,cex=cexleg)
			   	  }
  				lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
	         pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)
          }
				})
			}
	if (zona=="arsa") {
		asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-8.1,-5.5),main=titulo,sub=sub,xlab=NULL,ylab=NULL,subscripts=TRUE,
	    ylim=c(35.95,37.30),aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-10:-5),labels=as.character(abs(-10:-5))),
      y=list(at=seq(35,36,by=1),rot=90)),as.table=TRUE,panel=function(x,y,subscripts=subscripts) {
      lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	    grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[2]],
      default.units = "native",gp=grid::gpar(fill=gray(.8)))
				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
          #lrect(-5.98,36.25, -5.54, 36.54,col="white")
					lattice::panel.xyplot(rep(-6,3),c(36.3,36.4,36.5),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-6,3),c(36.3,36.4,36.5),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (zona=="medit") {
		asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-5.7,5),ylim=c(35,43),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
      subscripts=TRUE,aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-5:4),labels=c(paste(as.character(abs(-5:-1)),
      "W",sep=""),0,paste(1:4,"E",sep=""))),y=list(at=seq(36,42,by=1),rot=90)),as.table=TRUE,
      panel=function(x,y,subscripts=subscripts) {
      lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	    grid::grid.polygon(maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[1]],maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[2]],
      default.units = "native",gp=grid::gpar(fill=gray(.8)))
				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
          #lrect(-5.98,36.25, -5.54, 36.54,col="white")
					lattice::panel.xyplot(rep(-4,3),c(39.1,39.6,40.),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-4,3),c(39.1,39.6,40.),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (plot) {print(mapdist)}
	if (out.dat) {
	  if (ind=="n") dumb$numero<-round(dumb$numero,1)
    if (ind=="p") dumb$peso<-round(dumb$peso,2)
	  if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    print(dumb)
    }
	else {
    if (!plot) mapdist
    }
	}