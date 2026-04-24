#' Mapas de distribucion en varias campañas
#'
#' Crea mapas con la distribución en biomasa o numero para distintas zonas: Porcupine (dns="Pnew"), el Cantábrico (dns=Cant), Cádiz= (dns=Arsa), y el Mediterráneo (dns=Medi) 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros, 9 escoge todos los orgánicos pero excluye desechos
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camps Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o  "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param sub Añade un subtítulo debajo del gráfico, sin texto por defecto.
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param leg Si T añade la leyenda
#' @param pts Saca los puntos de los datos
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,peso.gr,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples 
#' maphist(1,50,Nsh[7:27],"Cant",layout=c(3,7),years=TRUE)
#' maphist(1,50,As2[15:18],"Arsa",layout=c(2,2),years=TRUE,sub="ARSA 2nd quarter")
#' @family mapas
#' @export
maphist64<-function(gr,esp,camps,zona="cant",dns=c("local","serv"),cor.time=TRUE,incl2=TRUE,bw=TRUE,ti=TRUE,sub=NULL,plot=TRUE,out.dat=FALSE,ind="p",idi="l",
  layout=NA,leg=TRUE,pts=FALSE,ceros=TRUE,escmult=.25,cexleg=1,years=TRUE) {
  if (all(!pts & !leg & length(camps)>1)) {stop("Solo estaciones se usa para sólo una campaña, ha incluido más de una")}
  options(scipen=2)
  #esp<-format(esp,width=3,justify="r")
  colo<-ifelse(bw,gray(.1),4)
  if (plot) {
    #lattice::trellis.par.set("strip.text"=list(cex=.9,font=2))
    #if (bw)   # 
	 if (bw) {
      lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
      colo=gray(.1)
      }
    else {
      lattice::trellis.par.set(lattice::col.whitebg())
      colo=4
      }
		}
  ndat<-length(camps)
	dumb<-NULL
	for (i in 1:ndat) {
    tempdumb<-datgr.camp64(gr,esp,camps[i],zona,dns,cor.time=cor.time,incl2=incl2)
		if (!is.null(tempdumb)) dumb<-rbind(dumb,cbind(tempdumb,camp=camps[i]))
	}
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
    }
	dumb$camp<-factor(dumb$camp)
  if (ind=="p") {
		dumb$peso<-dumb$peso.gr/1000
		leyenda<-signif(max(dumb$peso)*.9,1)
		escala<-signif(max(dumb$peso),1)*escmult }
	else {
		leyenda<-signif(max(dumb$numero)*.9,1)
		escala<-signif(max(dumb$numero),1)*escmult }
	if (is.logical(ti)) {
		if (ti) {titulo<-list(label=buscaesp64(gr,esp,zona,dns,id=idi),font=ifelse((idi=="l" & gr!="9" & esp!="999"),4,2))}
		else {titulo<-NULL}
		}
	else {
    if(is.list(ti)) titulo<-ti
    else titulo<-list(label=ti)
    }
	#browser()
  if (any(is.na(layout))) {
		if (ndat!=4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
		}
  #browser()
	if (out.dat) print(dumb[dumb[,5]>0,])
	if (pts) dumb[dumb[,5]>0,8]<-0
	if (substr(zona,1,4)=="pnew" | substr(zona,1,4)=="porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Porc.map$range[c(1,2)],ylim=Porc.map$range[c(3,4)],xlab=NULL,ylab=NULL,
		  main=titulo,sub=sub,aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-15:-11),labels=as.character(abs(-15:-11))),
			y=list(at=(51:54),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
				lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=gray(.8)))
				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
					lattice::panel.xyplot(-13,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(-13,51.2,labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=cexleg)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
					pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (substr(zona,1,4)=="cant" | substr(zona,1,4)=="cnew") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
    leyenda<-signif(c(1,.5,.25)*leyenda,1)
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Nort.map$range[c(1,2)],ylim=Nort.map$range[c(3,4)],main=titulo,sub=sub,
      xlab=NULL,ylab=NULL,subscripts=TRUE,aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),
      y=list(at=seq(42,44,by=1),rot=90)),as.table=TRUE,
      panel=function(x,y,subscripts=subscripts) {
				lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=gray(.8)))
				if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
					lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=cexleg)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (zona=="arsa") {
		asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Arsa.map$range[c(1,2)],ylim=Arsa.map$range[c(3,4)],main=titulo,sub=sub,
      xlab=NULL,ylab=NULL,subscripts=TRUE,aspect=asp,par.strip.text=list(cex=cexleg,font=2),
      par.strip.background=list(col=c(gray(.8))),scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-8:-5),
      labels=as.character(abs(-8:-5))),y=list(at=seq(35,37,by=1),rot=90)),as.table=TRUE,
      panel=function(x,y,subscripts=subscripts) {
         lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	       grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),
            plot=FALSE)[[2]],default.units = "native",gp=grid::gpar(fill=gray(.8)))
				 if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
          #lrect(-5.98,36.25, -5.54, 36.54,col="white")
					lattice::panel.xyplot(rep(-6,3),c(36.45,36.58,36.7),cex=sqrt((leyenda)/escala),pch=16,col=colo)
					lattice::ltext(rep(-6,3),c(36.45,36.58,36.7),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=.8,cex=cexleg-.1)
					}
				if (ind=="p") {lattice::panel.xyplot(x,y,pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),
					cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
  if (zona=="medi") {
    asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-5.7,5),ylim=c(35,43),main=titulo,sub=sub,xlab=NULL,ylab=NULL,
       subscripts=TRUE,aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
       scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-5:4),labels=c(paste(as.character(abs(-5:-1)),
       "W",sep=""),0,paste(1:4,"E",sep=""))),y=list(at=seq(36,42,by=1),rot=90)),as.table=TRUE,
       panel=function(x,y,subscripts=subscripts) {
          lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
          grid::grid.polygon(maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[1]],maps::map(Medits.tot,Medits.tot$names[],
             plot=FALSE)[[2]],default.units = "native",gp=grid::gpar(fill=gray(.8)))
          if (leg & max(dumb$numero[subscripts],na.rm=TRUE)>0) {
             #lrect(-4.5,38.8,-2.2,40.2,col="white")
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
    dumb$peso<-round(dumb$peso,3)
    if (years) dumb<-dumbcamp
    if (!ceros) dumb<-dumb[dumb$numero>0,]
    return(dumb)
    }
	else {
    if (!plot) mapdist
    }
	}