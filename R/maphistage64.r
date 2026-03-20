#' Mapa distribución por edad en la campaña
#' 
#' Mapa de la distribución geográfica por edad de la especie en las campañas solicitadas
#' @param gr Grupo de la especie: Solo hay dados de edad para algunos peces y cigala ? 
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña o campañas a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa". 
#' @param dns elige si se trabja con archivos del ordenador ("local") o del servidor ("serv")
#' @param age Edad solicitada 
#' @param plus Edad plus: incluir la edad considerada como plus, solo afecta si se pide como plus la edad solicitada que suma todas las edades mayores
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param n.ots Interno para decir que en la clave no se saca el número de otolitos sino proporciones
#' @param AltAlk Clave talla edad alternativa sin ruta ni extensión, NA por defecto usa la clave de la campaña edadXYY.dbf
#' @param incl2 si T representa los datos de lances especiales, si F los excluye
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Si T añade titulo al mapa, el nombre de la especie en latín
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param es Si T saca los ejes y legendas en español, si F lo saca en inglés
#' @param layout Organización de gráficos en filas columnas c(r,c)
#' @param ceros Añade puntos para los datos igual a 0 si T, si F no incluye x en los valores 0
#' @param years Si T permite sacar los años como nombre de campaña en los paneles lattice de campañas
#' @param mediahora Valor para obtener abundancias por hora si media hora es mayor
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,0,1,2,...,Age+ (número de individuos edad 0,1,2...),camp. Si out.dat=F saca el Gráfico en pantalla o como objeto para combinar con otros Gráficos con print.trellis
#' @examples maphistage("1"," 42","N05","Cant",2,ceros=TRUE,out.dat=TRUE)
#' @family mapas 
#' @family edades 
#' @export
maphistage64<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),age,plus=8,cor.time=TRUE,n.ots=FALSE,AltAlk=NA,incl2=TRUE,bw=TRUE,ti=TRUE,plot=TRUE,
  out.dat=FALSE,ind="n",idi="l",es=TRUE,layout=NA,ceros=FALSE,years=TRUE,mediahora=1) {
  options(scipen=2)
	if (plot) lattice::trellis.par.set(lattice::col.whitebg())
  if (length(esp)>1) {
    stop("Esta función sólo admite una especie")
    }
  #esp<-format(esp,width=3,justify="r")
	ndat<-length(camp)
	dumb<-NULL
	for (i in 1:ndat) {
	  if (!is.null(datagegr.camp64(gr,esp,camp[i],zona,dns,plus,cor.time=cor.time,n.ots=n.ots,AltAlk=AltAlk,incl2=incl2,mediahora=mediahora))) {
	   	anyo<-ifelse(as.numeric(substr(camp[i],2,3))>50,1900,2000)+as.numeric(substr(camp[i],2,3))
  			dumb<-rbind(dumb,cbind(datagegr.camp64(gr,esp,camp[i],zona,dns,plus,cor.time=cor.time,n.ots=n.ots,AltAlk=AltAlk,incl2=incl2,mediahora=mediahora),camp=camp[i]))
  			}
	}
	if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
    }
	dumb$camp<-factor(dumb$camp)
	dumb$numero<-dumb[,age+c(5)] # el dato de n?mero se usa s?lo para sacar en la gr?fica la edad solicitada age
  leyenda<-signif(max(dumb$numero)*.9,1)
	escala<-signif(max(dumb$numero),1)*35/100
	if (is.logical(ti)) {
		if (ti) {titulo<-list(label=paste(buscaesp64(gr,esp,zona,dns,id=idi),"\n",paste(ifelse(es,"Edad","Age"),paste(age,ifelse(plus==age,"+",""),sep="")),sep=""),
		font=ifelse(idi=="l",4,2),cex=1)}
		else {titulo<-NULL}
		}
	else {titulo<-list(label=ti)}
	if (bw & plot) {colo=gray(.1)
		lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
		}
	else colo=4
	if (any(is.na(layout))) {
		if (ndat!=4) layout=c(1,ndat)
		if (ndat==4) layout=c(2,2)
		}
	if (substr(zona,1,4)== "porc") {
		asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,xlab=NULL,ylab=NULL,
			ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=.7,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
			x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
				lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
				grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=gray(.7)))
					if (max(dumb$numero[subscripts],na.rm=TRUE)>0) {
						lattice::panel.xyplot(-12.5,51.2,cex=sqrt((leyenda)/escala),pch=16,col=colo)
						lattice::ltext(-12.5,51.2,labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=.8)
					}
					if (ind=="p") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.35),
						pch=ifelse(dumb$peso[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
					else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
						pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
					})
			}
	if (substr(zona,1,4)=="cant") {
		asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
		mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),main=titulo,xlab=NULL,ylab=NULL,subscripts=TRUE,
			ylim=c(41.82,44.3),aspect=asp,par.strip.text=list(cex=.8,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
			x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=seq(42,44,by=1),rot=90)),as.table=TRUE,
			panel=function(x,y,subscripts=subscripts) {
				lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.4))
				grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
					default.units = "native",gp=grid::gpar(fill=gray(.8)))
					if (max(dumb$numero[subscripts],na.rm=TRUE)>0) {
						lattice::panel.xyplot(rep(-7,3),c(43.,42.60,42.20),cex=sqrt((leyenda)/escala),pch=16,col=colo)
						lattice::ltext(rep(-7,3),c(43.,42.60,42.20),labels=paste(leyenda,ifelse(ind=="p","kg","ind.")),pos=4,offset=1.1,cex=.7)
						}
				if (ind=="p") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$peso[subscripts]>0,sqrt((dumb$peso[subscripts])/escala),.4),
					pch=ifelse(dumb$peso[subscripts]>0,16,19),col=colo)}
				else {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numero[subscripts]>0,sqrt((dumb$numero[subscripts])/escala),.35),
					pch=ifelse(dumb$numero[subscripts]>0,16,ifelse(ceros,4,NA)),col=colo)}
				})
			}
	if (zona=="arsa") {
		asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
		leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-8.1,-5.5),main=titulo,xlab=NULL,ylab=NULL,subscripts=TRUE,
	    ylim=c(35.95,37.30),aspect=asp,par.strip.text=list(cex=cexleg,font=2),par.strip.background=list(col=c(gray(.8))),
      scales=list(alternating=FALSE,tck=c(1,0),cex=cexleg,x=list(at=c(-10:-5),labels=as.character(abs(-10:-5))),y=list(at=seq(35,36,by=1),rot=90)),
      as.table=TRUE,panel=function(x,y,subscripts=subscripts) {
      lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.4))
	    grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[2]],default.units = "native",gp=grid::gpar(fill=gray(.8)))
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
	if (plot) {return(mapdist)}
	if (out.dat) {
    if (years) dumb<-dumbcamp
    #browser()
    if (!ceros) dumb<-dumb[any(rowSums(dumb[,5:c(ncol(dumb)-2)])>0),]
    return(dumb[,1:c(ncol(dumb)-1)])
    }
	else {
    if (!plot) mapdist 
    }
	}