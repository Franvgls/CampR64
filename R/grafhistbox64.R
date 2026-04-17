#' Evolución en biomasa o abundancia de la especie en la serie histórica
#' 
#' Gráfico de cajas con la evolución en biomasa o abundancia en la serie histórica
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Codigo de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camps campañas de la serie de datos a representar en el gráfico de abundancias Demersales Nsh, Porcupine Psh, Arsa primavera As1 y Arsa otoño As2
#' @param dns Elige el origen de las bases de datos: Porcupine "Pnew" o "Porc", Cantábrico "Cant" o "Cnew", Golfo de Cádiz "Arsa" y Mediterráneo "Medi" P
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T el gráfico está en kgs, si F en gramos
#' @param ci.lev El intervalo de confianza a representar
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param SE Si T dibuja las cajas representando el error estándar, si F no aparecen la cajas
#' @param es Si T ejes y unidades en español, si F en inglés
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sector Alternativa a excl.sect para un sólo sector. Si especificado como carácter solo toma el sector elegido
#' @param ti Si T muestra el título con el nombre de la especie (y el sector si se elige sect o excl.sect si falso no muestra nada. También se puede dar un valor y aparece el texto incluido o dar un list(label=(título), font, cex)
#' @param DLS si T muestra las medias de los dos últimos años y de los cinco precedentes
#' @param DLSrat ratio Elige el numero de años para el enfoque DLS 3/2 short life, 5/2 long life, vector con dos números, intervalo reciente intervalo histórico
#' @param Nas Si F no representa las cajas en los sectores/estratos en los que algún sector o estrato tiene un sólo lance. Si T utiliza el valor del estrato y evita los NAs
#' @param ymax Valor máximo del eje y
#' @param mar Sirve para definir los margenes de la figura alrededor del gráfico mismo. Por defecto es NA y deja los definicos por defecto por R. Si se definen tiene que ser un vector con cuatro valores numéricos en líneas: c(bottom,left,upper,right)
#' @param tline Si T dibuja una línea de tendencia a traves de un glm con los datos de abundancia. gráficos evaluación MSFD.
#' @param years Si T saca los años como nombre de campaña en el eje de las equis en vez de de campañas
#' @param sub Añade un subtítulo con el valor que se le ponga si no F
#' @param cex.leg Sirve para modificar los tamaños de letra del gráfico, funciona con ejes y títulos. Por defecto en 1.1
#' @param las Sirve para modificar la dirección de las etiquetas en el eje de las equis, por defecto F pero se cambia cuando se elige years=T, si no se cambia salen paralelas
#' @param nboot Número de réplicas bootstrap. Reducir (ej. 200) para exploración rápida, 1000 para resultados finales.
#' @return Crea una gráfica de evolución de las abundancias en biomasa o número y devuelve en consola un data.frame con columnas: avg,SE (error estándar),camp
#' @seealso {\link{grafhistbox.comp}}
#' @examples grafhistbox(1,45,Nsh[7:27],"Cant",es=FALSE,years=TRUE,tline=TRUE,DLS=FALSE,ti=TRUE,sub=TRUE)
#' @examples grafhistbox(1,45,Nsh[1:31],"Cant",es=FALSE,years=TRUE,tline=FALSE,DLS=TRUE,ti=TRUE,sub=TRUE)
#' @export
grafhistbox64<-function(gr,esp,camps,zona="porc",dns=c("local","serv"),ind="p",cor.time=TRUE,kg=TRUE,ci.lev=.8,DLS=F,DLSrat=c(2,5),idi="l",SE=TRUE,
  es=TRUE,excl.sect=NA,sector=NA,ti=TRUE,Nas=FALSE,ymax=NA,mar=NA,tline=FALSE,years=TRUE,sub=FALSE,cex.leg=1.1,nboot=1000) {
  options(scipen=2)
  if (length(sector)>1) {
    stop("Para calcular más de un sector utilice excl.sect quitando los no deseados")
    } 
  if (tline & DLS) {stop("Elija línea de tendencia tline=T o cambios últimos 2 años frente a 3 previos DLS=T")}
  # Guardar y restaurar par de forma segura: par() devuelve los valores anteriores al establecer nuevos
  if (any(is.na(mar))) {
    op <- par(mar=c(4, 4.5, 2.5, 2.5) + 0.1)
  } else {
    op <- par(mar=mar, mgp=c(2.8,.8,0))
  }
  on.exit(par(op), add=TRUE)
	ndat<-length(camps)
	dumb<-NULL
	dumbSETot<-data.frame(avg=NULL,SE=NULL,camp=NULL)
	for (i in 1:ndat) {
		dumb<-rbind(dumb,cbind(datos.camp64(gr,esp,camps[i],zona,dns,cor.time=cor.time,kg=kg),camp=camps[i]))
		dumbSE<-CV.camp64(gr,esp,camps[i],zona,dns,cor.time=cor.time,Nas=Nas,ind=ind,excl.sect=excl.sect)
		if (is.na(sector)) {
			dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSE$total[1],SE=dumbSE$total[2],camp=camps[i]))
			}
		else {
			if (nchar(sector)==2) {
				dumbSect<-dumbSE$locales[,which(colnames(dumbSE$locales)==sector)]
				dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSect[1],SE=dumbSect[2],camp=camps[i]))
				}
			else {
				if (!is.na(match(sector,colnames(dumbSE[[2]]))>0)) {
					dumbSect<-dumbSE[[2]][,which(colnames(dumbSE[[2]])==sector)]
					dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSect[1],SE=dumbSect[2],camp=camps[i]))
					}
				if (!is.na(match(sector,colnames(dumbSE[[3]])))) {
					dumbSect<-dumbSE[[3]][,which(colnames(dumbSE[[3]])==sector)]
					dumbSETot<-rbind(dumbSETot,data.frame(avg=dumbSect[1],SE=dumbSect[2],camp=camps[i]))
					}
				}
			}
		}
	if (any(!is.na(excl.sect))) {
		# print(dumb)
	  dumb$sector<-gsub("NA","N",dumb$sector) 
		for (i in 1:length(excl.sect)) {if(length(grep(excl.sect[i],as.character(dumb$sector)))>0) dumb<-dumb[-grep(excl.sect[i],as.character(dumb$sector)),]}
		dumb$sector<-factor(as.character(dumb$sector))
		}
	dumb$camp<-factor(dumb$camp)
	dumbSETot$camp<-factor(dumbSETot$camp)
	# Número de núcleos para paralelizar bootstrap (deja uno libre)
	ncpus <- max(1L, parallel::detectCores() - 1L)
	if (ind=="p") {
		if (any(is.na(sector))){
			if (ci.lev>0) dumb.env<-boot::envelope(boot::boot(dumb$peso,strmean.camps64,nboot,stype="f",
				strata=factor(dumb$sector),sector=dumb$sector,
				area=dumb$arsect,camps=dumb$camp,
				parallel="snow",ncpus=ncpus),level=ci.lev)
			dumb.mean<-strmean.camps64(dumb$peso,dumb$sector,dumb$arsect,camps=dumb$camp)}
		else {
			if (ci.lev>0) dumb.env<-boot::envelope(boot::boot(dumb$peso[grep(sector,as.character(dumb$sector))],strmean.camps64,nboot,stype="f",
				strata=factor(dumb$sector[grep(sector,as.character(dumb$sector))]),sector=dumb$sector[grep(sector,as.character(dumb$sector))],
				area=dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))],
				parallel="snow",ncpus=ncpus),level=ci.lev)
			dumb.mean<-strmean.camps64(dumb$peso[grep(sector,as.character(dumb$sector))],dumb$sector[grep(sector,as.character(dumb$sector))],
				dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))])}
			yetiq<-ifelse(es,expression("Yst"~~("kg"%*%"lan"^-1)),expression("kg"%*%"haul"^-1))
		}
	else {
		if (is.na(sector)){
			if (ci.lev>0) dumb.env<-boot::envelope(boot::boot(dumb$num,strmean.camps64,nboot,stype="f",
				strata=factor(dumb$sector),sector=dumb$sector,  # <-- factor() añadido
				area=dumb$arsect,camps=dumb$camp,
				parallel="snow",ncpus=ncpus),level=ci.lev)
		dumb.mean<-strmean.camps64(dumb$num,dumb$sector,dumb$arsect,camps=dumb$camp)}
		else {
			if (ci.lev>0) dumb.env<-boot::envelope(boot::boot(dumb$num[grep(sector,as.character(dumb$sector))],strmean.camps64,nboot,stype="f",
				strata=factor(dumb$sector[grep(sector,as.character(dumb$sector))]),sector=dumb$sector[grep(sector,as.character(dumb$sector))],
				area=dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))],
				parallel="snow",ncpus=ncpus),level=ci.lev)
			dumb.mean<-strmean.camps64(dumb$num[grep(sector,as.character(dumb$sector))],dumb$sector[grep(sector,as.character(dumb$sector))],
				dumb$arsect[grep(sector,as.character(dumb$sector))],camps=dumb$camp[grep(sector,as.character(dumb$sector))])}
			yetiq<-ifelse(es,expression("Ind"%*%"lan"^-1),expression("Ind"%*%"haul"^-1))
		}
	xetiq<-ifelse(es,ifelse(years,"Año","Campaña"),ifelse(years,"Year","Survey"))
  especie<-buscaesp64(gr,esp,zona,dns,idi)
	if (is.na(ymax)) ymax<-max(.5,ifelse(ci.lev>0,max(dumb.env$point[1,]),max(dumbSETot$SE+dumbSETot$avg,na.rm=TRUE))*1.05)
	plot(dumb.mean,xlab=xetiq,ylab=yetiq,ylim=c(0,ymax),axes=FALSE,cex.lab=cex.leg*.9) 
	rect(-1000,-1000,10^5,10^5,col="white")
	if (is.logical(ti)) {
		if (ti) {title(main=especie,cex.main=1.1*cex.leg,
		  font.main=ifelse((idi!="l" | any(esp=="999")),2,4),line=ifelse(any(is.character(sub),sub),1.5,1))}
		}
	else {title(main=ti,font.main=4,line=1.2,cex.main=1.1*cex.leg)}
	if (is.logical(sub)) {
		if (sub) {title(main=ifelse(ind=="p",ifelse(es,"Biomasa","Biomass"),ifelse(es,"Número","Number")),
		font.main=2,line=.3,cex.main=cex.leg*.9)}
		}
	else title(main=sub,line=.3,font.main=2,cex.main=cex.leg*.9)
	grid(nx=NA,ny=NULL,col=gray(.4))
	if (ci.lev>0) {
    lines(1:ndat,dumb.env$point[1,],lty=1)
  	lines(1:ndat,dumb.env$point[2,],lty=1)
    }
	if (tline & !DLS) abline(lm(dumb.mean~c(1:ndat)),lty=2,col=2,lwd=2)
  if (SE) {
		for (i in 1:ndat) {
			rect(i-.15,dumbSETot$avg[i]+dumbSETot$SE[i],i+.15,dumbSETot$avg[i]-dumbSETot$SE[i],col=gray(.8))
			}
		}
	points(dumbSETot$avg,pch=16,cex=.6,type="p")
	if (DLS & !tline) {
	  points(dumbSETot$avg,pch=16,cex=.6,type="l")
	  dats<-tail(dumbSETot$avg,sum(DLSrat))
    last<-mean(tail(dats,DLSrat[1]))
    prev<-mean(head(dats,DLSrat[2]))
    lines(c(ndat+0.5,ndat-DLSrat[1]+.5),c(last,last),col=2,lwd=3,lty=3)
    lines(c(ndat-DLSrat[1]+.5,ndat-DLSrat[2]-1.5),c(prev,prev),col=2,lwd=3,lty=3)
    }
	box()
	axis(2,las=2,cex.axis=cex.leg*.9)
  if (years) axis(1,at=1:ndat,labels=camptoyear(camps),las=2,cex.axis=cex.leg*.9)
  else axis(1,at=1:ndat,labels=camps,las=1,cex.axis=cex.leg*.9)
	if(ci.lev>0) axis(4,at=dumb.env$point[,ndat],labels=rev(paste(round(dumb.env$k.pt/10,0),"%")),
		tick=FALSE,cex.axis=cex.leg*.6,las=1,line=-.5)
  dumbSETot
	}
# grafhistbox(1,218,Psh,"Porc",es=FALSE)
# grafhistbox(1,218,Psh,"Porc",es=FALSE,years=TRUE)
