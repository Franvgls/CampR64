#' Perfil de distribución por profundidad
#' 
#' Crea un gráfico de perfil de distribución por profundidad de la biomasa o número de una especie o grupo de especies a partir de los datos de peso y número de la faunística tomados en una campaña
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camps Campañas de las que se obtiene la distribución de profundidades (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param dns Elige de dónde toma los datos si del ordenador "local" o del servidor "serv"
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param ind Parámetro a representar, saca los datos en "p"eso o "n"úmero
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param xmax Valor máximo del eje x
#' @param nlans Si T añade el número de lances en cada rango de profundidad
#' @param spl Si T incluye una curva spline en el gráfico
#' @param brks Especifica los rangos de profundidad:"Sturges" cada 100 metros, "norte" estratificación de Demersales, "porcupine" estratificación de Porcupine, "FD" cada 50 metros
#' @param tabres Muestra una tabla resumen del total de lances, media y total de biomasa o número y frecuencia de la especie por estrato según el brks especificado
#' @examples DpthPrfl(1, 50, "N08", "Cant",brks = "norte",tabres=TRUE,ind="p")
#' @examples DpthPrfl(1,50,"P08","Porc",brks="porcupine",ti=TRUE)
#' @seealso {\link{DpthPrflTals}}
#' @export
DpthPrfl64 <-function(gr,esp,camps,zona="porc",dns="local",cor.time=TRUE,ind="p",es=TRUE,ti=TRUE,idi="l",xmax=NA,nlans=TRUE,spl=FALSE,brks="Sturges",tabres=TRUE) {
    if (length(gr)>1) stop("No se pueden mezclar datos de grupos distintos, se pueden mezclar todos menos 6, utilizando 9 como grupo")
    #  if (chpar)  opar<-par(no.readonly=TRUE)
    #  if (length(wghts)>1) par(mfrow=c(2,2))
    #  par(mar=c(3,3,3,1))
    options(scipen=2)
    values<-c("norte","porcupine","Sturges","scott","FD")
    #browser()
    if (!is.numeric(brks) & any(!brks %in% values)) stop("brks tiene que ser norte, porcupine, valores num?ricos o Sturges como forma de determinar los rangos de profundidad")
    dumb<-maphist64(gr,esp,camps,zona,dns,cor.time=cor.time,plot=FALSE,out.dat=TRUE,ind=ind)
    if (sum(dumb$numero)==0) {
      stop(paste("La especie",buscaesp64(gr,esp,zona,dns),"no tiene datos de capturas en las campañas seleccionadas, revise por favor"))
    }
    #print(dumb)
    if (length(brks)>1) {
      dumb<-dumb[dumb$prof>brks[1] & dumb$prof<brks[length(brks)],]
      ylims<-c(brks[length(brks)],brks[1])*c(-1)
    }
    else {
      brks<-values[match(brks,values)]
      ylims<-c(-800,0)
    }
    if (any(brks=="porcupine")) {
      brks=c(0,150,300,450,800)
      if (min(dumb$prof)<brks[1] | max(dumb$prof)>brks[5]) stop("Existen lances fuera de los rangos de la campa?a, revise los datos")
    }
    if (any(brks=="norte")) {
      brks=c(0,70,120,200,500,810)
      if (min(dumb$prof)<brks[1] | max(dumb$prof)>brks[6]) stop("Existen lances fuera de los rangos de la campa?a, revise los datos")
    }
    dumbDpth<-hist(dumb$prof,plot=FALSE,breaks=brks)
    if (ind=="n") {dumbDatDpth<-hist(rep(dumb$prof,dumb$numero),plot=FALSE,breaks=dumbDpth$breaks)}
    else {dumbDatDpth<-hist(rep(dumb$prof,dumb$peso.gr/1000),plot=FALSE,breaks=dumbDpth$breaks)}
    spln<-spline(-dumbDpth$mids,c(dumbDatDpth$counts/dumbDpth$counts),n=201)
    if (all(par("mfrow")==c(1,1))) cex.mn=.8
    else cex.mn=1.1
    if (is.logical(ti)) {
      if (ti) {
        titulo1<-list(buscaesp64(gr,esp,zona,dns,id=idi),font=ifelse(idi==idi,4,2),cex=cex.mn)
      }
    }
    else {
      titulo1=ti
    }
    # browser()
    dumbDpth$counts1<-dumbDpth$counts
    if (any(dumbDpth$counts==0)) {
      count0<-which(dumbDpth$counts==0)
      dumbDpth$counts[count0]<-1
    }
    #  browser()
    plot(c(-dumbDpth$breaks,-ylims[2])~c(0,c(dumbDatDpth$counts/dumbDpth$counts),0),type=c("s"),xlim=c(0,ifelse(is.na(xmax),max(spln$y)*1.05,xmax*1.05)),
         ylim=ylims,xlab=NA,ylab=ifelse(es,"Prof (m)","Depth (m)"),axes=FALSE,
         pch=21,bg="white",cex.lab=cex.mn)
    if (ti) title(main=titulo1,line=1.8)
    points(c(-dumbDpth$mids)~c(dumbDatDpth$counts/dumbDpth$counts),type="p",pch=21)
    ceros<-rep(0,length(dumbDpth$mids)+1)
    segments(ceros,c(-dumbDpth$breaks,-ylims[2]),c(0,c(dumbDatDpth$counts/dumbDpth$counts),0),c(-dumbDpth$breaks,-ylims[2]))
    if (ind=="n") {title(xlab=ifelse(es,expression("ind."%*%"lan"^-1),expression("ind"%*%"haul "^-1)),cex.lab=cex.mn)}
    else{title(xlab=ifelse(es,expression("Yst"("kg"%*%"lan"^-1)),expression("kg"%*%"haul "^-1)),cex.lab=cex.mn)}
    #	print(spln)
    if (spl) lines(spln$y,spln$x, col = 2,lty=2,lwd=.1)
    #  browser()
    if (nlans) {
      text(c(-dumbDpth$mids)~c(dumbDatDpth$counts/dumbDpth$counts),
           labels=dumbDpth$counts1,cex=.7,font=2,pos=4)
    }
    axis(1,cex.axis=cex.mn-.1)
    if (any(brks %in% values)) axis(2,at=seq(0,-800,by=-100),seq(0,800,100),las=2,cex.axis=cex.mn-.1)
    else axis(2,at=-brks,labels=brks,las=2,cex.axis=cex.mn-.1)
    box()
    dumb<-cbind(dumb,strat=cut(dumb$prof,dumbDpth$breaks))
    if (tabres) {
      if (ind=="n") {
        dumb0<-dumb[dumb$numero>0,]
        nlans<-tapply(dumb$numero,dumb$strat,length)
        dlans<-tapply(dumb0$numero,dumb0$strat,length)
        totstr<-tapply(dumb0$numero,dumb0$strat,sum,na.rm=TRUE)
        avgstr<-tapply(dumb$numero,dumb$strat,mean,na.rm=TRUE)
      }
      if (ind=="p") {
        dumb0<-dumb[dumb$peso>0,]
        nlans<-tapply(dumb$peso,dumb$strat,length)
        dlans<-tapply(dumb0$peso,dumb0$strat,length)
        totstr<-tapply(dumb0$peso,dumb0$strat,sum,na.rm=TRUE)
        avgstr<-tapply(dumb$peso,dumb$strat,mean,na.rm=TRUE)
      }
      #    par(opar)
      resumen<-data.frame(lans=nlans,totstr=totstr,meanstr=avgstr,frecuencia=dlans)
      resumen
    }
  # DpthPrfl(1,50,"N08","Cant",brks=c(0,70,100,130,160,190,220))
  # DpthPrfl(1,50,"N08","Cant",brks="FD")
  }