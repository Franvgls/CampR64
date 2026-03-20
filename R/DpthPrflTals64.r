#' Perfil de distribución por profundidad y tallas
#' 
#' Crea un gráfico de perfil de distribución por profundidad de la abundancia de una especie o grupo de especies en un rango de tallas específico a partir de los datos de distribución de tallas tomados en una campaña. También puede sacar los datos en peso pero a partir de las relaciones talla-peso, en vez de a partir de las faunísticas como en DpthPrfl 
#' @param gr Grupo de la especie: 1 peces, 2 crustaceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numerico o caracter con tres espacios. 999 para todas las especies del grupo 
#' @param camps Campañas de las que se obtiene la distribución de profundidades (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige de dónde se cogen los datos, si del ordenador "local" o del servidor "serv" Si está configurado
#' @param tmin Talla mínima
#' @param tmax Talla máxima
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param ind Parámetro a representar, saca los datos en "p"eso o "n"úmero
#' @param sex Permite elegir entre machos(1), hembras(2) o indeterminados(3), NA escoge sin tener en cuenta el sexo
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param xmax Valor máximo del eje x
#' @param nlans Si T añade el número de lances en cada rango de profundidad
#' @param spl Si T incluye una curva spline en el gráfico
#' @param brks Especifica los rangos de profundidad:"Sturges" cada 100 metros, "norte" estratificación de Demersales, "porcupine" estratificación de Porcupine, "FD" cada 50 metros
#' @param tabres Muestra una tabla resumen de la media, total de biomasa o número y frecuencia de la especie por estación según el brks especificado
#' @param tit2 Añade un segundo título al gráfico especificando el rango de tallas
#' @seealso {\link{DpthPrfl}}
#' @examples DpthPrflTals64(1, 50, "N08", "cant","local",10,20,brks = "norte",tabres=TRUE,ind="p")
#' @examples DpthPrflTals64(1,50,"P08","porc","local",10,20,brks="porcupine")
#' @export
DpthPrflTals64<-function(gr,esp,camps,zona="porc",dns="local",tmin=0,tmax=999,cor.time=TRUE,incl2=T,ind="n",sex=NA,es=TRUE,ti=TRUE,idi="l",xmax=NA,
                       nlans=TRUE,spl=FALSE,brks="Sturges",tabres=TRUE,tit2=TRUE) {
  if (length(gr)>1 | any(gr==9)) stop("No se pueden mezclar datos de grupos distintos, solo distintas especies del mismo grupo")
  #  if (chpar)  opar<-par(no.readonly=TRUE)
  #  if (length(wghts)>1) par(mfrow=c(2,2))
  #  par(mar=c(3,3,3,1))
  options(scipen=2)
  values<-c("norte","porcupine","Sturges","scott","FD")
  if (!is.numeric(brks) & any(!brks %in% values)) stop("brks tiene que ser norte, porcupine, valores num?ricos o Sturges como forma de determinar los rangos de profundidad")
  if (length(esp)>1 | any(esp=="999")) {
    if (ind=="p") stop("No se pueden sacar datos en peso de m?s de una especie")
    print("Distintas especies pueden estar medidas en distintas unidades (mm y cm) o a la aleta anal")
    medida<-c("cm")
  }
  else { medida<-ifelse(unid.camp64(gr,esp,zona,dns)[1]==1,"cm","mm") }
  dumb<-maphistal64(gr,esp,camps,zona,dns,tmin,tmax,cor.time=cor.time,incl2=incl2,sex=sex,plot=FALSE,out.dat=TRUE,ind=ind)
  if (ind=="n") {
    if (sum(dumb$numero)==0) {
      stop(paste("La especie",buscaesp64(gr,esp,zona,dns),"no tiene capturas o datos de talla, saque distribuci?n con DpthPrfl"))
    }
  }
  if (ind=="p") {
    if (sum(dumb$peso)==0) {
      stop(paste("La especie",buscaesp64(gr,esp,zona,dns),"no tiene capturas o datos de talla, saque distribuci?n con DpthPrfl"))
    }
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
  else {dumbDatDpth<-hist(rep(dumb$prof,dumb$peso),plot=FALSE,breaks=dumbDpth$breaks)}
  spln<-spline(-dumbDpth$mids,c(dumbDatDpth$counts/dumbDpth$counts),n=201)
  if (all(par("mfrow")==c(1,1))) cex.mn=.8
  else cex.mn=1.1
  if (is.logical(ti)) {
    if (ti) {
      titulo1<-list(buscaesp64(gr,esp,id=idi),font=ifelse(idi==idi,4,2),cex=cex.mn)
      titulo2<-list(paste(tmin,"-",tmax,medida,ifelse(!is.na(sex),sex,"")),font=2,cex=c(cex.mn-.1))
    }
    else {titulo1<-NULL
          titulo2<-NULL}
  }
  else {
    titulo1=ti
    titulo2<-list(paste(tmin,"-",tmax,medida),font=2,cex=cex.mn)
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
  if (tit2) title(main=titulo2,cex.main=.9,line=.8)
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
}
# DpthPrflTals(1, 50, "N08", "Cant",10,20,brks = "norte",tabres=TRUE,ind="p")
# DpthPrflTals(1,50,"P08","Porc",brks="porcupine")
# DpthPrflTals(1,50,"N08","Cant",brks=c(0,70,100,130,160,190,220))
# DpthPrflTals(1,50,"N08","Cant",brks="FD")