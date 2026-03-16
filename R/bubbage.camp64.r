#' Crea gráficos de burbujas de la abundancia sqrt(num) 
#' 
#' Tipico bubleplot de análisis de VPA y evaluación de poblaciones con gráfico por edad y año
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camps Serie historica de campañas de la que se extraen los datos. Todas las campañas han de tener ALKs para las especie en cuestión
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param recr edad de reclutamiento para especies con reclutamiento en edad 1 o mayor
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @examples bubbage.camp(1,43,Nsh[1:29],"Cant",8,0)
#' @return Saca un lattice de descenso de abundancia logarítmica con la edad con dato de la pendiente para cada año. También saca una matriz de abundancias por años x edad columnas x filas
#' @family edades
#' @export
bubbage.camp64 <-function(gr,esp,camps,zona="porc",dns="local",plus=8,recr=0,cor.time=TRUE) {
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
#  esp<-format(esp,width=3,justify="r")
  ndat<-length(camps)
  dumb<-data.frame(n=NULL,age=NULL,year=NULL,camp=NULL)
  for (i in 1:length(camps)) {
    if (camps[i]=="N87") dumb<-rbind(dumb,data.frame(n=rep(NA,plus+1),age=0:plus,year=1987,camp="N87"))
    else {			
      anyo<-ifelse(as.numeric(substr(camps[i],2,3))>50,1900,2000)+as.numeric(substr(camps[i],2,3))
      dumb<-rbind(dumb,data.frame(n=edadstr.camp64(gr,esp,camps[i],zona,dns,plus,cor.time=cor.time)$total,age=0:plus,year=anyo,camp=camps[i]))
    }
  }
  # Totals by age, median of time series by age, mad time series by age, median proportion by age...
  dumb$sumage<-rep(colSums(tapply(dumb$n,dumb[,c(2,3)],sum)),each=plus+1)
  dumb$mediants<-rep(apply(tapply(dumb$n,dumb[,c(2,3)],sum,na.rm = T),1,median,na.rm=TRUE),ndat)
  dumb$madts<-(rep(apply(tapply(dumb$n,dumb[,2:3],sum),MARGIN=1,FUN=mad,na.rm=TRUE),ndat))
  dumb$medprpage<-rep(apply(tapply(dumb$n/dumb$sumage,dumb[,2:3],sum,na.rm=TRUE),1,median,na.rm=TRUE),ndat)
  dumb$madprpage<-(rep(apply(tapply(dumb$n/dumb$sumage,dumb[,2:3],sum),MARGIN=1,FUN=mad,na.rm=TRUE),ndat))
  op<-par(no.readonly=TRUE)
  par(mar=c(2,2.5,2,1)+.1,cex.main=.8,cex.lab=.7,cex.axis=.6)
  split.screen(c(3,1))
  split.screen(c(1,2),1)
  split.screen(c(1,2),2)
  ## Abundances in number
  screen(4)
  plot(dumb$age~dumb$year,cex=2.5*sqrt(dumb$n/max(dumb$n,na.rm=TRUE)),ylab=NA,xlab=NA,pch=16,axes=FALSE,col=gray(.1))
  title("Abundance at age")                                
  title(xlab="Survey",line=1)
  title(ylab="Age",line=1.5)
  axis(1,cex.axis=.7,las=1,tck=-0.03,padj=-2)
  axis(2,at=0:c(plus),labels=c(as.character(0:c(plus-1)),paste(plus,"+",sep="")),tck=-.03,hadj=-.1,las=2)
  box()
  # Proportion at age
  screen(5)
  sizer<-max(dumb$n/dumb$sumage,na.rm=TRUE)
  plot(dumb$age~dumb$year,cex=2.5*sqrt((dumb$n/dumb$sumage)/sizer),ylab=NA,xlab=NA,pch=16,axes=FALSE,
       col=gray(.1))
  title("Proportion at age")
  title(xlab="Survey",line=1)
  title(ylab="Age",line=1.5)
  axis(1,cex.axis=.7,las=1,tck=-.03,padj=-2)
  axis(2,at=0:c(plus),labels=c(as.character(0:c(plus-1)),paste(plus,"+",sep="")),tck=-.03,hadj=-.1,las=2)
  box()
  # Abundances standardized with the median (abund-median(ts))
  screen(6)
  sizer<-max(abs(dumb$n-dumb$mediants),na.rm=TRUE)
  plot(dumb$age~dumb$year,ylab=NA,xlab=NA,col=ifelse((dumb$n-dumb$mediants)>=0,"blue","red"),
       cex=ifelse(abs(dumb$n-dumb$mediants)>0,2.5*sqrt(abs(dumb$n-dumb$mediants)/(sizer)),.5),axes=FALSE,
       pch=ifelse(abs(dumb$n-dumb$mediants)>0,16,3))
  title("Median-Standardized abundance at age")
  title(xlab="Survey",line=1)
  title(ylab="Age",line=1.5)
  axis(1,cex.axis=.7,las=1,tck=-.03,padj=-2)
  axis(2,at=0:c(plus),labels=c(as.character(0:c(plus-1)),paste(plus,"+",sep="")),tck=-.03,hadj=-.1,las=2)
  box()
  # Proportions at age standardized with the median
  screen(7)
  sizer<-max(abs(dumb$n/dumb$sumage-dumb$medprpage),na.rm=TRUE)
  plot(dumb$age~dumb$year,ylab=NA,xlab=NA,col=ifelse((dumb$n/dumb$sumage-dumb$medprpage)>=0,"blue","red"),
       cex=ifelse(abs((dumb$n/dumb$sumage)-dumb$medprpage)>0,2.5*sqrt(abs((dumb$n/dumb$sumage)-dumb$medprpage)/sizer),.5),
       pch=ifelse(abs((dumb$n/dumb$sumage)-dumb$medprpage)>0,16,3),axes=FALSE)
  title("Median-Standardized proportion at age")
  title(xlab="Survey",line=1)
  title(ylab="Age",line=1.5)
  axis(1,cex.axis=.7,las=1,tck=-.03,padj=-2)
  axis(2,at=0:c(plus),labels=c(as.character(0:c(plus-1)),paste(plus,"+",sep="")),tck=-.03,hadj=-.1,las=2)
  box()
  # abundance of recruitment (variable recr in function) along the time series
  screen(3)
  par(mar=c(2.5,2.5,1.5,1)+.1)
  agex<-tapply(dumb$n,dumb[,c(2,3)],sum)[recr+1,]
  ymax<-max(agex,na.rm=TRUE)*1.05
  plot(as.numeric(colnames(tapply(dumb$n,dumb[,c(2,3)],sum))),agex,ylab=NA,xlab=NA,pch=16,
       type="o",ylim=c(0,ymax),axes=FALSE)
  title(paste("Abundance at age",recr),cex.main=.9)
  title(xlab="Survey",line=1)
  title(ylab="Ind./30 min haul ",line=1.5)
  axis(1,cex.axis=.7,las=1,tck=-.03,padj=-2)
  axis(2,tck=-.03,padj=1)
  box()
  close.screen(all = TRUE)    # exit split-screen mode
  par(op)
  print(tapply(dumb$n,dumb[,c(2,3)],sum))
  print(dumb)
}
