#' Gráfico de burbujas de abundancia por edad y campaña
#'
#' Genera un panel de 5 gráficos para el análisis de abundancia a la edad
#' a lo largo de una serie histórica de campañas: abundancia absoluta,
#' proporción por edad, ambas estandarizadas por la mediana de la serie
#' (azul = sobre la mediana, rojo = bajo), y abundancia de la edad de
#' reclutamiento a lo largo del tiempo.
#'
#' @details
#' Requiere que todas las campañas de `camps` tengan claves talla-edad (ALK)
#' disponibles para la especie. La campaña `"N87"` se trata como `NA`
#' (sin datos) por ausencia de muestreo de edades ese año.
#'
#' La edad `plus` agrupa todas las edades superiores. El tamaño de las
#' burbujas es proporcional a `sqrt(n/max(n))`.
#'
#' @param gr Grupo taxonómico (solo peces — requiere ALK).
#' @param esp Código numérico de especie. Solo admite una especie.
#' @param camps Vector de campañas de la serie histórica (p.ej. `Nsh`).
#'   Todas deben tener ALK disponible.
#' @param zona Zona: `"cant"`, `"porc"`, `"arsa"`, `"medi"`.
#' @param dns Origen de datos: `"local"` o `"serv"`.
#' @param plus Edad plus — las edades superiores se acumulan en esta
#'   clase. Por defecto `8`.
#' @param recr Edad de reclutamiento para el panel inferior de la serie
#'   temporal. Por defecto `0`.
#' @param cor.time Si `TRUE` corrige por duración del lance.
#'
#' @returns Invisiblemente, imprime en consola la matriz de abundancias
#'   edad (filas) × campaña (columnas). El gráfico principal se produce
#'   como efecto lateral.
#'
#' @seealso [logabage.camp64()] para descenso logarítmico por cohorte,
#'   [edadstr.camp64()] para los datos de abundancia a la edad
#'
#' @family edades
#'
#' @examples
#' \dontrun{
#' # Gallo (Lepidorhombus whiffiagonis, esp=43) — Cantábrico
#' bubbage.camp64(gr=1, esp=43, camps=Nsh, zona="cant", dns="local",
#'               plus=8, recr=0)
#'
#' # Bacaladilla (esp=51) — Cantábrico, reclutamiento edad 1
#' bubbage.camp64(gr=1, esp=51, camps=Nsh, zona="cant", dns="local",
#'               plus=8, recr=1)
#'
#' # Gallo whiff (esp=43) — Porcupine
#' bubbage.camp64(gr=1, esp=43, camps=Psh, zona="porc", dns="local",
#'               plus=8, recr=0)
#' }
#' @export
bubbage.camp64 <-function(gr,esp,camps,zona="porc",dns=c("local","server"),plus=8,recr=0,cor.time=TRUE) {
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
