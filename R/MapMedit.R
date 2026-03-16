#' Mapa del Mediterráneo Ibérico completo
#'
#' Función auxiliar para sacar mapas de la campaña MEDITS
#' @param xlims Define los limites longitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param ylims Define los limites latitudinales del mapa, los valores por defecto son los del total del área de la campaña
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param cuadrMSFD Si T dibuja caudrícula de 10 millas naúticas utilizada para la evaluación de la estrategia marina (MSFD)
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ICESrectcol Color para los rectángulos ICES
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param bw si T mapa con tierra en gris, si F tierra en color
#' @param ax Si T saca los ejes x e y
#' @param es Si T saca titulos y ejes en español
#' @param wmf Si T saca a fichero metafile Meditconc.emf
#' @param places Si T saca ciudades y puntos geográficos de referencia
#' @return Saca en pantalla el mapa y es utilizada por otras funciones
#' @examples MapMedit()
#' @family mapas base
#' @family Medits
#' @export
MapMedit<-function(xlims=c(-5.7,5),ylims=c(35,43),lwdl=1,cuadr=FALSE,cuadrMSFD=FALSE,ICESrect=FALSE,ICESrectcol=gray(.2),ICESlab=FALSE,ICESlabcex=.5,bw=F,ax=TRUE,wmf=FALSE,es=TRUE,places=TRUE) {
  asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
  if (wmf) win.metafile(filename = "Meditconc.emf", width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf) par(mar=c(2,2.5,2, 2.5) + 0.3)
  if (!ax) par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))
  maps::map(Medits.tot,xlim=xlims,ylim=ylims,type="n",yaxs="i",xaxs="i")
  if (!bw) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=ifelse(bw,"white","lightblue1"))
  if (cuadr) {
    abline(h=seq(35,43,by=1/12),col=gray(.6),lwd=.6)
    abline(v=seq(-6,5,by=0.089),col=gray(.6),lwd=.6)
  }
  if (ICESlab) text(c(stat_y+.22)~stat_x,Area,label=ICESNAME,cex=ICESlabcex,font=2)
  if (ICESrect) {
    abline(h=seq(35,43,by=.5),col=ICESrectcol,lwd=.6)
    abline(v=seq(-6,5,by=1),col=ICESrectcol,lwd=.6)
  }
  if (cuadrMSFD) {
    abline(h=seq(35,43,by=1/6),col=gray(.4),lwd=.5)
    abline(v=seq(-6,5,by=0.2174213),col=gray(.4),lwd=.5)
  }
  if (bw) {colo="lightgray"}
  else colo="wheat"
  maps::map(Medits.tot,add=TRUE,fill=TRUE,col=colo,lwd=lwdl)
  if (places) {
    points(c(-0.3762881,-4.4212655,2.1734035),c(39.4699075,36.721261,41.3850639),pch=20)
    text(-0.3762881,39.4699075,"Valencia",cex=.7,font=2,pos=2,offset=.3)
    text(-4.4212655,36.721261,"Málaga",cex=.7,font=2,pos=3,offset=.4)
    text(2.1734035,41.3850639,"Barcelona",cex=.7,font=2,pos=2,offset=.3)
    text(2.9,39.7,"Mallorca",cex=.6,font=2)
    text(3.042048,36.5,"Argelia",cex=1,font=2,pos=1)
  }
  if (ax) {
     degs = seq(-6,-1,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(1,5,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = c(0)
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(35,43,ifelse(abs(diff(ylims))>1,1,.5))
     alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
     axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
     axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    rug(seq(34.5,43.5,by=1),.005,side=2,lwd=lwdl,quiet=TRUE)
    rug(seq(-7.5,5.5,by=1),.005,side=1,lwd=lwdl,quiet=TRUE)
    rug(seq(-7.5,5.5,by=1),.005,side=3,lwd=lwdl,quiet=TRUE)
    rug(seq(34.5,43.5,by=1),.005,side=4,lwd=lwdl,quiet=TRUE)
#    axis(2,at=35:43,labels=paste(35:43,"º",sep=""),las=1,tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.7),tck=FALSE,mgp=c(3,.3,0))
#    rug(35:43,.01,side=2,lwd=lwdl,quiet=TRUE)
#    axis(1,at=c(-6:5),labels=c(paste(6:1,"ºW",sep=""),0,paste(1:5,"ºE",sep="")),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.7),tck=FALSE,mgp=c(3,.3,0))
#    rug(-6:5,.01,side=1,lwd=lwdl,quiet=TRUE)
#    axis(3,at=c(-6:5),labels=c(paste(6:1,"ºW",sep=""),0,paste(1:5,"ºE",sep="")),tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.7),tck=FALSE,mgp=c(3,.3,0))
#    rug(-6:5,.01,side=3,lwd=lwdl,quiet=TRUE)
#    rug(-6:5,.01,side=3,lwd=1,quiet=TRUE)
#    rug(seq(-6,5,by=.5),.005,side=3,lwd=lwdl,quiet=TRUE)
#    axis(4,at=35:43,labels=paste(35:43,"º",sep=""),las=1,tcl=NA,cex.axis=ifelse(par()$family=="serif",1,.7),tck=FALSE,mgp=c(3,.3,0))
#    rug(35:43,.01,side=4,lwd=lwdl,quiet=TRUE)
  }
  box(lwd=lwdl)
  if (wmf) dev.off()
  if (wmf) par(mar=c(5, 4, 4, 2) + 0.1)
}
