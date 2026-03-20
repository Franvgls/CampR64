#' Mapa de la Península Ibérica completo
#'
#' Función auxiliar para sacar mapas de la campaña MEDITS
#' @param xlims Define los limites longitudinales del mapa, los valores por defecto son los del total del ?rea de la campaña
#' @param ylims Define los limites latitudinales del mapa, los valores por defecto son los del total del ?rea de la campaña
#' @param lwdl Ancho de las líneas del mapa
#' @param cuadr Si T saca las cuadrículas de 5x5 millas naúticas
#' @param cuadrMSFD Si T dibuja caudr?cula de 10 millas naúticas utilizada para la evaluaci?n de la estrategia marina (MSFD)
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param bw si T mapa con tierra en gris, si F tierra en color
#' @param ax Si T saca los ejes x e y
#' @param es Si T saca titulos y ejes en español
#' @param wmf Si T saca a fichero metafile Iberia.emf
#' @param places Si T saca ciudades y puntos geográficos de referencia
#' @param escmult =1 aumenta o disminuye el tamaño de las etiquetas y textos menos de 1 lo disminuye, m?s de uno lo aumenta
#' @return Saca en pantalla el mapa y es utilizada por otras funciones
#' @examples MapIberia()
#' @family mapas base
#' @family Medits
#' @export
MapIberia64<-function(xlims=c(-9.72,5),ylims=c(35.9,44.5),lwdl=1,cuadr=FALSE,cuadrMSFD=FALSE,ICESrect=FALSE,bw=F,ax=TRUE,wmf=FALSE,es=TRUE,places=TRUE,escmult=1,new.dev=T) {
  asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
  if (wmf) win.metafile(filename = "Iberia.emf", width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf) par(mar=c(2,2.5,2, 2.5) + 0.3)
  if (!ax) par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,1,0,1))
  if (new.dev) {
    asp <- diff(ylims) / (diff(xlims) * cos(mean(ylims)*pi/180))
    dev.new(width=10, height=10*asp, noRStudioGD=TRUE)
  }
  maps::map(Iberiamap,xlim=xlims,ylim=ylims,type="n",yaxs="i",xaxs="i",fill=T,col=ifelse(bw,"white","lightblue1"))
  #abline(h=seq(35,45,by=1/12),v=seq(-10,5,by=0.089),col=gray(.6),lwd=.6)
  if (ICESrect) abline(h=seq(35,45,by=.5),v=seq(-10,5,by=1),col=gray(.2),lwd=.6)
  if (cuadrMSFD) abline(h=seq(35,45,by=1/6),v=seq(-10,5,by=0.2174213),col=gray(.4),lwd=.5)
  if (bw) {colo="lightgray"}
  else colo="wheat"
  rect(xlims[1]-1,ylims[1]-1,xlims[2]+1,ylims[2]+1,col=ifelse(bw,"white","lightblue1"))
  maps::map(Iberiamap,add=TRUE,fill=TRUE,col=colo,lwd=lwdl)
  if (places) {
    points(c(-6.299667,-6.950833),c(36.53433,37.25833),pch=1,lwd=2,cex=.9)
    text(-6.950833,37.25833,"Huelva",cex=escmult*0.7,font=2,pos=4)
    text(-6.299667,36.53433,"Cádiz",cex=escmult*0.7,font=2,pos=4)
    text(-8.25,39.5,"PORTUGAL",cex=escmult*1.3,font=2,pos=4,srt=90)
    points(-9.1427,38.737,pch=1,lwd=2,cex=.9)
    text(-9.1427,38.737,ifelse(es,"Lisboa","Lisbon"),cex=escmult*0.7,font=2,pos=4,offset=.7)
    legend(-6,mean(par("usr")[3:4]),ifelse(es,"ESPAÑA","SPAIN"),text.font=2,cex=escmult*2,inset=.15,bty="n")
    text(-8.383,43.367,"A Coruña",cex=escmult*0.7,font=2,pos=1)
    points(-8.383,43.367,pch=1,lwd=2,cex=.9)
    text(-8.7167,42.233,"Vigo",cex=escmult*0.7,font=2,pos=4)
    points(-8.7167,42.233,pch=1,lwd=2,cex=.9)
    text(-3.81,43.47,"Santander",cex=escmult*0.7,font=2,pos=1)
    points(-3.81,43.48,pch=1,lwd=2,cex=.9)
    text(-2.934,43.26,"Bilbao",cex=escmult*0.7,font=2,pos=1)
    points(-2.934,43.268,pch=1,lwd=2,cex=.9)
    text(-1.9884,43.3205,"San Sebastian",cex=escmult*0.7,font=2,pos=1)
    points(-1.9884,43.3205,pch=1,lwd=2,cex=.9)
    points(-5.663,43.56,pch=1,lwd=2,cex=.9)
    text(-5.663,43.56,"Gijón",cex=escmult*0.7,font=2,pos=1)
    points(c(-0.3762881,-4.4212655,2.1734035),c(39.4699075,36.721261,41.3850639),cex=.9,pch=1,lwd=2)
    text(-0.3762881,39.4699075,"Valencia",cex=escmult*.7,font=2,pos=2,offset=.3)
    text(-4.4212655,36.721261,"Málaga",cex=escmult*.7,font=2,pos=3,offset=.4)
    text(2.1734035,41.3850639,"Barcelona",cex=escmult*.7,font=2,pos=2,offset=.3)
    text(2.9,39.7,"Mallorca",cex=escmult*.7,font=2)
    text(3.042048,36.5,ifelse(es,"ARGELIA","ALGERIA"),cex=escmult*1.3,font=2,pos=1)
  }
  if (ax) {
     degs = seq(-10,-1,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(1,5,ifelse(abs(diff(xlims))>1,1,.5))
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = c(0)
     alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
     axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
     degs = seq(35,44,ifelse(abs(diff(ylims))>1,1,.5))
     alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
     axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
     axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=escmult*.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    rug(seq(34.5,44.5,by=1),.005,side=2,lwd=lwdl,quiet=TRUE)
    rug(seq(-9.5,5.5,by=1),.005,side=1,lwd=lwdl,quiet=TRUE)
    rug(seq(-9.5,5.5,by=1),.005,side=3,lwd=lwdl,quiet=TRUE)
    rug(seq(34.5,44.5,by=1),.005,side=4,lwd=lwdl,quiet=TRUE)
  }
  box(lwd=lwdl)
  if (wmf) dev.off()
  if (wmf) par(mar=c(5, 4, 4, 2) + 0.1)
}
