#' Mapa de las cuadrículas sorteadas para la campaña Porcupine
#'  
#' Mapa del resultado de un sorteo de cuadriculas de Porcupine, con simbolos diferentes para cada sector/estrato
#' @param x El resultado de un sorteo mediante la función sorteo(). Puede ser un objeto con la misma estructura que el resultado de utilizar sorteo {\link{sorteo}}
#' @param wmf si T guarda el resultado en un metafile, si F saca el gráfico en pantalla
#' @param ax si T saca los ejes en el gráfico, si F queda solo el grid, útil para sacar metafiles a utilizar en el PescaWin
#' @param nomfic Especifica el nombre del fichero metafile de salida por defecto Porco.emf si wmf es T
#' @param lwdl Ancho de las líneas que marcan los estratos en el mapa (2 suelen verse mejor)
#' @param colo Color de las líneas del mapa
#' @param add Si T añade los puntos al gráfico en pantalla, si F dibuja un mapa nuevo
#' @param labs Si F dibuja puntos, si T saca el número de las cuadriculas en su cuadrícula correspondiente
#' @param col Color de los puntos del sorteo en x
#' @param cex Tamaño del texto de las etiquetas de cuadrícula si labs=TRUE
#' @seealso {\link{sorteo}}
#' @export
mapsorteo<-function(x,wmf=FALSE,ax=TRUE,nomfic="porco.emf",lwdl=.5,colo=NA,add=FALSE,labs=FALSE,col=2,cex=.9) {
  asp<-diff(c(50.5,54.5))/(diff(range(-15.5,-10.5))*cos(mean(50.5,54.5)*pi/180))
  if (wmf) win.metafile(filename = nomfic, width = 10, height = 10*asp+.63, pointsize = 10)
  if (!wmf & ax) par(mar=c(2,2.5,2, 2.5) + 0.3)
  if (!ax) {par(mar=c(0,0,0,0),oma=c(0,0,0,0),omd=c(0,0,0,0))}
  if (!add)	MapPorc64(lwdl=lwdl,cuadr=TRUE,ax=ax,corners=TRUE)
  if (!labs) {
    for (nstrat in 1:length(Porc.map$names)) {
      points(x[x[,4]==substr(Porc.map$names[nstrat],1,2),1:2],pch=18+nstrat,col=1,bg=colo,cex=.9,lwd=2)
    }
  }
  else text(y~x,? x,labels=x$pt,cex=cex,font=2,col=col)
  if (wmf) dev.off()
  par(mar=c(5, 4, 4, 2) + 0.1)
}
