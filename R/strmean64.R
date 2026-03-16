#' Media estratificada total del area de una especie
#' 
#' Función interna para cálculos y aplicación de bootstrap
#' @param x Es el vector de abundancias en peso o número
#' @param sector Es la asignación al sector de cada muestra
#' @param area El área del sector
#' @param w Es un factor de ponderación para cada uno de los valores de x. Sirve para la función de la librería boot boot::boot(x,strmean,R,stype="f",Porc.map=sector,sector=sector,area=area) 
#' @family Calculos internos no mostrado
#' @return Devuelve la media estratificadda ponderada al área de los estratos
#' @export
strmean64<- function(x,sector,area,w=rep(1,length(x))) {
  dummy<-as.vector(tapply(x*w,sector,mean));
  area<-as.numeric(as.character(area))
  area<-as.vector(tapply(area,sector,mean))
  weighted.mean(dummy,area)
}
