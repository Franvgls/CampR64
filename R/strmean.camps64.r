#' Media estratificada total del área de la especie X en varias campañas
#' 
#' Similar a strmean.dtt media ponderada al tamaño del estrato pero para varias campañas
#' @param x Es el vector de abundancias en peso o número de más de una campaña
#' @param sector Es la asignación al sector de cada muestra igual que en strmean {\link{strmean}} pero ha de tener dos caracteres, primero el sector geográfico y segundo el estrato batimétrico
#' @param area El área del sector al que pertenece cada lance
#' @param w Es un factor de ponderación para cada uno de los valores de x. Sirve para la función de la librería boot boot::boot(x,strmean,R,stype="f",Porc.map=sector,sector=sector,area=area,camps=camps) 
#' @param camps Son las campañas a que pertenece cada lance
#' @family Calculos internos no mostrado
#' @return Devuelve la media estratificadda ponderada al área para cada una de las campañas solicitadas
#' @export              
strmean.camps64<-function(x,sector,area,w=rep(1,length(x)),camps) {
  dummy<-tapply(x*w,data.frame(sector,camps),mean,na.rm=TRUE)
  area.dumb<-tapply(area,data.frame(sector,camps),mean,na.rm=TRUE)
  mean.dumb<-colSums(dummy*area.dumb,na.rm=TRUE)/colSums(area.dumb,na.rm=TRUE)
  unname(mean.dumb)
}
