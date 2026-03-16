#' Medias intraestrato y estratificadas por estratos geográfico y batimétrico
#' 
#' Función interna para cálculos y aplicación de bootstrap
#' @param x Es el vector de abundancias en peso o número
#' @param sector Es la asignación al sector de cada muestra igual que en strmean {\link{strmean}} pero ha de tener dos caracteres, primero el sector geográfico y segundo el estrato batimétrico
#' @param area El área del sector
#' @param w Es un factor de ponderación para cada uno de los valores de x. Sirve para la función de la librería boot boot::boot(x,strmean,R,stype="f",Porc.map=sector,sector=sector,area=area) 
#' @family Calculos internos no mostrado
#' @return Devuelve la media estratificadda ponderada al área dentro de cada estrato y subestrato y la total del área
#' @export
strmean.dtt64<-function(x,sector,area,w=rep(1,length(x))) {
  dummy<-tapply(x*w,sector,mean)
  sumsect<-tapply(dummy*area[2,],as.integer(substr(as.character(area[1,]),1,1)),sum)
  avgsect<-sumsect/tapply(area[2,],as.integer(substr(as.character(area[1,]),1,1)),sum)
  sumestr<-tapply(dummy*area[2,],as.integer(substr(as.character(area[1,]),2,2)),sum)
  avgestr<-sumestr/tapply(area[2,],as.integer(substr(as.character(area[1,]),2,2)),sum)
  avg<-weighted.mean(dummy,area[2,])
  dummy<-c(dummy,avgestr,avgsect,avg)
  dummy
}
