#' Abundancias, SE y coeficientes de variación paramétricos por subestrato, estrato y total
#'
#' Función de resultados 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo 
#' @param camp Campaña de la que sacar los dato un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param ind Dato a procesar, si (p)eso o (n)umero
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T saca los resultados en Kg, si F en gramos
#' @param Nas Si F el error estándar de los subestratos y estratos con un solo lance son NA. Si T utiliza el valor del estrato y evita los NAs
#' @param excl.sect Excluye los sectores o subsectores dados como caracteres
#' @param verbose si T avisa de que distintas especies pueden pertenecer a grupos distintos y hay que valorar el sendio que tiene mezclarlas
#' @return Devuelve una lista con valorees de media, SE y CV para los subestratos, estratos y total de la campaña
#' @seealso {\link{CV.camps}}, {\link{CV.bt.camp}}
#' @export
CV.camp64<- function(gr,esp,camp,zona,dns="local",ind="p",cor.time=TRUE,kg=TRUE,Nas=FALSE,excl.sect=NA,verbose=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  mm<-datos.camp64(gr,esp,camp,zona,dns,kg=kg,cor.time=cor.time,verbose=verbose)
  if (any(!is.na(excl.sect))) {
    for (i in 1:length(excl.sect)) {mm<-mm[-grep(excl.sect[i],as.character(mm$sector)),]}
    mm$sector<-factor(as.character(mm$sector))
  }
  if (ind=="p") {dumb<-strmean.dt64(mm$peso,mm$sector,mm$arsect,Nas=Nas)}
  else {dumb<-strmean.dt64(mm$numero,mm$sector,mm$arsect,Nas=Nas)}
  CV.camp<-dumb
  dumb
}
