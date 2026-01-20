#'Transforma grados, minutos y segundos a grados decimales
#'
#'Transforma grados y minutos decimales a grados decimales, avisa si el dato es incorrecto, es decir minutos o segundos más de 60
#'@param x grados, minuto y segundos en formato "GGG:MM:SS", vector de caracter
#'@examples gradms("43:14:25")
#'@family Conversion unidades
#'@export
gradms<- function(x) {
  dms<-do.call(rbind, strsplit(as.character(x), ":"))
  if (any(as.numeric(dms[2:3])>60)) stop("Los datos en minutos y segundos no pueden ser más de 60, revise datos")
  dec<-as.numeric(dms[,1])+(as.numeric(dms[,2])+as.numeric(dms[,3])/60)/60
  dec
}
