#'Transforma hora hexadecimal en formato HH.MM a hora decimal H.
#'Transforma horas y minutos a horas decimales, avisa si el dato es incorrecto, es decir más de 60 minutos
#'@param x horas y minutos en formato HH.MM, vector de caracter
#'@examples HextoDec("10.54")
#'@export
HextoDec<- function(x) {
  if (any(is.character(x))) x<-as.numeric(x)
  if (any(is.numeric(x) & c(x-trunc(x))>.6)) stop("Los datos en minutos no pueden ser más de 60, revise datos")
  dec<-trunc(x)+(x-trunc(x))/.6
  dec
}
