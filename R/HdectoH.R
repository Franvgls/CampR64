#'Transforma hora decimal en formato HH.NN a hora hexadecimal HH.MM
#'Transforma horas decimales a horas hexadecimales en horas y minutos
#'@param x horas en formato decimal
#'@examples HdectoH("10.9")
#'@family Conversion unidades
#'@export
HdectoH<- function(x) {
  if (any(is.character(x))) x<-as.numeric(x)
  dec<-trunc(x)+(x-trunc(x))*.6
  trunc(dec*100)/100
}
