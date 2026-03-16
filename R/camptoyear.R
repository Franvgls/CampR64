#'Transforma series de nombres de campaña en años
#'
#'Transforma series de nombres de campañas en formato Camp XYY a años, si se incluyen códigos de 3 caracteres que no corresponden a caracter número número, devuelve 0, si es
#'@param x Vector con la serie de nombres de campaña a transformar a años
#'@family datos_especies
#'@examples camptoyear(Nsh)
#'@export
camptoyear<- function(x) {
  x<-sub("X","",x)
  if (any(nchar(as.character(x))!=3)) stop("Los valores a transformar no responden a nombres de campaña formato Camp, revise la entrada")
  suppressWarnings(ifelse(is.na(as.numeric(substr(x,2,3))),0,
                          as.numeric(substr(x,2,3))+ifelse(as.numeric(substr(x,2,3))>70,1900,2000)))
  }




