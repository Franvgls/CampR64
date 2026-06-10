#'Crea abreviaturas de especies a partir del nombre científico, género+especie
#'
#'Transforma series de nombres científicos de especies en abreviaturas de 9 caracteres, se puede elegir el número de
#' caracteres y el caracter de separación. Ignora borrando el sp. los nombres genéricos de especies con sp., no evita
#' los spp. cuidado.
#' Avisa si hay códigos duplicados en el resultado de los códigos solicitados, para evitar crear códigos repetidos para distintas especies haciend uso de base::make.unique
#' @param x Vector con la serie de nombres científicos a abreviar
#' @param nGen Número de caracteres del género a utilizar
#' @param nEsp número de catacteres del nombre de especie a utilizar
#' @param sep Caracter para separar Género de especie
#' @param unique Evitar duplicados en los códigos, no tiene mucho sentido pero se puede dejar como falso y tener repetidos los códigos con unique=FALSE
#' @return Devuelve un data.frame con dos columnas, nombres originales $names y los codigos $codes
#' @family datos_especies#' 
#' @examples AbrvEsp64(c("Merluccius merluccius","Merlangius merlangus"))
#' \dontrun{
#' AbrvEsp64(buscaesp64(1,50,"cant"),1,8,".")
#' }
#' @export
AbrvEsp64<- function(x,nGen=3,nEsp=4,sep="_",unique=TRUE) {
  x1<-NULL
  for (i in 1:length(x)) {
    x[i] <-sub(" sp.","",x[i],useBytes = TRUE,fixed = TRUE)
    if (regexpr(" ",x[i])==c(-1)) x1<-c(x1,substr(x[i],1,nGen+nEsp+1))
    if (regexpr(" ",x[i])>=1) x1<-c(x1,paste0(substr(x[i],1,nGen),sep,substr(x[i],regexpr(" ",x[i])+1,regexpr(" ",x[i])+nEsp)))
    x1
  }
  if (length(x1[duplicated(x1)])>0) {message(paste0("Registros repetidos para: ",paste(x[duplicated(x1)],collapse = ", ")))
    x1<-base::make.unique(x1,sep=sep)}
  return(data.frame(names=x,codes=x1))
  }
