#' Distribucion binomial: exitos necesarios para ser significativo
#' 
#' Funcion para la MSFD y calculo de numero de especies que hacen que unos resultados esten fuera de lo que cabria esperar al azar para ver mejoras en la biodiversidad por especie.
#' @param n Número de eventos a tener en cuenta
#' @param p.teorica probabilidad teórica de la distribución que se quiere comprobar.
#' @return Devuelve una matriz con tres columnas: una la probabilidad en función del número de casos que se consideren como un éxito, y luego los intervalos de confianza inferior y superior, a partir de que p baja de 1 y llega a .05 se puede considerar que el resultado no se puede considerar debido al azar
#' @examples #binom(24)
#' @export
binom64<-function(n,p.teorica=.5) {
  binomial<-data.frame(n=NULL,p=NULL,cinf=NULL,csup=NULL)
  for (i in 1:n) {
    dumb<-binom.test(i,n,p.teorica)
    binomial<-rbind(binomial,data.frame(x=i,p=dumb$p.value,cinf=dumb$conf.int[1],csup=dumb$conf.int[2]))
  }
  binomial<-cbind(binomial[,1],round(binomial[,2:4],4))
  print(list(p.teorica=p.teorica,binomial=binomial))
}