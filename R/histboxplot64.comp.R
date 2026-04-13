#' Gráficos de boxplot combinados biomasa y número para la serie histórica
#'
#' Crea box plots combinados (biomasa + número) con la distribución de la abundancia
#' para distintas zonas: Porcupine (zona="porc"), el Cantábrico (zona="cant"),
#' Cádiz (zona="arsa"), y el Mediterráneo (zona="medi").
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 desechos y otros, 9 escoge todos los orgánicos pero excluye desechos
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camps Campaña a representar: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa", Mediterráneo "medi"
#' @param dns Elige de dónde se toman los datos "local" del mismo ordenador, "serv" del servidor
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param bw Gráfico en blanco y negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín si T, si F no añade título
#' @param sub Añade un subtítulo debajo del gráfico, sin texto por defecto
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param es Si T rótulos gráfico en español, si F en inglés
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad
#' @param longrange Si c(longmin,longmax) filtra por ese rango de longitudes
#' @param latrange Si c(latmin,latmax) filtra por ese rango de latitudes
#' @param ceros Por defecto incluye los valores de 0, si F los quita
#' @param cex.leg Varía el tamaño de letra de los ejes
#' @param years Si T saca los años como nombre de campaña en el eje x
#' @return Crea una gráfica doble de boxplots en biomasa y número
#' @seealso \link{histboxplot64}
#' @examples
#' histboxplot64.comp(1,50,Nsh[7:27],"cant",years=TRUE)
#' @family abunds
#' @export
histboxplot64.comp <- function(gr, esp, camps, zona="cant", dns="local", cor.time=TRUE,
                               es=TRUE, bw=TRUE, ti=TRUE, sub=NULL, idi="l",
                               ceros=TRUE, cex.leg=1, years=TRUE,
                               profrange=NA, longrange=NA, latrange=NA) {
  op <- par(mfrow=c(2,1))
  on.exit(par(op), add=TRUE)
  histboxplot64(gr=gr, esp=esp, camps=camps, zona=zona, dns=dns, cor.time=cor.time,
                es=es, bw=bw, ti=ti, sub=ifelse(es, "Biomasa", "Biomass"), idi=idi,
                ind="p", ceros=ceros, cex.leg=cex.leg, years=years,
                profrange=profrange, longrange=longrange, latrange=latrange)
  histboxplot64(gr=gr, esp=esp, camps=camps, zona=zona, dns=dns, cor.time=cor.time,
                es=es, bw=bw, ti=FALSE, sub=ifelse(es, "Número", "Number"), idi=idi,
                ind="n", ceros=ceros, cex.leg=cex.leg, years=years,
                profrange=profrange, longrange=longrange, latrange=latrange)
}
