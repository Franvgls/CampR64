#' Mapa de estaciones hidrológicas (CTDs) en una campaña
#'
#' Utiliza los datos del Camp representar la variación geográfica de los datos hidrologicos, temperatura y salinidad
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param dns Elige si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param ind Elige el valor (t)emperatura o (s)alinidad
#' @param plot Saca el gráfico (si T) o si se salva como objeto se puede componer para componer con otros gráficos de lattice (F)
#' @param ti Añade la Campaña, si F no añade titulo
#' @param sub Si T añade el parámetro (temperatura, salinidad) como subtítulo bajo el gráfico en inglés o español
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T saca los titulos y rotulos en español, si F en inglés
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param cex.pt Varía el tamaño de los puntos en los gráficos
#' @param cexleg varía el tamaño del texto de la leyenda y los ejes
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param graf si F no el gráfico va a pantalla, si nombre fichero va a fichero en el directorio en que está wdf
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Saca el mapa de diversidad en la campaña seleccionada.
#' @examples MatchHidro.ctd("N24","Cant",ind="t",ti=T)
#' @family mapas
#' @family hidrología
#' @export
MatchHidro.ctd64<-function(camp,zona="porc",dns=c("local","serv"),plot=TRUE,subtit=TRUE,bw=FALSE,ti=TRUE,es=TRUE,out.dat=FALSE,cex.pt=1,
                       cexleg=1,years=TRUE,observ=FALSE,estn=FALSE,lanctd=FALSE,graf=FALSE,xpng=1200,ypng=800,ppng=15,cuts=4) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    lnkk<-datlan.camp64(camp,zona,dns,redux=T)
    hdkk<-dathidro.camp64(camp,zona,dns)
    if (zona=="cant") MapNort64()
    if (zona=="porc") mapporco64()
    if (zona=="arsa") MapArsa64()
    title(camp)
    text(lat~long,lnkk,lance,cex=.8,font=2)
    legend("bottomright","Nº lance en lances",col=1,inset=c(.03,.01),bty ="n")
    if (lanctd) {
        text(lat~long,lnkk,lance,cex=.8,font=2,col="red",pos=3)
        legend("bottomright","Nº lance en hidro",inset = c(.03,.04),text.col="red",bty ="n")
        }
    if (estn) {
        text(lat.ctd~long.ctd,hdkk,estn,cex=.8,font=2,col="green",pos=1)
        legend("bottomright","Nº estn CTD",inset = c(.04,.04),text.col="green",bty ="n")
    }
    if (observ) {
        text(lat~long,hdkk,observ,cex=.7,font=2,pos=4,col=2)
        legend("bottomright","Observaciones hidro",inset = c(.06,.04),text.col=2,bty ="n")
    }
}
