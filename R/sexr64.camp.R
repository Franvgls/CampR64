#' Sex ratio por talla de una especie
#'
#' Función de acceso a datos:
#' Extrae los datos de sex ratio de una especie a lo largo de la distribución de tallas muestreada
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige la zona de la campaña: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa", medits: "medi"
#' @param dns Elige el origen de las bases de datos: "local" en el ordenador, "serv" toma los datos del servidor
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sex.ratio Si plot=T el gráfico será del sex ratio
#' @param sex.prop si plot=T el gráfico será de las proporciones por sexos
#' @param out.dat si T saca los datos de tallas por sexo
#' @param verbose Si T muestra avisos problemas de tallas entre distintas especies
#' @return Devuelve un data.frame con variables: talla, machos, hembras e indet(erminados) si existen todos y si sex=TRUE
#' @seealso \link{propmat64.camp}
#' @examples 
#' \dontrun{
#' sexr64.camp(1,50,"P07","porc",dns="serv",excl.sect=c("B","C"))
#' }
#' @export
sexr64.camp<- function(gr,esp,camp,zona="cant",dns=c("local","serv"),cor.time=TRUE,excl.sect=NA,verbose=TRUE,plot=TRUE,es=F,sex.ratio=TRUE,sex.prop=FALSE,out.dat=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("seleccionadas más de una especie, no tiene sentido sacar el sex ratio mezclando especies")}
  dtall<-dattal.camp64(gr=gr,esp=esp,camp=camp,zona=zona,dns=dns,cor.time = cor.time,excl.sect=excl.sect,sex=TRUE,verbose = F)
  if (ncol(dtall)==2 & names(dtall)[2]=="indet") stop("Datos sin sexos diferenciados, no se puede sacar información de sex-ratio")
  dtall<-cbind(talla=dtall$talla,apply(dtall[2:ncol(dtall)],2,'/',
              rowSums(dtall[2:ncol(dtall)])),sr=dtall$hembras/rowSums(dtall[,c("hembras","machos")]))
  dtall[is.nan(dtall)]<-0
  op<-par(no.readonly = T)
  on.exit(par(op))
    if (plot) {
      par(mar=c(7,5,4,4))
      if (all(sex.prop & sex.ratio)) par(mfrow=c(1,2),mar=c(8,5,4,4));
      if (sex.prop) {barplot(t(dtall[,2:4]/rowSums(dtall[,2:4])),spac=0,ylab="Sex-proportion",ylim=c(0,1),xlab="Size",
                             names=dtall[,1],main=camptoyear(camp),legend.text = T,args.legend = list(x="bottom",
                                                                                                      horiz=T,bty="n",inset=c(0,-.2),xpd=TRUE))
        box()}
      if (sex.ratio) {barplot(t(dtall[,c("hembras","machos")]/rowSums(dtall[,c("hembras","machos")])),spac=0,
                              ylab="Sex-ratio (proportion)",ylim=c(0,1),xlab="Size (cm)",names=dtall[,1],main=camptoyear(camp),legend.text = T,
                              args.legend = list(x="bottom",horiz=T,bty="n",inset=c(0,-.2),xpd=TRUE))
        box()}
    }
  if (out.dat) as.data.frame(dtall)
}