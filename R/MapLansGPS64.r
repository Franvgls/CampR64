#' Gráfico con los lances en segmentos
#'
#' gráfica todos los lances de una campaña con las distancias recorridas en cada uno de ellos
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" 
#' @param dns Elige si tonma los datos del ordenador "local" o del servidor "serv"
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña 
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña  
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param Nlans si T pone el número de lance junto al segmento
#' @param ax Si T saca los ejes x e y
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @return Devuelve un data.frame con datos de cada lance, las variables dependen de la selección de hidro y redux
#' @seealso {\link{datlan.camp}}, {\link{qcdistlan.camp}}
#' @examples MapLansGPS64("12C","cant")
#' @family mapas
#' @family PescaWin
#' @export
MapLansGPS64<-function(camp,zona="cant",dns=c("local","serv"),Nlans=FALSE,incl0=FALSE,xlims=NA,ylims=NA,places=TRUE,es=T,bw=FALSE,ax=T) {
  #if (!all(any(is.na(xlims)),any(is.na(ylims))))  stop("Si se especifica limite de coordenadas debe hacerlo en latitud y longitud")
  lan<-datlan.camp64(camp,zona,dns,redux=FALSE,incl2=TRUE,incl0=TRUE)
  lannul<-lan[lan$validez==0,c("longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  lan<-lan[lan$validez!=0,c("lance","longitud_l","latitud_l","prof_l","longitud_v","latitud_v","prof_v")]
  if (substr(zona,1,4)=="porc") {
    if (any(!is.na(xlims))) {mapporco64(xlims=xlims,ylims=ylims,ax=ax)} else mapporco64()
    }
  if (substr(zona,1,4)=="cant") {
    if (any(!is.na(xlims))) {MapNort64(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapNort64()
  }
  if (zona=="arsa") {
    if (any(!is.na(xlims))) {MapArsa64(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapArsa64()
  }
  if (zona=="Medi") {
    if (any(!is.na(xlims))) {MapMedit64(xlims=xlims,ylims=ylims,places=places,es=es,bw=bw,ax=ax)} else MapMedit64()
  }
  segments(lan$longitud_l,lan$latitud_l,lan$longitud_v,lan$latitud_v,col=1,lwd=2)
  if (Nlans==T) text(latitud_l~longitud_l,lan,label=lance,cex=.8,font=2,pos=1)
  if (incl0) segments(lannul$longitud_l,lannul$latitud_l,lannul$longitud_v,lannul$latitud_l,col=2,lwd=2)
}