#' Lances de una campaña en formato PescaWin
#'
#' Función auxiliar para el PescaWin:
#' Pasa las posiciones de largada y virada del fichero lanceXXX.dbf de la campaña
#' al formato de lances del PescaWin (long_l,lat_l,prof_l,long_v,lat_v,prof_v en
#' grados decimales con signo), y si `gps=TRUE` lo guarda como XXX.lan en el
#' directorio de lances del PescaWin. Antes `pasa.lan` en CampR (con RODBC).
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige de dónde se leen los datos, "local" para el ordenador, "serv" para el servidor
#' @param gps Si T exporta el fichero como camp.lan al directorio `folder`
#' @param valid Si T incluye sólo los lances válidos (>0) y excluye los nulos (=0)
#' @param folder Directorio de lances del PescaWin donde se guarda el fichero si `gps=TRUE`
#' @return Devuelve (invisible) un data.frame con lance,long_l,lat_l,prof_l,long_v,lat_v,prof_v en grados decimales con signo. El fichero .lan no lleva la columna lance ni cabecera.
#' @seealso \link{pasa.arte64}, \link{MapLansGPS64}
#' @examples
#' \dontrun{
#' pasa.lan64("N25","cant")
#' pasa.lan64("P25","porc",gps=TRUE,folder="c:/gps/lan/porcupin/")
#' }
#' @family PescaWin
#' @export
pasa.lan64<-function(camp,zona="cant",dns=c("local","serv"),gps=FALSE,valid=TRUE,folder="c:/gps/lan/") {
  if (length(camp)>1) stop("Seleccionada más de una campaña, sólo se puede pasar una")
  lan<-readCampDBF("lance",zona,camp,dns)
  names(lan)<-tolower(names(lan))
  if (valid) lan<-lan[as.numeric(lan$validez)>0,]
  # Hemisferio: si el dbf no tiene nsl/ewl (campañas antiguas) se asume N y W
  hemi<-function(x,def) if (is.null(x)) def else ifelse(is.na(x) | trimws(x)=="",def,toupper(trimws(x)))
  sgn.ns<-function(x) ifelse(hemi(x,"N")=="N",1,-1)
  sgn.ew<-function(x) ifelse(hemi(x,"W")=="E",1,-1)
  out<-data.frame(lance=as.numeric(lan$lance),
                  long_l=gradec(lan$longitud_l)*sgn.ew(lan$ewl),
                  lat_l=gradec(lan$latitud_l)*sgn.ns(lan$nsl),
                  prof_l=lan$prof_l,
                  long_v=gradec(lan$longitud_v)*sgn.ew(lan$ewv),
                  lat_v=gradec(lan$latitud_v)*sgn.ns(lan$nsv),
                  prof_v=lan$prof_v)
  out<-out[order(out$lance),]
  rownames(out)<-NULL
  print(out)
  if (gps) {
    fich<-file.path(folder,paste0(camp,".lan"))
    utils::write.table(out[,-1],fich,sep=",",row.names=FALSE,col.names=FALSE)
    message("Guardado ",fich)
  }
  invisible(out)
}
