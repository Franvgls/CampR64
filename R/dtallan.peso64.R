#' Peso de la distribución de tallas de esp en los lances solicitados
#'
#' Da el peso de la distribución de tallas de las capturas de la especie en los lances solicitados, a partir de la relación talla-peso. Tiene en cuenta si la especie se mide en mm o a .5 cm (med e increm en especies.dbf) para tomar la marca de clase en cm
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter
#' @param camp Campaña de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param lances Número de lance o lances de los que se extrae el dato de peso de la especie solicitada. Si lances se deja en NA, da todos los lances de la campaña.
#' @param sex Si F (por defecto) da el peso total, si T da el peso por sexos (si los hay) y el total
#' @param muestr Si T (por defecto) da el peso de la captura (tallas elevadas al peso total), si F el de la muestra medida
#' @param areg Para cambiar el coeficiente "a" de la regresión si es distinto al archivado en el CAMP.
#' @param breg Para cambiar el coeficiente "b" de la regresión si es distinto al archivado en el CAMP.
#' @return Peso en gramos (según las unidades de a y b) de la distribución de tallas, un valor si sex=F o un vector con machos, hembras, indet y total si sex=T
#' @seealso \link{dtallan.camp64} \link{talpes64.camp} \link{qcLW64.camp}
#' @family Distribuciones de tallas
#' @examples
#' \dontrun{
#' dtallan.peso64(gr=1,esp=50,camp="N23",zona="cant",dns="local",lances=50)
#' dtallan.peso64(gr=1,esp=50,camp="N23",zona="cant",dns="local",sex=TRUE)
#' }
#' @export
dtallan.peso64<-function(gr,esp,camp,zona,dns=c("local","serv"),lances=NA,sex=FALSE,muestr=TRUE,areg=NA,breg=NA) {
  if (length(esp)>1 | length(camp)>1 | any(esp=="999")) {
    stop("la función sólo sirve para una campaña y una especie")
  }
  dumb<-dtallan.camp64(gr,esp,camp,zona,dns,lances=lances,sex=sex,muestr=muestr)
  if (is.na(areg) & is.na(breg)) abdumb<-talpes64.camp(gr,esp,zona,dns)
  else abdumb<-c(areg,breg)
  #### marca de clase en cm según las unidades de medida de la especie
  unid<-unid64.camp(gr,esp,zona,dns)
  if (nrow(unid)>0 && unid$med[1]==2) {
    tal<-(dumb$talla+as.numeric(unid$increm[1])/2)/10
  }
  else tal<-dumb$talla+.5
  pesos<-abdumb[1]*tal^abdumb[2]*as.matrix(dumb[,-1,drop=FALSE])
  if (!sex) return(sum(pesos))
  c(colSums(pesos),total=sum(pesos))
}
