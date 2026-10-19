#' Datos del muestreo de la especie esp en la campaña
#'
#' Saca un resumen de los ejemplares y peso muestreados de la especie esp en la campaña camp
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter. Utilizar buscacod64 para ver codigos
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @return Devuelve un list con nombre de la especie, número total medido del fichero de tallas, peso total capturado (kg) y peso total muestreado (kg) en los lances con tallas, ambos del fichero de tallas, y rango de tallas. Los pesos se suman por lance y categoría, de modo que coinciden con el fichero de capturas aunque haya varias categorías en un lance
#' @examples
#' \dontrun{
#' datmuest.camp64(2,19,"P16","porc","local")
#' }
#' @export
datmuest.camp64<-function(gr,esp,camp,zona,dns=c("local","serv"),excl.sect=NA) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de mas de una")}
  if (any(length(esp)>1 | esp==999)) {stop("seleccionadas más de una especie, no tiene sentido sacar resultados sobre el muestreo de varias especies, sacarlos por especie y sumarlos")}
  ntalls<-readCampDBF("ntall",zona,camp,dns)
  ntalls<-ntalls[as.character(ntalls$grupo)==as.character(gr) & as.numeric(ntalls$esp)==as.numeric(esp),
                 c("lance","cate","peso_gr","peso_m","talla","sexo","numer")]
  names(ntalls)<-gsub("_", ".",names(ntalls))
  if (any(!is.na(excl.sect))) {
    lan<-datlan.camp64(camp,zona,dns,redux=TRUE,excl.sect=excl.sect)
    ntalls<-ntalls[ntalls$lance %in% lan$lance,]
  }
  if (nrow(ntalls)==0) {
    message("No hay datos de tallas de ",buscaesp64(gr,esp,zona,dns)," en ",camp)
    return(invisible(NULL))
  }
  rgtal<-range(ntalls$talla)
  if (any(is.na(ntalls$numer))) {
    warning("Detectado valores perdidos en algún registro de talla")
    print(ntalls[is.na(ntalls$numer),])
  }
  Ntot<-sum(ntalls$numer,na.rm=TRUE)
  if (any(is.na(ntalls$peso.gr))) {
    warning("Detectado valores nulos en algún registro de peso muestra peso.gr fichero ntallXXX.dbf")
    print(ntalls[is.na(ntalls$peso.gr),])
  }
  if (any(is.na(ntalls$peso.m))) {
    warning("Detectados valores nulos en algún registro de peso muestra peso.m fichero ntallXXX.dbf")
    print(ntalls[is.na(ntalls$peso.m),])
  }
  # peso.gr y peso.m son únicos por lance y categoría (iguales en todas las tallas y sexos)
  pesos<-unique(ntalls[,c("lance","cate","peso.gr","peso.m")])
  Ptot<-round(sum(pesos$peso.gr,na.rm=TRUE)/1000)
  Pmue<-round(sum(pesos$peso.m,na.rm=TRUE)/1000,1)
  output<-list(especie=paste(buscaesp64(gr,esp,zona,dns),"en",camp),
               numero.medido=Ntot,peso.total.kg=Ptot,
               peso.muestreado.kg=Pmue,rango.tallas=rgtal)
  output
}
