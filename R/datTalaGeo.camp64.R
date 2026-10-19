#' Datos de tallas y sexo con geografía (para uso en especies a evaluar con datos de tallas: L. circularis, Phycis...)
#'
#' Función de transformación de datos:
#' Extrae los datos de abundancia por talla y sexo de una especie en cada lance y les añade los datos de su ubicación geográfica y profundidad
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter
#' @param camps Campañas de las que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param cor.time Si T corrige las abundancias en función de la duración del lance (number/weight.time), la columna weight.time se mantiene en la salida
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param sex Si T muestra los datos por sexo, si F suma los sexos en cada talla y lance (sex=NA)
#' @param verbose Si T avisa de los lances con tallas que no aparecen en el fichero de lances
#' @return Devuelve un data.frame con variables: species, year, survey, station, valid, length, sex, number, latitude, longitude, depth, sector, weight.time. number está elevado al peso total de la muestra (numer*peso_gr/peso_m)
#' @seealso \link{dattal.camp64} \link{datlan.camp64}
#' @examples
#' \dontrun{
#' datTalaGeo.camp64(1,87,Nsh[10:38],"cant","local")
#' }
#' @export
datTalaGeo.camp64<- function(gr,esp,camps,zona,dns=c("local","serv"),cor.time=TRUE,excl.sect=NA,sex=TRUE,verbose=TRUE) {
  if (length(esp)>1 | any(esp=="999")) stop("Esta función sólo admite una especie")
  ntalls<-NULL
  for (i in camps) {
    dumb<-readCampDBF("ntall",zona,i,dns)
    dumb<-dumb[as.character(dumb$grupo)==as.character(gr) & as.numeric(dumb$esp)==as.numeric(esp),
               c("lance","peso_gr","peso_m","talla","sexo","numer")]
    if (nrow(dumb)>0) ntalls<-rbind(ntalls,data.frame(dumb,camp=i,stringsAsFactors=FALSE))
  }
  if (is.null(ntalls)) {
    message("No hay datos de tallas de ",buscaesp64(gr,esp,zona,dns)," en las campañas seleccionadas")
    return(invisible(NULL))
  }
  ntalls$numer<-ifelse(ntalls$peso_gr!=ntalls$peso_m,ntalls$numer*ntalls$peso_gr/ntalls$peso_m,ntalls$numer)
  ntalls$lance<-as.numeric(ntalls$lance)
  dumblan<-datlan.camp64(camps,zona,dns,redux=TRUE,incl0=TRUE)
  dumblan<-dumblan[,c("camp","lance","lat","long","prof","weight.time","sector","validez")]
  dumblan$sector<-as.character(dumblan$sector)
  dumblan$lance<-as.numeric(dumblan$lance)
  dumblan$camp<-as.character(dumblan$camp)
  if (verbose) {
    nolan<-unique(ntalls[!paste(ntalls$camp,ntalls$lance) %in% paste(dumblan$camp,dumblan$lance),c("camp","lance")])
    if (nrow(nolan)>0) message("Tallas en lances que no están en el fichero de lances: ",paste(nolan$camp,nolan$lance,collapse=", "))
  }
  res<-merge(ntalls[,c("camp","lance","talla","sexo","numer")],dumblan,by=c("camp","lance"))
  if (any(!is.na(excl.sect))) {
    for (i in excl.sect) res<-res[!grepl(i,res$sector),]
  }
  if (cor.time) {
    if (any(res$weight.time==0)) {
      res$weight.time[res$weight.time==0]<-.1
      warning("Hay lances con duración 0 minutos, revisa validez")
    }
    res$numer<-res$numer/res$weight.time
  }
  if (!sex) {
    res<-aggregate(numer~camp+lance+talla+lat+long+prof+weight.time+sector+validez,res,sum)
    res$sexo<-NA
  }
  res$year<-camptoyear(res$camp)
  res<-res[order(res$year,res$lance,res$talla,res$sexo),]
  res$species<-buscaesp64(gr,esp,zona,dns)
  res<-res[,c("species","year","camp","lance","validez","talla","sexo","numer","lat","long","prof","sector","weight.time")]
  names(res)<-c("species","year","survey","station","valid","length","sex","number","latitude","longitude","depth","sector","weight.time")
  row.names(res)<-NULL
  as.data.frame(res)
}
