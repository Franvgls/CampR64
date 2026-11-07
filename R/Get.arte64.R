#' Transforma a DataFrame un na salida del Pesca Win de registro del arte Arte
#'
#' Función auxiliar para el PescaWin:
#' Lee el fichero de arte de una campaña (Arte_XXX.csv, registro de los sensores
#' del arte en el PescaWin), y da los datos como data.frame, ya corrigiendo tiempos
#' a lo largo del arrastre dejando sólo los registros válidos (`Val==1`) en
#' FIRME, conservando los datos de lance del lanceXXX.dbf.
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el directorio de marcas del PescaWin si `folder=NA`: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige la raíz del PescaWin si `folder=NA`: "local" "c:/gps/", "serv" "z:/gps/". Se pueden cambiar con `options(CampR64.gps.local=...)` y `options(CampR64.gps.serv=...)`
#' @param lan Lance o lances que se quieren revisar, si se deja en NA se toman todos los lances del fichero
#' @param FileArte Prefijo del fichero de arte, el fichero leído es `paste0(folder,FileArte,camp,".csv")`
#' @param folder Directorio del fichero de arte. Si NA la raíz según `dns` más "mrk/norte/", "mrk/porcupine/" o "mrk/arsa/" según `zona`
#' @param nmin Número mínimo de registros válidos en FIRME para dar las medianas de un lance
#' @return Devuelve (invisible) un data.frame con lance, dista_p (distancia entre puertas), abert_h (abertura de calones), abert_v (abertura vertical) en metros y n (registros válidos usados, tengan o no dato de los sensores). Los valores que no se han podido calcular son NA
#' @note El PescaWin registra un 0 cuando no recibe dato de un sensor, y estos ceros se tratan como dato ausente. Si un sensor no da dato en un lance, su mediana es NA y su panel sale vacío (lo que ocurre con los calones de los lances 1, 3, 4 y 5 de P26). Si son los tres sensores (lances 14 a 25 de P26), el lance sigue en la salida con NA y se avisa con un mensaje. Si un sensor no tiene datos en ninguno de los lances seleccionados, su gráfico no se saca y también se avisa.
#' @details En los gráficos los puntos rellenos son registros válidos (`Val==1`) y los huecos no válidos; la línea azul continua es la mediana de los válidos, la roja el suavizado loess y la azul de puntos en la abertura vertical el primer minuto en que el arte baja de 1.1 veces la mediana (estimación de llegada al fondo).
#' @seealso \link{pasa.lan64}, \link{ArtePars64}
#' @examples
#' \dontrun{
#' Get.arte64("N25","cant")
#' Get.arte64("N25","cant","local",lan=c(16,17))
#' }
#' @family PescaWin
#' @export
Get.arte64<-function(camp,zona="cant",dns=c("local","serv"),lan=NA,FileArte="Arte_",folder=NA,gps=FALSE,nmin=3) {
  if (length(camp)>1) stop("Seleccionada más de una campaña, sólo se puede pasar una")
  dns<-match.arg(dns)
  if (is.na(folder)) {
    # Raíz del PescaWin en el ordenador o en el servidor, sin pasar por CampR64_paths
    raiz<-getOption(paste0("CampR64.gps.",dns),ifelse(dns=="local","c:/gps/","z:/gps/"))
    folder<-switch(tolower(substr(zona,1,4)),
                   cant="mrk/norte/",
                   porc="mrk/porcupine/",
                   arsa="mrk/arsa/",
                   stop("zona debe ser 'cant', 'porc' o 'arsa', o indicar folder"))
    folder<-paste0(sub("[/\\\\]+$","",raiz),"/",folder)
  }
  fich<-paste0(folder,FileArte,camp,".csv")
  if (!file.exists(fich)) stop("No existe el fichero ",fich)
  # Todo como texto: el PescaWin repite la cabecera en cada lance
  DatArte<-utils::read.csv(fich,colClasses="character",check.names=TRUE,strip.white=TRUE)
  DatArte<-DatArte[DatArte$Maniobra=="FIRME",]
  for (i in c("Lance","Puertas","Calones","Vertical","Esp.libre","Val")) {
    DatArte[[i]]<-suppressWarnings(as.numeric(DatArte[[i]]))
  }
  DatArte<-DatArte[!is.na(DatArte$Lance),]
  if (!any(is.na(lan))) DatArte<-DatArte[DatArte$Lance %in% lan,]
  if (nrow(DatArte)==0) stop("Los lances seleccionados no existen o no tienen datos en FIRME")
  # Tiempo de arrastre: m:ss, mm:ss o mm:ss:00 (exportado por Excel pasados 24'), a minutos decimales
  tt<-strsplit(DatArte$Tiempo,":",fixed=TRUE)
  DatArte$t.arr<-vapply(tt,function(x) as.numeric(x[1])+as.numeric(x[2])/60,numeric(1))
  # Ceros son sensores sin dato; la vertical con espacio libre en los primeros 6' es el arte aún bajando
  DatArte$Puertas[which(DatArte$Puertas==0)]<-NA
  DatArte$Calones[which(DatArte$Calones==0)]<-NA
  DatArte$Vertical[which(DatArte$Vertical==0)]<-NA
  DatArte$Vertical[which(DatArte$Esp.libre>0 & DatArte$t.arr<6)]<-NA
  DatArte$Val[is.na(DatArte$Val)]<-0
  if (any(!is.na(lan))) {DatArte<-DatArte[DatArte$Lance %in% lan,]}
  return(DatArte)
}
  