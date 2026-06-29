#' Calcula las abundancias estratificadas por edad y hora en cada rectángulo ICES de la zona considerada
#'
#' Función de resultados: abundancias estratificadas por edad en cada rectángulo ICES dentro del área de la cqampaña a partir de los datos del camp.
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña de la que se extraen los datos un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (proporciona los datos para Medits pero no saca mapas y no tiene sentido ICES rectangulos)
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param mediahora Pondera a una hora si se solicita por defecto en 2 suponiendo lances de media hora
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param AltAlk ALK alternativa tomada de un fichero de edad del Camp edadXYY.dbf sin ruta ni extensión
#' @param incl2 Si T incluye los datos de los lances extra, tener en cuenta que no son lances oficiales para calcular los índices de abundancia
#' @param DatGraf Si F saca tabla con abundancias en cada rectángulo ICES muestreado en la campaña, si T saca los datos para sacar gráfica con MapIStatRec
#' @param es si T saca para etiquetas en español (Ed para edad), si FALSE en inglés (Ag para edad)
#' @examples 
#' \dontrun{
#' AbAgStatRec.camp64(1,74,"N21","cant","local",plus=3,DatGraf = FALSE)
#' AbAgStatRec.camp64(1,43,"P10","porc","local",DatGraf = T)
#' }
#' @family edades
#' @export
AbAgStatRec.camp64<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),plus=8,mediahora=2,cor.time=TRUE,AltAlk=NA,incl2=FALSE,DatGraf=FALSE,plotrix=TRUE,es=TRUE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {stop("Sólo se puede incluir una especie en esta función")}
  # ceros=TRUE: incluir TODOS los lances muestreados (con y sin captura).
  # Sin esto, maphistage64 filtra los lances vacíos y se pierden rectángulos
  # enteros; además sesga al alza las medias (denominador sólo con positivos).
  AbAgeHaul<-maphistage64(gr=gr,esp=esp,camp=camp,zona=zona,dns=dns,age=0,plus = plus,cor.time = cor.time,out.dat = TRUE,AltAlk = AltAlk,incl2 = incl2,plot = FALSE,mediahora = mediahora,ceros = TRUE)
  # maphistage64() ya devuelve la columna StatRec (vía datagegr.camp64),
  # por lo que NO hace falta el merge con CAMPtoHH64 (generaba StatRec.x/.y
  # y rompía el tapply). Trabajamos directamente sobre AbAgeHaul.
  AbAgeHaulICES<-AbAgeHaul
  # Columnas de edad por nombre (robusto ante cambios de posición):
  noedad<-c("lan","lat","long","weight.time","StatRec","camp","numero")
  edades<-setdiff(names(AbAgeHaulICES),noedad)
  Results<-tapply(AbAgeHaulICES[,edades[1]],AbAgeHaulICES$StatRec,mean)
  nLans<-tapply(AbAgeHaulICES[,edades[1]],AbAgeHaulICES$StatRec,length)
  for (i in 2:length(edades)) {
    Results<-rbind(Results,tapply(AbAgeHaulICES[,edades[i]],AbAgeHaulICES$StatRec,mean))
  }
  row.names(Results)<-c(paste0(ifelse(es,"Ed","Ag"),0:c(plus-1)),paste0(ifelse(es,"Ed","Ag"),plus,"+"))
  # gsub() previo: el StatRec ya viene con "#" delante (p.ej. "#15E6"), así que
  # sin limpiar quedaba doble almohadilla ("##15E6#"). Normalizamos a un solo #.
  colnames(Results)<-paste0("#",gsub("#","",colnames(Results)),"#")
  if(!DatGraf) return(t(Results))
  if (DatGraf) {
    Results1<-data.frame(ICESrect=gsub("#","",colnames(Results)),t(Results))
    Results1$long<-NA
    Results1$lat<-NA
    for (i in 1:nrow(Results1)) {
      Results1$long[i]<-Area[Area$ICESNAME==gsub(" ","",as.character(Results1$ICESrect[i])),"stat_x"]
      Results1$lat[i]<-Area[Area$ICESNAME==gsub(" ","",as.character(Results1$ICESrect[i])),"stat_y"]
    }
    Results1<-data.frame(Nlans=unname(nLans),Results1)
    Result3<-tidyr::pivot_longer(Results1,cols=colnames(Results1[3:c(ncol(Results1)-2)]),values_to="numero")
    Result3$name<-gsub("\\.","+",Result3$name)
    Result3$long<-NA
    Result3$lat<-NA
    Result3$ICESrect<-gsub(" ","",as.character(Result3$ICESrect))
    #Area$ICESNAME<-as.character(Area$ICESNAME)
    for (i in 1:nrow(Result3)) {
      Result3$long[i]<-Area[Area$ICESNAME==Result3$ICESrect[i],"stat_x"]
      Result3$lat[i]<-Area[Area$ICESNAME==Result3$ICESrect[i],"stat_y"]
    }
    Result3<-data.frame(nLans=unname(nLans),Result3)
    Result3}
  if (plotrix) return(Results1) else return(Result3)
}