#' Tabla resumen abundancias estratificadas por estrato y sector
#'
#' Salida de datos para informe de campaña: abundancias medias y errores estándar por subestrato (sector x estrato), con los totales por sector, por estrato y de la campaña. Pensada para el Cantábrico y Galicia (5 sectores y 3 estratos), pero construye la tabla a partir de los nombres de sectores y estratos, por lo que sirve también para otras zonas
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año concreto (XX): Demersales "NXX", Porcupine "PXX"
#' @param zona Elige el origen de las bases de datos: Cantábrico "cant", Porcupine "porc"...
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas Permite calcular los errores estándar aunque sólo haya un lance en algún estrato (haciendo varianza =0 en ese estrato, incorrecto pero da una idea cuando sólo un estrato entre varios tiene sólo un lance)
#' @return Devuelve una lista con dos matrices, absw (biomasa) y absn (número), con filas estratos y Total, y columnas media (Avg_) y error estándar (SE_) de cada sector y del total
#' @seealso \link{CV.camp64}
#' @examples
#' \dontrun{
#' TabAbsEsp.camp64(1,36,"N08","cant","local")
#' }
#' @export
TabAbsEsp.camp64<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),cor.time=TRUE,Nas=FALSE) {
  tabla<-function(cv) {
    sect<-colnames(cv$sectores)
    estr<-colnames(cv$estratos)
    loc<-t(sapply(estr,function(e) {
      x<-cv$locales[1:2,match(paste0(sect,e),colnames(cv$locales)),drop=FALSE]
      as.vector(x)
    }))
    res<-cbind(rbind(loc,as.vector(cv$sectores[1:2,])),
               rbind(t(cv$estratos[1:2,,drop=FALSE]),t(cv$total[1:2,,drop=FALSE])))
    colnames(res)<-c(paste(c("Avg","SE"),rep(sect,each=2),sep="_"),"Total_media","Total_SE")
    rownames(res)<-c(estr,"Total")
    res
  }
  dumbw<-CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas)
  dumbn<-CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas,ind="n")
  list(absw=tabla(dumbw),absn=tabla(dumbn))
}
