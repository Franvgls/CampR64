#' Datos de abundancia por división ICES para una especie resúmen para grupos de trabajo
#' 
#' Salida de datos a csv para rellenar los informes de grupo de trabajo, filas con datos ab estratificada (Biomasa y N) y error estándar por subdivisión ICES función para Demersales Norte (saca 9.a, 8.c y total) 
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" 
#' @param dns Elige si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas Permite calcular los errores estándar aunque sólo haya un lance en algún estrato (haciendo varianza =0 en ese estrato, incorrecto pero da una idea cuando sólo un estrato entre varios tiene sólo un lance)
#' @return Devuelve un número con nombres organizado en dos líneas (biomasa y número) en columnas por subdivisiones ICES por columnas abundancia estratificada media por XIa, 8.cE, 8.cW
#' @seealso {\link{databICES} \link{databEstr} \link{datab}}
#' @export
databICESdiv<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),cor.time=TRUE,Nas=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (all(substr(zona,1,4)!="cant")) {stop("Función sólo disponible para Demersales Costa Norte divisiones 9.a, 8.c Este y 8.c Oeste")}
  dumb1<-sapply(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,excl.sect=c(2:5),Nas=Nas)$sectores[1:2,],t)
  dumb2<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,excl.sect=c(1),Nas=Nas)$total[1:2,])
  dumb3<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas)$total[1:2,])
  dumb4<-sapply(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",excl.sect=c(2:5),Nas=Nas)$sectores[1:2,],t)
  dumb5<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",excl.sect=c(1),Nas=Nas)$total[1:2,])
  dumb6<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$total[1:2,])
  dumb7<-rbind(c(dumb1,dumb2,dumb3),
               c(dumb4,dumb5,dumb6))
  rownames(dumb7)<-c(paste(buscaesp64(gr,esp,zona,dns),camp,"p",sep="_"),paste(buscaesp64(gr,esp,zona,dns),camp,"n",sep="_"))
  colnames(dumb7)<-c("9.aN_Avg","9.aN_SE","8.cAvg","8.cSE","Tot_Avg","Tot_SE")
  dumb7
}
