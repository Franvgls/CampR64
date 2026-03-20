#' Salida con abundancias estratificadas por estrato de profundidad
#' 
#' Salida de datos a csv para rellenar los informes de grupo de trabajo por estrato, filas con datos ab estratificada (Biomasa y N) y error estándar por estrato. Da la abundancia y error estándar en cada estrato de profundidad (definido en el Camp)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" 
#' @param dns Elige si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas permite calcular los errores estándar aunque sólo haya un lance en algún estrato (haciendo varianza =0 en ese estrato, incorrecto pero da una idea cuando sólo un estrato entre varios tiene sólo un lance)
#' @return Devuelve un número con nombres organizado en dos líneas (biomasa y número) en columnas por estratos de profundidad segun definidos en el Camp e información por columnas abundancia estratificada media por estrato (avgestr) y error estándar (SEestr) y totales (avgsect, SEsect). Preparado para pegarlo de año en año en los ficheros excel de abundancia en grupo de trabajo
#' @examples databEstr(1,51,"N05","Cant")
#' @seealso {\link{databICES64} \link{databICESdiv64} \link{datab64}}
#' @export
#' 
databEstr64<-function(gr,esp,camp,zona=c("cant"),dns=c("local","serv"),cor.time=TRUE,Nas=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  dumb1<-sapply(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas)$estratos[1:2,],t)
  dumb2<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas)$total[1:2,])
  dumb3<-sapply(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$estratos[1:2,],t)
  dumb4<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$total[1:2,])
  names1<-rep(sapply(row.names(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$estratos[1:2,]),t),length(dumb1)/2)
  names2<-paste(t(names(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$total[1:2,])),"Tot",sep=".")
  names3<-colnames(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas)$estratos)
  names4<-paste(names1,rep(names3,each=2),sep=".")
  dumb5<-rbind(c(dumb1,dumb2),
               c(dumb3,dumb4))
  rownames(dumb5)<-c(paste(buscaesp64(gr,esp,zona,dns),camp,"p",sep="_"),paste(buscaesp64(gr,esp,zona,dns),camp,"n",sep="_"))
  colnames(dumb5)<-c(names4,names2)
  dumb5
}
