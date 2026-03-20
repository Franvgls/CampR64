#' Datos estratificados por sector para una especie para grupos de trabajo
#' 
#' Salida de datos estratificados por sector a csv para rellenar los informes de grupo de trabajo, filas con datos ab estratificada (Biomasa y N) y error estándar por sector. Da la abundancia y error estándar en cada sector (geográfico definido en el Camp)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param Nas Permite calcular los errores estándar aunque sólo haya un lance en algún estrato (haciendo varianza =0 en ese estrato, incorrecto pero da una idea cuando sólo un estrato entre varios tiene sólo un lance)
#' @return Devuelve un número con nombres organizado en dos líneas (biomasa y número) en columnas por sectores geográficos segun los definidos en el Camp e información por columnas abundancia estratificada media por estrato (avgestr) y error estándar (SEestr) y totales (avgsect, SEsect). Preparado para pegarlo de año en año en los ficheros excel de abundancia en grupo de trabajo
#' @seealso {\link{databICES} \link{databICESdiv} \link{databEstr}}
#' @export
datab64<-function(gr,esp,camp,zona="cant",dns=c("local","serv"),cor.time=TRUE,Nas=FALSE) {
  if (length(camp)>1) {stop("Seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  dumb1<-sapply(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,Nas=Nas)$sectores[1:2,],t)
  dumb2<-t(CV.camp64(gr,esp,camp,zona,dns,Nas=Nas)$total[1:2,])
  dumb3<-sapply(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$sectores[1:2,],t)
  dumb4<-t(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$total[1:2,])
  names1<-rep(sapply(row.names(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$sectores[1:2,]),t),length(dumb1)/2)
  names2<-t(names(CV.camp64(gr,esp,camp,zona,dns,cor.time=cor.time,ind="n",Nas=Nas)$total[1:2,]))
  dumb5<-rbind(c(dumb1,dumb2),
               c(dumb3,dumb4))
  rownames(dumb5)<-c(paste(buscaesp64(gr,esp,zona,dns),camp,"p",sep="_"),paste(buscaesp64(gr,esp,zona,dns),camp,"n",sep="_"))
  colnames(dumb5)<-c(names1,names2)
  dumb5
}
#datab("1"," 36","N05","Cant")
