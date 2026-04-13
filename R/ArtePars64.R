#' Gráficos de parámetros del arte con la profundidad
#'
#' @description
#' Crea gráficos del comportamiento del arte en los lances de una campaña con la profundidad
#'
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "cant", Golfo de Cádiz "arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param dns Elige de dónde se toman los datos, si del ordenador "local" o del servidor "serv"
#' @param incl2 Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param sub Añade un subtítulo debajo del gráfico, sin texto por defecto.
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T rotulos gráfico en español, si F en inglés
#' @param profrange Si c(profmin,profmax) filtra por ese rango de profundidad por defecto NA no filtra profundidades
#' @param ceros por defecto incluye los valores de 0 de los parámetros
#' @param lan.cex tamaño de las etiquetas del numero de lances por campaña
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param escmult Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos
#' @param cex.leg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param graf si F no el gráfico va a pantalla, si nombre fichero va a fichero en el directorio en que está wdf
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,peso.gr,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
#' @examples
#' ArtePars64("N23","cant",Nlans = F,ti=T)
#' ArtePars64("N23","cant",Nlans = T,ti=T,lan.cex=2)
#' @family gear
#' @export
ArtePars64<-function(camp,zona="cant",dns=c("local","serv"),incl2=TRUE,es=T,bw=TRUE,ti=TRUE,sub=NULL,out.dat=FALSE,ceros=TRUE,cex.leg=1.1,
  years=TRUE,profrange=NA,proflab=F,Nlans=TRUE,lan.cex=.8,graf=FALSE,xpng=1200,ypng=800,ppng=15) {
  options(scipen=2)
  colo<-ifelse(bw,gray(1),"steelblue")
	dumb<-NULL
  lan<-datlan.camp64(camp=camp,zona=zona,dns=dns,incl2=incl2,redux=T)
  if (any(!is.na(profrange))) {
    lan<-dplyr::filter(lan,prof>min(profrange) & prof<max(profrange))}
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  if (is.logical(graf)) par(mar=c(2,2.5,2, 2.5) + 0.3,xaxs="i",yaxs="i")
  par(mfcol=c(1,3))
  plot(dista_p~prof,lan,pch=ifelse(Nlans,NA,21),cex=1*lan.cex,bg=colo,ylim=c(0,max(dista_p,na.rm=T)*1.1),
       ylab=paste(ifelse(es,"Distancia puertas","Door spread"),"(m)"),
       xlab=paste(ifelse(es,"Prof.","Depth"),"(m)"))
  if (Nlans) text(dista_p~prof,lan,label=lance,cex=lan.cex*1,font=2)
  if (ti) title(ifelse(es,"Distancia puertas con profundidad","Door spread vs depth"))
  plot(abert_h~prof,lan,pch=ifelse(Nlans,NA,21),cex=1*lan.cex,bg=colo,ylim=c(0,max(abert_h,na.rm = T)*1.1),
       ylab=paste(ifelse(es,"Abertura calones","Wing spread"),"(m)"),
       xlab=paste(ifelse(es,"Prof.","Depth"),"(m)"))
  if (Nlans) text(abert_h~prof,lan,label=lance,cex=lan.cex*1,font=2,pos=4)
  if (ti) title(ifelse(es,"Abertura calones con profundidad","Wing spread vs depth"))
  plot(abert_v~prof,lan,pch=ifelse(Nlans,NA,21),cex=1*lan.cex,bg=colo,ylim=c(0,max(abert_v,na.rm = T)*1.1),
       ylab=paste(ifelse(es,"Abertura vertical","Vertical opening"),"(m)"),
       xlab=paste(ifelse(es,"Prof.","Depth"),"(m)"))
  if (Nlans) text(abert_v~prof,lan,label=lance,cex=lan.cex*1,font=2,pos=4)
  if (ti) title(ifelse(es,"Abertura vertical con profundidad","Vertical opening vs depth"))
  if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
  if (!is.logical(graf)) par(mar=c(5, 4, 4, 2) + 0.1)
  if (out.dat) {
	  if (out.dat) print(lan[,c("lance","dista_p","abert_h","abert_v")])
	}
#  par(op)
}
