#' Abundancia estratificada para un rango de talla
#'
#' Extrae los datos de abundancia de una especie o conjunto de especies con un rango de tallas determinado a partir de las distribuciones de talla.También puede mostrar los datos de biomasa a partir de la relación talla-peso
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camps campañas (años) a representar en el mapa: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param tmin Talla mínima
#' @param tmax Talla máxima
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param years Si T muestra los datos por años, si F por campañas (siguiendo el formato del parámetro camps)
#' @param ind Parámetro a representar saca los datos en "p"eso o "n"úmero
#' @param ti Si T en el gráfico muestra el nombre de la especie y el rango de tallas comprendido
#' @param las Controla el sentido de las etiquetas del gráfico, 2 perpendicular al eje, mejor para etiquetas de años
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @return Devuelve un vector con nombre con el número estratificado del rango de tallas deseados por campaña/año. Si se solicita plot=TRUE saca un gráfico de barras que muestra la abundancia por año. En peso sólo saca los resultados para una especie.
#' @examples dattal.camps(1,36,Psh,"Porc",1,15,ind="n",plot=TRUE)
#' @seealso {\link{dattal.camp}}
#' @export
dattal.camps64<- function(gr,esp,camps,zona,dns,tmin=0,tmax=999,cor.time=TRUE,excl.sect=NA,years=TRUE,ind="n",ti=TRUE,las=2,plot=FALSE,es=FALSE) {
  options(scipen=2)
  #esp<-format(esp,width=3,justify="r")
  if (length(esp)>1 & ind=="p") stop("No se pueden calcular las regresiones talla peso de más de una especie, considera usar calculos espec?ficos y sumarlos")
  if (length(esp)>1) warning("Seguro que tiene sentido mezclar más de una especie para sacar el rango de talla")
  dumb<-data.frame(dattal.camp64(gr,esp,camps[1],zona,dns,cor.time=cor.time,excl.sect=excl.sect,sex=FALSE),camp=camps[1])
  if (length(camps)>1) {
    for (i in camps[2:length(camps)]) {
      dumb<-rbind(dumb,data.frame(dattal.camp64(gr,esp,i,zona,dns,cor.time=cor.time,excl.sect=excl.sect,sex=FALSE),camp=i))
    }
  }
  dumbtal<-tapply(dumb$numero,dumb[,c("talla","camp")],sum,na.rm=TRUE)  
  #dumbtal<-dtall.camp(gr,esp,camps,dns,excl.sect,years,out.dat=TRUE,plot=FALSE)
  talla<-as.numeric(rownames(dumbtal))
  if (ind=="p") {
    ab<-talpes.camp64(gr,esp,zona,dns)
    peso<-(ab[1]*(talla+.5)^ab[2])
    dumbtal<-data.frame(talla=talla,as.data.frame(dumbtal)*peso)
  }
  else dumbtal<-data.frame(talla=talla,as.data.frame(dumbtal))
  dumbtal<-dumbtal[dumbtal$talla>=tmin & dumbtal$talla<=tmax,]
  if (years) colnames(dumbtal)<-c("talla",camptoyear(colnames(dumbtal[,2:ncol(dumbtal)])))
  #  browser()
  if (es){
    if (ind=="p") print(paste("Peso medio estratificado en gramos por lance de",buscaesp64(gr,esp,zona),"entre",tmin,"y",tmax,ifelse(unid.camp64(gr,esp)[1]==1,"cm","mm")))
    else print(paste("número medio estratificado de individuos por lance de",buscaesp64(gr,esp,zona),"entre",tmin,"y",tmax,ifelse(unid.camp64(gr,esp)[1]==1,"cm","mm")))
  }
  else {
    if (ind=="p") print(paste(buscaesp64(gr,esp,zona),"between",tmin,"and",tmax,ifelse(unid.camp64(gr,esp)[1]==1,"cm","mm"),"mean stratified weight in grams per haul"))
    else print(paste(buscaesp64(gr,esp,zona),"between",tmin,"and",tmax,ifelse(unid.camp64(gr,esp)[1]==1,"cm","mm"),"mean stratified number of individuals per haul"))
  }
    if (plot) {
#    op<-par(no.readonly=TRUE)
    par(mgp=c(2,.6,0))
    yetiq<-ifelse(es,expression("Ind"%*%"lan"^-1),expression("Ind"%*%"haul"^-1))
    datos<-colSums(dumbtal[,2:ncol(dumbtal)],na.rm=TRUE)
    barplot(datos,ylim=c(0,max(datos)*1.1),names.arg=colnames(datos),space=0,ylab=yetiq,las=las)
    box()
    if (ti) {
       title(main=buscaesp64(gr,esp,zona),font.main=4,line=2)
       title(main=paste(tmin,"-",tmax,ifelse(unid.camp64(gr,esp)[1]==1,"cm","mm")),font.main=2,cex.main=.9,line=.9)
       }
#    par(op)
  }
  colSums(dumbtal[,2:ncol(dumbtal)],na.rm=TRUE)
}

#hkeCantPequeños<-dattal.camps(1,50,Nsh[7:27],"Cnew",1,20,years=TRUE)
#hkeAlboranPequeños<-dattal.camps(1,50,Msh[1:17],"Medi",1,20,excl.sect=c(2:3),years=TRUE)
#hkeAlboranGrandes<-dattal.camps(1,50,Msh[1:17],"Medi",21,99,excl.sect=c(2:3),years=TRUE)
#hkeLevantinoPequeños<-dattal.camps(1,50,Msh[1:17],"Medi",1,20,excl.sect=c(1),years=TRUE)
#hkeLevantinoGrandes<-dattal.camps(1,50,Msh[1:17],"Medi",21,99,excl.sect=c(1),years=TRUE)