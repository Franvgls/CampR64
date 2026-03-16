#'  Histograma de distribución de tallas con edades para varias campañas
#'  
#'  Función gráfica a partir de los ficheros del camp. Histograma distribución de tallas estratificada por edad en varias campañas
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campañas a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param dns elije de dónde se toman los datos, "local" del mismo ordendor, "serv" del servidor en el IEO si está preparado
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param leg Si T añade leyenda
#' @param years Si T saca años en lugar de nombres de la campaña
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param clms Número de columnas para ordenar la serie histórica
#' @param layout Organización de gráficos en filas ó columnas c(r,c) 
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param ymax Valor máximo del eje y
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @return Saca gráfica con distribución de tallas y la distribución de las edades en cada talla. Si out.dat=TRUE saca un data.frame con columnas n(valor del número de la distribución estratifcada para la talla y la edad),talla,edad,camp. Da error si no existe ALK para la especie en la campaña
#' @seealso {\link{grafedtal.camp}}
#' @examples grafedtal.camps(1,43,Nsh[22:31],"Cant",plus=7,years=T,es=FALSE)
#' @family edades
#' @export
grafedtal.camps64 <- function(gr,esp,camp,zona="cant",dns="local",plus=8,cor.time=TRUE,excl.sect=NA,ti=FALSE,leg=TRUE,es=TRUE,layout=NA,idi="l",cexleg=1,clms=2,plot=TRUE,ymax=NA,out.dat=FALSE,years=TRUE)  {
  if (length(camp)==1) {stop("Ha seleccionado sólo una campaña, utilice grafedtal.camp")}
  ndat=length(camp)
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
  dumb<-grafedtal.camp64(gr,esp,camp[1],zona,dns,plus,cor.time=cor.time,excl.sect,ti=FALSE,leg=TRUE,es,plot=FALSE,out.dat=TRUE)
  for (i in camp[2:length(camp)]) {
    dumb<-rbind(dumb,grafedtal.camp64(gr,esp,i,zona,dns,plus,cor.time=cor.time,AltAlk=NA,excl.sect,ti=FALSE,leg=TRUE,es,plot=FALSE,out.dat=TRUE))
  }  
  colo<-rainbow(plus+1)
  if (is.logical(ti)) {
    if (ti) {tit<-list(label=buscaesp64(gr,esp,zona,dns,id=idi),font=ifelse(idi=="l",4,2),cex=1*cexleg)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(label=ti)
  }
  cols<-ifelse((plus+1)/2-trunc((plus+1)/2)==0,(plus+1)/2,trunc((plus+1)/2)+1)
  leg<-list(columns=cols,space="top",rect=list(T,col=colo,size=3),
            text=list(labels=c(0:c(plus-1),paste(plus,"+",sep="")),col="black",cex=.7))
  lattice::trellis.par.set(lattice::col.whitebg())
  lattice::trellis.par.set("par.strip.text"=list(cex=.8,font=2))
  lattice::trellis.par.set("par.strip.background"=list(col=c("grey90")))
  lattice::trellis.par.set("par.xlab.text",list(cex=1,font=2))
  lattice::trellis.par.set("par.ylab.text",list(cex=1,font=2))
  if (years) {
    dumb$camp<-as.character(dumb$camp)
    dumb$camp<-camptoyear(dumb$camp)
  }
  dumb$camp<-factor(dumb$camp,ordered=F)
  xlimi<-c(min(dumb$talla)*(.95-1),max(dumb$talla)*1.05)
  if (any(is.na(layout))) {
    if (ndat>3) layout<-c(clms,ceiling(ndat/clms))
    else {layout<-c(1,ndat)}
  }
  a<-lattice::barchart(n~talla|camp,dumb,groups=factor(dumb$edad),col=colo,main=tit,xlim=xlimi,
              scales=list(alternating=F,tck=c(1,0),cex=.7,x=list(tick.number=10)),box.ratio=1000,h=F,stack=T,
              key=leg,xlab=paste(ifelse(F,"talla","Length"),"(cm)"),layout=layout,par.strip.text=list(cex=.7,font=2),
              ylab="Number",par.strip.background=list(col=c("grey90")),as.table=T,
              panel=function(x,y,...){
                lattice::panel.grid(-1,0,lty=3,col="black")
                lattice::panel.barchart(x,y,...)
              }
            
    )
  if (plot) print(a)
  else a
}