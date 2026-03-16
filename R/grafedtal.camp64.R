#'  Histograma de distribución de tallas con edades 
#'  
#'  Función gráfica a partir de los ficheros del camp. Histograma distribución de tallas estratificada por edad
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param dns elige si toma los datos del ordenador "local" o del servidor "serv" 
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param AltAlk Clave talla edad alternativa sin ruta ni extensión, NA por defecto usa la clave de la campaña edadXYY.dbf
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param leg Si T añade leyenda
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param ymax Valor máximo del eje y
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @return Saca gráfica con distribución de tallas y la distribución de las edades en cada talla. Si out.dat=TRUE saca un data.frame con columnas n(valor del número de la distribución estratifcada para la talla y la edad),talla,edad,camp. Da error si no existe ALK para la especie en la campaña
#' @seealso {\link{grafedtal.camps}}
#' @examples grafedtal.camp(1,43,"P09","Porc",es=FALSE,out.dat=TRUE)
#' @family edades
#' @export
grafedtal.camp64 <- function(gr,esp,camp,zona="cant",dns="local",plus=8,cor.time=TRUE,excl.sect=NA,AltAlk=NA,ti=FALSE,leg=TRUE,cexleg=1,es=TRUE,plot=TRUE,ymax=NA,out.dat=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
  edad<-GetAlk.camp64(gr,esp,camp,zona,dns,plus,n.ots=FALSE,AltAlk=AltAlk,keep_sexo = FALSE)          
  if (nrow(edad)==0) stop(paste("no existe clave talla edad para la especie",buscaesp64(gr,esp,zona,dns),"en la campaña",camp))
  if (is.logical(ti)) {
    if (ti) {tit<-list(label=buscaesp64(gr,esp,zona,dns,id=idi),font=ifelse(idi=="l",4,2),cex=1*cexleg)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(label=ti)
  }
  dtall<-dattal.camp64(gr,esp,camp,zona,dns,excl.sect=excl.sect,cor.time=cor.time)
  if (ncol(dtall)>2) dtall<-data.frame(talla=dtall[,1],n=rowSums(dtall[,-1])) else names(dtall)<-c("talla","n")
#  edad<-edad[which(rowSums(edad[5:20],na.rm=TRUE)>0),]
#  edad<-edad[,-c(1:3,21)]
#  for (i in 1:nrow(edad)) edad[i,which(is.na(edad[i,]))]<-0
#  if (plus<15) edad<-data.frame(edad[,1:(plus+1)],plus=rowSums(edad[,(plus+2):length(edad[1,])]))
#  edad<-data.frame(edad[,1],edad[,-1]/rowSums(edad[,-1]))
#  names(edad)<-c("talla",paste("E",0:(plus-1),sep=""),paste("E",plus,"+",sep=""))
  tedad<-data.frame(talla=dtall$talla)
  tedad<-merge(tedad,edad,by.x="talla",by.y="talla",all=TRUE)
  a<-dtall$talla[which(dtall$n>0)]
  b<-which((match(a,edad$talla,nomatch=0)==0),T)
  if (length(b)>0) {
    print("Las tallas: ",quote=FALSE)
    print(a[b])
    print("no estan en la clave talla edad",quote=FALSE)
  }
  else {
    for (i in 1:nrow(tedad)) tedad[i,which(is.na(tedad[i,]))]<-0
    b<-which((match(tedad$talla,dtall$talla,nomatch=0)==0),T)
    if (length(b)>0) {
      print("Las tallas: ",quote=FALSE)
      print(tedad$talla[b])
      print("aparecen en la clave y no en la distribución de tallas",quote=FALSE)
    }
    if (length(b)>0) tedad<-tedad[! tedad$talla %in% b,]
    tedad[,2:ncol(tedad)]<-tedad[,2:ncol(tedad)]*dtall[,2]
    edadtal<-data.frame(n=tedad[,2])
    for (i in 3:ncol(tedad)) {edadtal<-rbind(edadtal,data.frame(n=tedad[,i]))}	
    edadtal$talla<-rep(tedad$talla,ncol(tedad)-1)
    edadtal$edad<-rep(names(tedad)[-1],rep(nrow(tedad),ncol(tedad)-1))
    edadtal$n[which(is.na(edadtal$n))]<-0
    lattice::trellis.par.set(lattice::col.whitebg())
    colo<-rainbow(plus+1)
    cols<-ifelse((plus+1)/2-trunc((plus+1)/2)==0,(plus+1)/2,trunc((plus+1)/2)+1)
    if (is.na(ymax)) ylim=c(0,max(dtall$n)*1.1)
    else ylim=c(0,ymax)
    if (leg) {leg<-list(columns=cols,space="top",rect=list(T,col=colo),
                           text=list(labels=names(edad)[2:c(plus+2)],col="black",cex=.7))}
    else {leg<-NULL}
    xlimi<-c(min(dtall$talla)*(.95-1),max(dtall$talla)*1.05)
    foo<-lattice::barchart(n~talla,edadtal,groups=factor(edadtal$edad),col=colo,main=tit,xlim=xlimi,ylim=ylim,
                  scales=list(alternating=FALSE,tck=c(1,0),x=list(tick.number=10)),box.ratio=1000,h=FALSE,stack=TRUE,
                  strip=T,par.strip.text=list(cex=.7,font=2),key=leg,xlab=paste(ifelse(es,"talla","length"),"(cm)"),
                  panel=function(x,y,...){
                    lattice::panel.grid(-1,0,lty=3,col="black")
                    lattice::panel.barchart(x,y,...)
                  }
    )
    if (plot) print(foo)
    else foo
  }
  if (out.dat) cbind(edadtal,camp=camp)
}
