#' Comprueba la distancia recorrida en los lances y la consistencia con recorrido y la velocidad en los datos del CAMP
#'
#' Sirve para control de calidad y asegurarse que los datos de distancias y posiciones son correctos.
#'
#' Con \code{plot} distinto de \code{"none"} reproduce los gráficos que existían en la
#' versión original de CampR y que se mantuvieron en el shiny de IMBUS (función
#' \code{qcHaulsDist()} sobre datos DATRAS): tamaño de punto proporcional a la magnitud
#' del error, color rojo/azul según el signo, y líneas de referencia por cuantiles. Se
#' habían perdido en la migración a CampR64, donde la función solo devolvía la tabla.
#'
#' De los tres gráficos, el de rumbo (\code{"course"}) es el más espurio: depende mucho
#' de dónde termine el barco al final del lance y de cómo lo mueva la corriente, así que
#' hay que interpretarlo con más cautela que los de distancia y velocidad.
#'
#' @param camp campaña a revisar los datos en formato Camp Xyy
#' @param zona Origen de bases de datos: "cant" cantábrico, "porc"  Porcupine, "arsa" para el Golfo de Cádiz y "medi" para MEDITS
#' @param todos Por defecto F. Si T lista todos los lances con valores, si no sólo los que pc.error>error
#' @param pc.error porcentaje de error aceptable para no mostrar los lances como erróneos; también determina las líneas de referencia de los gráficos (cuantil pc.error/10 del valor absoluto del error)
#' @param plot Qué gráfico(s) sacar: \code{"none"} (por defecto, sin gráfico), \code{"dist"}
#'   (distancia-puntos), \code{"speed"} (distancia-velocidad), \code{"course"}
#'   (rumbo vs. puntos de largada/virada) o \code{"all"} (los tres apilados con
#'   \code{par(mfrow=c(3,1))}, como en la versión original de CampR)
#' @param esc.mult factor de escala del texto en los gráficos (cex.lab, cex.axis, cex.main)
#' @return Devuelve un data.frame con campaña, lance, recorrido, recorrido según la fórmula de Haversine, recorrido según la velocidad x el tiempo, velocidad, tiempo, rumbo, rumbo estimado según posiciones,velocidad calculada a partir de la distancia y el tiempo, y los porcentajes de errores de distancia, velocidad y rumbo. Si \code{plot} no es \code{"none"} además dibuja el gráfico correspondiente como efecto lateral.
#' @examples
#' \dontrun{
#' qcdistlan.camp64("C14","cant","local",pc.error=.01)
#' qcdistlan.camp64("216","arsa","local",pc.error=.01)
#' qcdistlan.camp64("N12","cant","local",pc.error=2,plot="speed")
#' qcdistlan.camp64("N12","cant","local",pc.error=2,plot="all")
#' }
#' @seealso \link{MapLansGPS64}
#' @references distHaversine function gives the haversine calculation of distance between two geographic points \code{\link[geosphere]{distHaversine}}
#' @family Control de calidad
#' @export
qcdistlan.camp64<-function(camp,zona="cant",dns=c("local","serv"),todos=FALSE,pc.error=2,
                           plot=c("none","dist","speed","course","all"),esc.mult=1) {
  plot<-match.arg(plot)
  dumblan<-datlan.camp64(camp,zona,dns,redux=FALSE)
  dumblan$mins<-round(dumblan$haul.mins*dumblan$weight.time,1)
  dumblan$dist.vel<-round(c(dumblan$weight.time*dumblan$haul.mins)/60*dumblan$velocidad*1852,0)
  dumblan$dist.hf<-round(geosphere::distHaversine(dumblan[,c("longitud_l","latitud_l")],dumblan[,c("longitud_v","latitud_v")]))
  dumblan$vel.dist<-round((dumblan$dist.hf/1852)/(dumblan$weight.time*dumblan$haul.mins/60),1)
  dumblan$error.vel<-round((dumblan$dist.vel-dumblan$recorrido)*100/dumblan$recorrido,2)
  dumblan$error.dist<-round((dumblan$dist.hf-dumblan$recorrido)*100/dumblan$recorrido,2)
  dumblan$rumb<-round(geosphere::bearingRhumb(dumblan[,c("longitud_l","latitud_l")],dumblan[c("longitud_v","latitud_v")]),1)
  dumblan$error.rumb<-round(dumblan$rumb-dumblan$rumbo)
  
  if (plot!="none") .qcdistlan64_plot(dumblan,camp=camp,zona=zona,pc.error=pc.error,plot=plot,esc.mult=esc.mult)
  
  if (todos) return(dumblan[order(dumblan$camp,dumblan$lance),c("camp","lance","recorrido","dist.hf","dist.vel","velocidad","mins","vel.dist","error.dist","error.vel","rumbo","error.rumb")])
  else return(dumblan[abs(dumblan$error.dist)>pc.error | abs(dumblan$error.vel)>pc.error*2 | abs(dumblan$error.rumb)>pc.error,
                      c("camp","lance","recorrido","dist.hf","dist.vel","velocidad","mins","rumbo","rumb","vel.dist","error.dist","error.vel","error.rumb")])
}

#' Gráficos internos de qcdistlan.camp64
#'
#' Reproduce, sobre los datos ya calculados por \code{qcdistlan.camp64()}, los gráficos
#' que sacaba \code{qcHaulsDist()} (adaptada de CampR/IMBUS a datos DATRAS): un punto por
#' lance, tamaño proporcional a la magnitud del error, color rojo/azul según el signo, y
#' líneas de referencia según el cuantil \code{pc.error/10} del error absoluto.
#'
#' @param dumblan data.frame ya calculado dentro de \code{qcdistlan.camp64()}, con las
#'   columnas \code{lance}, \code{error.dist}, \code{error.vel} y \code{error.rumb}
#' @param camp,zona solo para el subtítulo del gráfico
#' @param pc.error igual que en \code{qcdistlan.camp64()}
#' @param plot uno de \code{"dist"}, \code{"speed"}, \code{"course"} o \code{"all"}
#' @param esc.mult factor de escala del texto
#' @keywords internal
#' @noRd
.qcdistlan64_plot<-function(dumblan,camp,zona,pc.error,plot,esc.mult=1) {
  dumblan<-dumblan[order(dumblan$lance),]
  op<-graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(op))
  
  draw_one<-function(errvar,titulo) {
    err<-dumblan[[errvar]]
    ylims<-max(abs(err),na.rm=TRUE)*1.1
    graphics::plot(dumblan$lance,err,cex=sqrt(1+abs(err)),pch=21,
                   bg=ifelse(err<0,"red","blue"),type="o",xlim=c(0,max(dumblan$lance,na.rm=TRUE)+1),
                   ylim=c(-ylims,ylims),ylab="Error",xlab="Lance",
                   cex.lab=1*esc.mult,cex.axis=1*esc.mult)
    graphics::mtext(paste0("Campaña ",camp," - ",zona),side=3,line=0,adj=0,cex=0.8*esc.mult,font=2)
    q<-stats::quantile(abs(err),pc.error/10,na.rm=TRUE)
    graphics::abline(h=c(-q,0,q),lty=c(3,2,3),lwd=c(.5,1,.5))
    graphics::title(main=titulo,cex.main=1.1*esc.mult)
  }
  
  if (plot=="all") graphics::par(mfrow=c(3,1),mar=c(3,4,2,2))
  else graphics::par(mfrow=c(1,1),mar=c(5,4,4,2))
  
  if (plot %in% c("dist","all"))   draw_one("error.dist","Distance-Points error")
  if (plot %in% c("speed","all"))  draw_one("error.vel","Distance-speed error")
  if (plot %in% c("course","all")) draw_one("error.rumb","Course vs. Shoot-end points error")
}