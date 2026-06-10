#' Combinación en dos mapas de armap.camp con etiquetas de los lance y mapa con los lances en segmentos
#'
#' Muestra un mapa con el número de lance en el punto del lance y debajo o al lado mapa de los lances en segmentos
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: porcupine "porc", cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns elique si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param incl0 Si T se incluyen los lances nulos
#' @param xlims Define los limites longitudinales del mapa, si se deja en NA toma los límites de long del área definida en la campaña
#' @param ylims Define los limites latitudinales del mapa, si se deja en NA toma los límites de lat del área definida en la campaña
#' @param col Define el color de los segmentos
#' @param lwd Define el ancho de los segmentos
#' @param places si T por defecto, incluye las etiquetas de países y ciudad en tierra, no funciona en Porcupine
#' @param Nlans se T incluye los números de lance por encima de los segmentos
#' @param cexlab Tamaño de los números de lance utilizados en el gráfico de armap.lan parte del gráfico
#' @param rumbo si T incluye un punto al final del lance, en el punto de la virada, sólo en el segundo mapa
#' @param bw Si T gráfico en blanco y negro por default, si F gráfico en color
#' @return Devuelve un mapa, pero no objetos de datos.
#' @seealso \link{datlan.camp64}, \link{qcdistlan.camp64}
#' @examples 
#' \dontrun{
#' arMapLansGPS64("N20","cant","local",xlims=c(-9.8,-7.8),ylims=c(43.5,44.2))
#' }
#' @family mapas
#' @family PescaWin
#' @export
#' @export
arMapLansGPS64 <- function(camp, zona = "porc",
                           dns = c("local","serv"),
                           incl0 = FALSE,
                           xlims = NA, ylims = NA,
                           col = 2, lwd = 2, cexlab = .6,
                           places = TRUE, Nlans = FALSE,
                           es = TRUE, bw = FALSE,
                           layout = NA,
                           restore.par = TRUE) {
  
  dns <- match.arg(dns)
  
  datlan <- datlan.camp64(camp = camp, zona = zona, dns = dns,
                          incl2 = TRUE, incl0 = incl0)
  
  # ---- Defaults de límites según zona, sólo si no se han especificado ----
  if (any(is.na(xlims)) || any(is.na(ylims))) {
    def <- switch(zona,
                  cant = list(xlims = c(-10.25, -1.40), ylims = c(41.82, 44.48)),
                  porc = list(xlims = c(-15.50, -8.50), ylims = c(50.90, 54.50)),
                  arsa = list(xlims = c( -7.75,  -5.98), ylims = c(35.9, 37.35)),
                  stop("zona no contemplada (cant|porc|arsa)"))
    if (any(is.na(xlims))) xlims <- def$xlims
    if (any(is.na(ylims))) ylims <- def$ylims
  }
  
  # ---- restore.par PRIMERO, antes de tocar par() ----
  if (restore.par) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
  }
  
  # ---- Layout ----
  if (length(layout) == 2 && !any(is.na(layout))) {
    par(mfrow = layout)
  } else {
    par(mfrow = switch(zona, cant = c(2,1), porc = c(1,2), arsa = c(1,2)),
        mar   = switch(zona,
                       cant = c(3, 3,   2, 1)   + 0.1,
                       porc = c(3, 3,   2, 3)   + 0.1,
                       arsa = c(3, 3,   2, 3)   + 0.1))  }
  
  # ---- Llamadas a los mapas ----
  armap64(camp = camp, zona = zona, dns = dns,
          xlims = xlims, ylims = ylims, lwd = lwd,
          places = places, Nlans = TRUE, es = es, bw = bw)
  
  MapLansGPS64(camp = camp, zona = zona, dns = dns, incl0 = incl0,
               xlims = xlims, ylims = ylims, places = places,
               Nlans = FALSE, es = es, bw = bw)
  
  invisible(NULL)
}