#' VedasBesugo: Vedas Besugo Cantábrico y Galicia
#'
#' Polígonos que delimitan las zonas de veda de juveniles de besugo en el Cantábrico, la zona de veda fue propuesta por los armadores españoles en 2019, sin llegar a entrar en vigor. Los cinco polígonos van separados por filas NA, de modo que se pueden dibujar directamente con \code{lines} o \code{polygon}.
#' @name VedasBesugo
#' @docType data
#' @title Polígonos áreas de veda besugo Cantábrico
#' @usage data(VedasBesugo)
#' @format A data.frame with long and lat points defining the closed areas, polygons separated by NA
#' @references Propuesta de vedas protección juveniles besugo \url{https://www.mapa.gob.es/pesca/participacion-publica/detalle/nuevas_vedas_cantabrico}
#' @source Generado con \code{data-raw/VedasBesugo.R}
#' @examples
#' MapNort64(); lines(lat ~ long, VedasBesugo, col = 2, lwd = 2)
#' @family mapas
#' @family Cantábrico Galicia
"VedasBesugo"
