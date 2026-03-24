#' Sorteo de distribución de lances en Porcupine
#'
#' Realiza el sorteo de las cuadrículas de muestreo en la campaña de Porcupine
#' con el número correspondiente de estaciones por sector y estrato y aplicando
#' una distancia mínima entre lances contiguos
#' @param buffer Distancia mínima entre cuadrículas contiguas
#' @param srv.dsg Diseño del muestreo: data.frame con columnas strat y stations
#' @seealso {\link{mapsorteo64}}, {\link{sacagrid64}}
#' @return Data.frame con columnas x, y, pt, strat
#' @export
sorteo64 <- function(buffer = .1177, srv.dsg = Porc.dsg) {
  distgeogr <- function(a, b) {
    sqrt(sum((abs(a[1] - b[1]) * cos(mean(c(a[[2]], b[[2]])) * pi/180))^2,
             abs(a[2] - b[2])^2))
  }
  resample <- function(x, size, ...) {
    if (length(x) <= 1) {
      if (!missing(size) && size == 0) x[FALSE] else x
    } else sample(x, size, ...)
  }
  strats   <- data.frame(lab = srv.dsg[, 1], stat.no = srv.dsg[, 2])
  ordenstr <- sample(as.character(strats$lab))
  if (!exists("Porc.grid")) Porc.grid <- sacagrid64()
  gridbase <- Porc.grid
  hauls    <- data.frame(x = NULL, y = NULL, pt = NULL, strat = NULL)
  for (nstrat in 1:length(strats$lab)) {
    for (i in seq_len(strats$stat.no[strats$lab == ordenstr[nstrat]])) {
      hauls <- rbind(hauls,
                     gridbase[gridbase$pt == sample(
                       gridbase$pt[gridbase$strat == ordenstr[nstrat]], size = 1), ])
      gridbase <- gridbase[
        as.vector(apply(gridbase[, 1:2], 1, distgeogr,
                        b = hauls[nrow(hauls), 1:2])) > buffer, ]
      MapPorc64(cuadr = TRUE)
      points(gridbase[, 1:2], cex = .6)
      points(hauls[, 1:2], pch = 16, cex = .5, col = 2)
      if (length(gridbase$x[gridbase$strat == ordenstr[nstrat]]) == 0) {
        message("Only ", i, " stations selected in strata: ", ordenstr[nstrat])
        break
      }
    }
  }
  mapsorteo64(hauls)
  points(gridbase[, 1:2])
  hauls
}
