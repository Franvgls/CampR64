#' Saca el grid de cuadrículas para Porcupine
#'
#' Crea el grid de cuadrículas de Porcupine con los puntos centrales de cada
#' una de las cuadrículas (5x5 millas náuticas) disponibles en la campaña
#' @param color Color de las etiquetas (default 2)
#' @param cexlab Tamaño de las etiquetas (default 0.7)
#' @param etiq Si TRUE muestra etiquetas de número de cuadrícula
#' @seealso {\link{sorteo64}}, {\link{mapsorteo64}}
#' @return Data.frame con columnas x, y, pt, strat
#' @export
sacagrid64 <- function(color = 2, cexlab = .7, etiq = TRUE) {
  MapPorc64(cuadr = TRUE, lwdl = 2)
  dumb <- data.frame(x = NULL, y = NULL, pt = NULL, strat = NULL)
  i2 <- 1
  strat_names <- Porc.map$names[Porc.map$names != "narr"]
  for (nstrat in 1:length(strat_names)) {
    for (i in seq(54 - (.5/12), 51 - (.5/12), by = -1/12)) {
      for (i1 in seq(-15 + (1.5/23), -12 + (21/23), by = 3/23)) {
        where <- maps::map.where(Porc.map, i1, i)
        if (!is.na(where) && where == strat_names[nstrat]) {
          dumb <- rbind(dumb, data.frame(x = i1, y = i, pt = i2,
                        strat = substr(strat_names[nstrat], 1, 2)))
          if (etiq) text(i1, i, labels = i2, cex = cexlab, col = color)
          i2 <- i2 + 1
        }
      }
    }
  }
  dumb
}
