#' Saca el grid de cuadrículas para Demersales Norte
#'
#' Crea el grid de cuadrículas del Cantábrico/Galicia (5x5 millas náuticas)
#' @param color Color de las etiquetas (default 2)
#' @param cexlab Tamaño de las etiquetas (default 0.7)
#' @param etiq Si TRUE muestra etiquetas de número de cuadrícula
#' @seealso {\link{sacagrid64}}
#' @return Data.frame con columnas x, y, pt, strat
#' @export
sacagridNort64 <- function(color = 2, cexlab = .7, etiq = TRUE) {
  MapNort64(cuadr = TRUE, lwdl = 2)
  dumb <- data.frame(x = NULL, y = NULL, pt = NULL, strat = NULL)
  i2 <- 1
  strat_names <- Nort.map$names[Nort.map$names != "Costa" & Nort.map$names != "Litoral"]
  for (nstrat in 1:length(strat_names)) {
    for (i in seq(41.5 - (.5/12), 45 - (.5/12), by = 1/12)) {
      for (i1 in seq(-12 + (1.5/26), -1 + (1.5/26), by = 3/26)) {
        where <- maps::map.where(Nort.map, i1, i)
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
