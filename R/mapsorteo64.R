
#' Mapa de las cuadrículas sorteadas para la campaña Porcupine
#'
#' Mapa del resultado de un sorteo de cuadrículas de Porcupine
#' @param x Resultado de sorteo64()
#' @param wmf Si TRUE guarda en metafile Windows
#' @param ax Si TRUE muestra ejes
#' @param nomfic Nombre del fichero metafile (default "porco.emf")
#' @param lwdl Ancho de líneas de estratos
#' @param colo Color de líneas del mapa
#' @param add Si TRUE añade al gráfico existente
#' @param labs Si TRUE muestra números de cuadrícula, si FALSE muestra puntos
#' @param col Color de los puntos/etiquetas
#' @param cex Tamaño de etiquetas si labs=TRUE
#' @seealso {\link{sorteo64}}
#' @export
mapsorteo64 <- function(x, wmf = FALSE, ax = TRUE, nomfic = "porco.emf",
                        lwdl = .5, colo = NA, add = FALSE, labs = FALSE,
                        col = 2, cex = .9) {
  asp <- diff(c(50.5, 54.5)) / (diff(range(-15.5, -10.5)) * cos(mean(50.5, 54.5) * pi/180))
  if (wmf) win.metafile(filename = nomfic, width = 10, height = 10 * asp + .63, pointsize = 10)
  if (!wmf & ax) par(mar = c(2, 2.5, 2, 2.5) + 0.3)
  if (!ax) par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), omd = c(0, 0, 0, 0))
  if (!add) MapPorc64(lwdl = lwdl, cuadr = TRUE, ax = ax)
  if (!labs) {
    for (nstrat in seq_along(Porc.map$names)) {
      points(x[x[, 4] == substr(Porc.map$names[nstrat], 1, 2), 1:2],
             pch = 18 + nstrat, col = 1, bg = colo, cex = .9, lwd = 2)
    }
  } else {
    text(x$x, x$y, labels = x$pt, cex = cex, font = 2, col = col)
  }
  if (wmf) dev.off()
  par(mar = c(5, 4, 4, 2) + 0.1)
}
