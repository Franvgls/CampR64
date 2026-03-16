#' Mapa base Banco de Porcupine
#' @export
MapPorc64 <- function(
    xlims   = c(-15.5, -10.5),
    ylims   = c(50.5,  54.5),
    lwdl    = 1,
    cuadr   = FALSE,
    ICESrect = FALSE,
    label   = FALSE,
    ax      = TRUE,
    bw      = FALSE,
    es      = FALSE,
    places  = FALSE,
    corners = FALSE
) {
  par(mar = c(2, 2.5, 2, 2.5) + 0.3, mgp = c(2, .5, 0))
  
  # ── Lienzo — Porc.map es clase "map", funciona directamente ──────────────
  maps::map(Porc.map, xlim = xlims, ylim = ylims, type = "n")
  plot.window(xlim = xlims, ylim = ylims, xaxs = "i", yaxs = "i")
  
  # ── Fondo mar ─────────────────────────────────────────────────────────────
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4],
       col = if (bw) "white" else "lightblue1", border = NA)
  
  # ── Cuadrículas ───────────────────────────────────────────────────────────
  if (cuadr) {
    abline(h = seq(50, 55, by = 1/12), col = gray(.6), lwd = .6)
    abline(v = seq(-18, -10, by = 3/23), col = gray(.6), lwd = .6)
  }
  if (ICESrect) {
    abline(h = seq(50, 55, by = .5), col = gray(.2), lwd = .6)
    abline(v = seq(-18, -10, by = 1), col = gray(.2), lwd = .6)
  }
  
  # ── Tierra ────────────────────────────────────────────────────────────────
  nstrat <- length(which(!is.na(Porc.map$names)))
  nland  <- length(Porc.map$names) - nstrat
  maps::map(Porc.map, add = TRUE, fill = TRUE,
            col = c(rep(NA, nstrat),
                    rep(if (bw) "gray85" else "wheat", nland)),
            lwd = lwdl)
  
  # ── Ciudades ──────────────────────────────────────────────────────────────
  if (places) {
    points(-6.260278, 53.349722, pch = 15, cex = .9)
    text(-6.260278,   53.349722, "Dublin", cex = .85, font = 2, pos = 1)
  }
  
  # ── Esquinas de referencia ────────────────────────────────────────────────
  if (corners)
    points(c(-15.5, -10.5), c(50.5, 54.5), pch = 16, col = 2)
  
  # ── Ejes ──────────────────────────────────────────────────────────────────
  if (ax) {
    usr <- par("usr")
    degs_x <- seq(ceiling(usr[1]), floor(usr[2]),
                  ifelse(abs(diff(xlims)) > 1, 1, .5))
    alg <- sapply(degs_x, function(x) bquote(.(abs(x)) * degree ~ W))
    axis(1, at = degs_x, lab = do.call(expression, alg),
         font.axis = 2, cex.axis = .8, tck = -.01, mgp = c(1, .2, 0))
    axis(3, at = degs_x, lab = do.call(expression, alg),
         font.axis = 2, cex.axis = .8, tck = -.01, mgp = c(1, .2, 0))
    
    degs_y <- seq(ceiling(usr[3]), floor(usr[4]),
                  ifelse(abs(diff(ylims)) > 1, 1, .5))
    alt <- sapply(degs_y, function(x) bquote(.(x) * degree ~ N))
    axis(2, at = degs_y, lab = do.call(expression, alt),
         font.axis = 2, cex.axis = .8, tck = -.01, las = 2, mgp = c(1, .5, 0))
    axis(4, at = degs_y, lab = do.call(expression, alt),
         font.axis = 2, cex.axis = .8, tck = -.01, las = 2, mgp = c(1, .5, 0))
  }
  
  box(lwd = lwdl)
  invisible(par("usr"))
}