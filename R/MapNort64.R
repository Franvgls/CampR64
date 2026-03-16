#' Mapa base Mar Cantábrico y costa norte de España
#'
#' Dibuja el mapa base del área del Cantábrico usando sf para los polígonos
#' y maps::map para gestionar automáticamente el aspecto geográfico correcto.
#'
#' @param lwdl grosor de línea de costa
#' @param cuadr cuadrícula de muestreo
#' @param cuadrcol color de la cuadrícula
#' @param cuadrMSFD cuadrícula MSFD
#' @param ICESrect rectángulos ICES
#' @param ICESrectcol color rectángulos ICES
#' @param ICESlab etiquetas rectángulos ICES
#' @param ICESlabcex tamaño etiquetas ICES
#' @param Sects dibujar sectores de la campaña
#' @param SecLty tipo de línea de los sectores
#' @param leg leyenda de estratos de profundidad
#' @param bw blanco y negro
#' @param es etiquetas en español
#' @param ax dibujar ejes
#' @param strat sombrear estratos de profundidad
#' @param places dibujar ciudades
#' @param FU Functional Units a dibujar
#' @param ColFU color de las FU
#' @param FUsLab etiquetas de las FU
#' @param dens densidad de tramado de las FU
#' @param xlims límites de longitud
#' @param ylims límites de latitud
#' @export
MapNort64 <- function(
    lwdl        = .5,
    cuadr       = FALSE,
    cuadrcol    = gray(.4),
    cuadrMSFD   = FALSE,
    ICESrect    = FALSE,
    ICESrectcol = gray(.2),
    ICESlab     = FALSE,
    Sects       = TRUE,
    SecLty      = 2,
    ICESlabcex  = .7,
    leg         = FALSE,
    bw          = FALSE,
    es          = FALSE,
    ax          = TRUE,
    strat       = FALSE,
    places      = FALSE,
    FU          = NA,
    ColFU       = "chartreuse",
    FUsLab      = FALSE,
    dens        = 20,
    xlims       = c(-9.9,-1.4),  
    ylims       = c(41.91,44.19)
) {

  # ── 1. CARGA DEL MAPA (Nort.sf) ───────────────────────────────────────────
  if (!exists("Nort.sf")) {
    data("Nort.sf", package = "CampR64")
  }

  # Convertir sf a polígonos base R
  polys <- lapply(seq_len(nrow(Nort.sf)), function(i) {
    coords <- sf::st_coordinates(Nort.sf[i, ])
    list(x = coords[, 1], y = coords[, 2], name = Nort.sf$name[i])
  })

  # ── 2. LIENZO ─────────────────────────────────────────────────────────────
  # maps::map con type="n" gestiona el asp automáticamente igual que MapNort
  # original — es el mismo truco que usa IBTSNeAtl_map_sf
  # Añadir antes de maps::map:
  par(xaxs = "i", yaxs = "i")
  maps::map("world", xlim = xlims, ylim = ylims, type = "n")  

  # ── 3. FONDO (MAR) ────────────────────────────────────────────────────────
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4],
       col = ifelse(bw, "white", "lightblue1"), border = NA)

  # ── 4. CUADRÍCULAS ────────────────────────────────────────────────────────
  if (cuadr) {
    abline(h = seq(41, 45, by = 1/12),    col = cuadrcol, lwd = .5)
    abline(v = seq(-12, 0,  by = 3/26),   col = cuadrcol, lwd = .5)
  }
  if (cuadrMSFD) {
    abline(h = seq(31, 45, by = 1/6),       col = cuadrcol, lwd = .5)
    abline(v = seq(-12, 0, by = 0.2174213), col = cuadrcol, lwd = .5)
  }
  if (ICESrect) {
    abline(v = seq(-12, -1, 1),   col = ICESrectcol, lwd = .5)
    abline(h = seq(41, 45, .5),   col = ICESrectcol, lwd = .5)
  }

  # ── 5. SECTORES ───────────────────────────────────────────────────────────
  if (Sects && exists("Nort.str")) {
    lines(Nort.str, lty = SecLty, lwd = .8, col = "grey40")
  }

  # ── 6. ESTRATOS DE PROFUNDIDAD (debajo de la tierra) ─────────────────────
  if (strat) {
    for (p in polys) {
      if (grepl("\\.a$", p$name))
        polygon(p$x, p$y, col = ifelse(bw, gray(.8), "lightblue"), border = NA)
      if (grepl("\\.b$", p$name))
        polygon(p$x, p$y, col = ifelse(bw, gray(.6), "blue"),      border = NA)
      if (grepl("\\.c$", p$name))
        polygon(p$x, p$y, col = ifelse(bw, gray(.3), "darkblue"),  border = NA)
    }
    abline(h = 43, v = c(-7.66, -6, -3.5), lty = 1, col = gray(0), lwd = 2)
    text(c(-9.8, -9.5, -6.83, -4.75, -2.7),
         c(42.4,  44.3,  44.3,  44.3,  44.3),
         c("MF", "FE", "EP", "PA", "AB"), font = 2)
  }

  # ── 7. ICES LABELS ────────────────────────────────────────────────────────
  if (ICESlab && exists("Area")) {
    text(Area$stat_x, Area$stat_y + 0.22,
         labels = Area$ICESNAME, cex = ICESlabcex, font = 2)
  }

  # ── 8. FUNCTIONAL UNITS ───────────────────────────────────────────────────
  if (any(!is.na(FU))) {
    draw_FU <- function(obj_name, lab, pos_fn = NULL) {
      if (!exists(obj_name, inherits = TRUE)) return()
      obj <- get(obj_name, inherits = TRUE)
      polygon(obj[, "long"], obj[, "lat"],
              density = dens, col = ColFU, border = "red", lwd = 3)
      if (FUsLab && !is.null(pos_fn)) {
        r <- pos_fn(obj)
        text(r$x, r$y, labels = lab, cex = 0.8, font = 2, pos = r$pos, col = 2)
      }
    }
    if (any(stringr::str_detect(FU, "FU26")))
      draw_FU("FU26", "FU26",
              function(o) list(x = min(o[,"long"]) - 0.55,
                               y = max(o[,"lat"])  + 0.1, pos = 4))
    if (any(stringr::str_detect(FU, "FU25")))
      draw_FU("FU25", "FU25",
              function(o) list(x = max(o[,"long"]) - 0.55,
                               y = max(o[,"lat"])  + 0.1, pos = 4))
    if (any(stringr::str_detect(FU, "FU31")))
      draw_FU("FU31", "FU31",
              function(o) list(x = min(o[,"long"]) - 0.1,
                               y = max(o[,"lat"])  + 0.1, pos = 1))
  }

  # ── 9. TIERRA (encima de todo lo marino) ─────────────────────────────────
  for (p in polys) {
    if (p$name %in% c("Costa", "Tierra", "Land", "Coastline"))
      polygon(p$x, p$y,
              col    = ifelse(bw, "gray80", "wheat"),
              border = "black", lwd = lwdl)
  }

  # ── 10. LEYENDA DE ESTRATOS ───────────────────────────────────────────────
  if (strat && leg) {
    legend("bottomright",
           legend = c("A: 70-120 m", "B: 121-200 m", "C: 201-500 m"),
           fill   = if (bw) c(gray(.8), gray(.6), gray(.3))
                    else    c("lightblue", "blue", "darkblue"),
           title  = ifelse(es, "Estr. prof", "Depth strata"),
           border = NA, bty = "n", cex = .8, inset = .05, bg = "white")
  }

  # ── 11. CIUDADES ──────────────────────────────────────────────────────────
  if (places) {
    cities <- data.frame(
      name = c("A Coruña", "Vigo", "Gijon", "Santander",
                "Bilbao", "San Sebastian"),
      lon  = c(-8.383, -8.7167, -5.6619, -3.81,  -2.934, -1.9884),
      lat  = c(43.367,  42.233,  43.544,  43.47,  43.26,  43.3205)
    )
    points(cities$lon, cities$lat, pch = 15, cex = .9)
    text(cities$lon, cities$lat, labels = cities$name,
         pos = 1, cex = .85, font = 2)
  }

  # ── 12. EJES ──────────────────────────────────────────────────────────────
  # usr refleja los límites reales que maps::map estableció
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

    box(lwd = 2)
  }

  invisible(par("usr"))
}
