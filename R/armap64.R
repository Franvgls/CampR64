#' Mapa de desarrollo de campaña — función unificada para cant, porc y arsa
#'
#' @description
#' Sustituye a armap.camp64 y armap.tot64. Dibuja el mapa base adecuado según
#' la zona y añade los lances realizados con sus opciones de visualización.
#'
#' @param camp Campaña: "NXX" Demersales, "PXX" Porcupine, "1XX"/"2XX" ARSA
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param lans Si TRUE muestra puntos de lance (default TRUE)
#' @param Nlans Si TRUE muestra números de lance en vez de puntos
#' @param lan0 Si TRUE incluye lances nulos (validez=0) representados con pch=13 (aspa)
#' @param arrows Si TRUE dibuja flechas del recorrido lance a lance
#' @param CTDs Si TRUE añade estaciones CTD
#' @param NCTDs Si TRUE añade números de estación CTD
#' @param Dates Si TRUE añade fecha en cada lance
#' @param bw Si TRUE mapa en blanco y negro (mar blanco, tierra gris); si FALSE mar en lightblue
#' @param strat Si TRUE dibuja líneas de contorno de estratos
#' @param leg Si TRUE añade leyenda (de lances y/o estratos si strat=TRUE)
#' @param es Si TRUE textos en castellano; si FALSE en inglés
#' @param ti Si TRUE añade título con el nombre de la campaña
#' @param cuadr Si TRUE añade cuadrículas (solo cant)
#' @param cuadrMSFD Si TRUE añade cuadrículas MSFD (solo cant)
#' @param xlims Límites de longitud (calculados automáticamente si NULL)
#' @param ylims Límites de latitud (calculados automáticamente si NULL)
#' @param lwdl Grosor de líneas de flechas (default 1)
#' @param argr Grosor de cabeza de flechas (default 2)
#' @param graf Si FALSE el gráfico va a pantalla; si es un string guarda como PNG con ese nombre
#' @param xpng Anchura del PNG en píxeles (default 800); la altura se calcula automáticamente por asp
#' @examples
#' armap64("P25", zona="porc", arrows=TRUE, leg=TRUE)
#' armap64("N24", zona="cant", Nlans=TRUE, strat=FALSE)
#' armap64("224", zona="arsa", bw=FALSE, leg=TRUE)
#' @family mapas
#' @export
armap64 <- function(camp, zona = "cant",
                    dns    = c("local","serv"),
                    # Lances
                    lans   = TRUE,
                    Nlans  = FALSE,
                    lan0  = TRUE,
                    # Recorrido
                    arrows = FALSE,
                    lwdl   = 1,
                    argr   = 2,
                    # Extras
                    CTDs   = FALSE,
                    NCTDs  = FALSE,
                    Dates  = FALSE,
                    # Mapa base
                    bw     = FALSE,
                    strat  = FALSE,
                    cuadr  = FALSE,
                    cuadrMSFD = FALSE,
                    # Presentación
                    places = TRUE,
                    leg    = TRUE,
                    es     = FALSE,
                    ti     = FALSE,
                    xlims  = NULL,
                    ylims  = NULL,
                    graf   = FALSE,
                    xpng   = 800) {

  if (length(camp) > 1)
    stop("Solo se puede representar una campaña a la vez.")

  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  # ── 1. LANCES ──────────────────────────────────────────────────────────────
  ln <- datlan.camp64(camp = camp, zona = zona, dns = dns,
                      redux = TRUE, incl2 = TRUE, incl0 = TRUE)

  # Ordenar por fecha y hora
  if ("hora_l" %in% names(ln)) {
    ord <- order(as.character(ln$fecha), ln$hora_l, ln$lance)
  } else {
    ord <- order(as.character(ln$fecha), ln$lance)
  }
  ln <- ln[ord, , drop = FALSE]

  # Rangos automáticos
  if (is.null(xlims)) {
    xr    <- range(ln$lon, na.rm = TRUE)
    pad   <- max(diff(xr) * 0.05, 0.1)
    xlims <- c(xr[1] - pad, xr[2] + pad)
  }
  if (is.null(ylims)) {
    yr    <- range(ln$lat, na.rm = TRUE)
    pad   <- max(diff(yr) * 0.05, 0.1)
    ylims <- c(yr[1] - pad, yr[2] + pad)
  }
  if (zona=="porc") {
    xlims <- c(-15.5,-7.8)
    ylims <- c(50.5,54.5)
  }
  if (zona=="arsa") {
    xlims = c(-8.15, -5.5)
    ylims = c(35.96, 37.334)
  }
   
  # ── asp y apertura PNG ────────────────────────────────────────────────────
  asp <- diff(ylims) / (diff(xlims) * cos(mean(ylims) * pi / 180))
  if (!is.logical(graf)) {
    ypng <- round(xpng * asp)
    png(filename = paste0(graf, ".png"), width = xpng, height = ypng, pointsize = 12)
    on.exit(dev.off(), add = TRUE)
  }

  # ── 2. CTDs (opcional) ─────────────────────────────────────────────────────
  hid <- NULL
  if (isTRUE(CTDs) || isTRUE(NCTDs)) {
    hid <- try(readCampDBF("hidro", zona = zona, camp = camp, dns = dns),
               silent = TRUE)
    if (inherits(hid, "try-error") || !is.data.frame(hid) || nrow(hid) == 0) {
      warning("No hay HIDRO para ", camp, "; no se dibujan CTDs.")
      hid  <- NULL
      CTDs <- FALSE
    } else {
      if (all(c("latitud","longitud","eswe") %in% names(hid))) {
        hid$latitud  <- gradec(as.numeric(hid$latitud))
        hid$longitud <- gradec(as.numeric(hid$longitud)) *
          ifelse(toupper(trimws(hid$eswe)) == "W", -1, 1)
      } else {
        warning("HIDRO sin columnas latitud/longitud/eswe.")
        hid  <- NULL
        CTDs <- FALSE
      }
    }
  }

  # ── 3. MAPA BASE ───────────────────────────────────────────────────────────
  if (zona == "porc") {
    maparea64(es = es, leg = FALSE, places=places, bw = bw, strat = strat, xlims = xlims, ylims = ylims)
  } else if (zona == "cant") {
    MapNort64(strat = strat, bw = bw, es = es,
              cuadr = cuadr, cuadrMSFD = cuadrMSFD,
              xlims = xlims, ylims = ylims, places = places)
  } else if (zona == "arsa") {
    MapArsa64(es = es, bw = bw, strat = strat,
              xlims = xlims, ylims = ylims, places = places)
  } else {
    stop("zona debe ser 'cant', 'porc' o 'arsa'.")
  }

  # ── 4. TÍTULO ──────────────────────────────────────────────────────────────
  if (isTRUE(ti)) {
    cp <- try(readCampDBF("camp", zona = zona, camp = camp, dns = dns),
              silent = TRUE)
    if (!inherits(cp, "try-error") && "ident" %in% names(cp))
      title(as.character(cp$ident[1]), line = 1.5, cex.main = 1.1)
  }

  # ── 5. COLORES ─────────────────────────────────────────────────────────────
  col_val  <- if (bw) "gray40" else "blue"
  col_esp  <- if (bw) "black"  else "red"
  col_nul  <- "black"

  # ── 6. FLECHAS DE RECORRIDO ────────────────────────────────────────────────
  if (isTRUE(arrows) && nrow(ln) > 1) {
    x1 <- ln$lon[-nrow(ln)]; y1 <- ln$lat[-nrow(ln)]
    x2 <- ln$lon[-1];        y2 <- ln$lat[-1]
    arrows(x1, y1, x2, y2,
           length = 0.05,
           lwd    = lwdl,
           col    = if (bw) "gray30" else "gray40")
  }

  # ── 7. LANCES — puntos o números ───────────────────────────────────────────
  val  <- ln$validez == 1
  esp  <- ln$validez >  1
  nuls <- ln$validez == 0

  if (isTRUE(Nlans)) {
    # Solo números, sin puntos
    if (any(val,  na.rm = TRUE))
      text(ln$lon[val],  ln$lat[val],
           labels = ln$lance[val],  cex = 0.8, font = 2, col = col_val)
    if (any(esp,  na.rm = TRUE))
      text(ln$lon[esp],  ln$lat[esp],
           labels = ln$lance[esp],  cex = 0.8, font = 2, col = col_esp)
    if (isTRUE(lan0) && any(nuls, na.rm = TRUE))
      text(ln$lon[nuls], ln$lat[nuls],
           labels = ln$lance[nuls], cex = 0.8, font = 2, col = col_nul)

  } else if (isTRUE(lans)) {
    # Puntos
    if (any(val,  na.rm = TRUE))
      points(ln$lon[val],  ln$lat[val],
             pch = 21, cex = 1.0, bg = col_val)
    if (any(esp,  na.rm = TRUE))
      points(ln$lon[esp],  ln$lat[esp],
             pch = 21, cex = 1.0, bg = col_esp)
    if (isTRUE(lan0) && any(nuls, na.rm = TRUE))
      points(ln$lon[nuls], ln$lat[nuls],
             pch = 13, cex = 1.1)
  }

  # ── 8. FECHAS ──────────────────────────────────────────────────────────────
  if (isTRUE(Dates)) {
    dt <- try(as.Date(ln$fecha), silent = TRUE)
    if (!inherits(dt, "try-error"))
      text(ln$lon, ln$lat, labels = format(dt, "%d-%m"), cex = 0.7, pos = 1)
  }

  # ── 9. CTDs ────────────────────────────────────────────────────────────────
  if (!is.null(hid)) {
    if (isTRUE(CTDs))
      points(hid$longitud, hid$latitud,
             pch = 25, cex = 0.8,
             bg  = if (bw) "white" else "lightblue")
    if (isTRUE(NCTDs) && "estn" %in% names(hid))
      text(hid$longitud, hid$latitud,
           labels = hid$estn, cex = 0.8, font = 2, col = 2)
  }

  # ── 10. LEYENDA ────────────────────────────────────────────────────────────
  if (isTRUE(leg) && (isTRUE(lans) || isTRUE(Nlans))) {
    l_val <- ifelse(es, "Lances válidos",   "Valid tows")
    l_esp <- ifelse(es, "Lances especiales","Extra tows")
    l_nul <- ifelse(es, "Lances nulos",     "Null tows")
    l_ctd <- "CTD"

    if (isTRUE(Nlans)) {
      # Leyenda con texto en vez de puntos
      leg_txt  <- c(l_val, l_esp)
      leg_col  <- c(col_val, col_esp)
      if (isTRUE(lan0)) { leg_txt <- c(leg_txt, l_nul); leg_col <- c(leg_col, col_nul) }
      if (!is.null(hid) && isTRUE(CTDs)) { leg_txt <- c(leg_txt, l_ctd) }
      legend("bottomright", legend = leg_txt, text.col = leg_col,
             bg = "white", inset = 0.05, cex = 0.9)
    } else {
      leg_pch  <- c(21, 21)
      leg_bg   <- c(col_val, col_esp)
      leg_txt  <- c(l_val, l_esp)
      leg_cex  <- c(1.1, 1.1)
      if (isTRUE(lan0)) {
        leg_txt <- c(leg_txt, l_nul); leg_pch <- c(leg_pch, 13)
        leg_bg  <- c(leg_bg,  NA);    leg_cex  <- c(leg_cex, 1.1)
      }
      if (!is.null(hid) && isTRUE(CTDs)) {
        leg_txt <- c(leg_txt, l_ctd); leg_pch <- c(leg_pch, 25)
        leg_bg  <- c(leg_bg,  if (bw) "white" else "lightblue")
        leg_cex <- c(leg_cex, 1.0)
      }
      legend("bottomright", legend = leg_txt,
             pch    = leg_pch,
             pt.bg  = leg_bg,
             pt.cex = leg_cex,
             bg     = "white", inset = 0.05, cex = 0.9)
    }
  }

  invisible(ln)
}
