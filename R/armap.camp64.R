armap.camp64 <- function(camp, zona,
                         dns = c("local","serv"),
                         ti = FALSE,
                         lans = TRUE,
                         Nlans = FALSE,
                         CTDs = FALSE,
                         NCTDs = FALSE,
                         Dates = FALSE,
                         arrow = FALSE,
                         xlims = NULL, ylims = NULL,
                         map_fn = NULL,
                         add_strata_fn = NULL,
                         bw = FALSE,
                         places = FALSE,
                         strat  = FALSE,
                         cuadr  = FALSE,
                         cuadrMSFD = FALSE) {
  
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)
  
  # ── 1. LANCES — usando datlan.camp64 que ya convierte coordenadas ────────
  ln <- datlan.camp64(camp = camp, zona = zona, dns = dns,
                      redux = TRUE, incl2 = TRUE, incl0 = TRUE)
  # datlan devuelve: lance, lat, lon, prof, validez, fecha... en minúsculas
  
  # Ordenar por fecha y hora
  if ("hora_l" %in% names(ln)) {
    ord <- order(as.character(ln$fecha), ln$hora_l, ln$lance)
  } else {
    ord <- order(as.character(ln$fecha), ln$lance)
  }
  ln <- ln[ord, , drop = FALSE]
  
  # Rangos automáticos si no se pasan
  if (is.null(xlims)) {
    xr   <- range(ln$lon, na.rm = TRUE)
    pad  <- diff(xr) * 0.05
    xlims <- c(xr[1] - pad, xr[2] + pad)
  }
  if (is.null(ylims)) {
    yr   <- range(ln$lat, na.rm = TRUE)
    pad  <- diff(yr) * 0.05
    ylims <- c(yr[1] - pad, yr[2] + pad)
  }
  
  # ── 2. HIDRO (opcional) ──────────────────────────────────────────────────
  hid <- NULL
  if (isTRUE(CTDs) || isTRUE(NCTDs)) {
    hid <- try(readCampDBF("hidro", zona = zona, camp = camp, dns = dns),
               silent = TRUE)
    if (inherits(hid, "try-error") || !is.data.frame(hid) || nrow(hid) == 0) {
      warning("No hay HIDRO para ", camp, "; no se dibujan CTDs.")
      hid <- NULL
    } else {
      # hidro ya en minúsculas desde read_dbf_simple
      if (all(c("latitud","longitud","eswe") %in% names(hid))) {
        hid$latitud  <- gradec(as.numeric(hid$latitud))
        hid$longitud <- gradec(as.numeric(hid$longitud)) *
          ifelse(toupper(trimws(hid$eswe)) == "W", -1, 1)
      } else {
        warning("HIDRO sin columnas latitud/longitud/eswe.")
        hid <- NULL
      }
    }
  }
  
  # ── 3. TÍTULO ────────────────────────────────────────────────────────────
  title_txt <- NULL
  if (isTRUE(ti)) {
    cp <- readCampDBF("camp", zona = zona, camp = camp, dns = dns)
    if ("ident" %in% names(cp)) title_txt <- as.character(cp$ident[1])
  }
  
  # ── 4. MAPA BASE ─────────────────────────────────────────────────────────
  # if (is.null(map_fn)) {
  #   plot(NA, NA, xlim = xlims, ylim = ylims,
  #        xlab = ifelse(zona == "cant", "Longitud", "Longitude"),
  #        ylab = ifelse(zona == "cant", "Latitud",  "Latitude"),
  #        asp  = 1 / cos(mean(ylims) * pi / 180),
  #        axes = TRUE)
  #   box()
  # } else {
  #   map_fn(xlims = xlims, ylims = ylims, bw = bw, es = TRUE,
  #          places = places, strat = strat,
  #          cuadr = cuadr, cuadrMSFD = cuadrMSFD)
  # }
  # if (!is.null(add_strata_fn)) try(add_strata_fn(), silent = TRUE)
  # if (!is.null(title_txt)) title(title_txt, line = 1.5, cex.main = 1.1)
  MapNort64()
  # ── 5. PUNTOS Y ETIQUETAS ────────────────────────────────────────────────
  col_valid <- if (bw) "gray40" else "blue"
  col_spec  <- if (bw) "black"  else "red"
  
  if (isTRUE(arrow) && lans) {
    x1 <- ln$lon[-nrow(ln)]; y1 <- ln$lat[-nrow(ln)]
    x2 <- ln$lon[-1];        y2 <- ln$lat[-1]
    arrows(x1, y1, x2, y2, length = 0.05, col = if (bw) 1 else "gray40")
  }
  
  if (isTRUE(lans)) {
    nulos <- ln$validez == 0
    val   <- ln$validez == 1
    esp   <- ln$validez > 1
    if (any(nulos, na.rm = TRUE))
      points(ln$lon[nulos], ln$lat[nulos], pch = 13, cex = 1.1)
    if (any(val, na.rm = TRUE))
      points(ln$lon[val], ln$lat[val], pch = 21, cex = 1.0, bg = col_valid)
    if (any(esp, na.rm = TRUE))
      points(ln$lon[esp], ln$lat[esp], pch = 21, cex = 1.0, bg = col_spec)
  }
  
  if (isTRUE(Nlans)) {
    text(ln$lon, ln$lat,
         labels = ln$lance, cex = 0.8, font = 2, pos = 3)
    lab_idx <- which(ln$validez != 1)
    if (length(lab_idx))
      text(ln$lon[lab_idx], ln$lat[lab_idx],
         labels = ln$lance[lab_idx], cex = 0.8, font = 2, pos = 3,col="red")
  }
  
  if (isTRUE(Dates)) {
    dt <- try(as.Date(ln$fecha), silent = TRUE)
    if (!inherits(dt, "try-error"))
      text(ln$lon, ln$lat, labels = format(dt, "%d-%m"), cex = 0.7)
  }
  
  if (!is.null(hid)) {
    if (isTRUE(CTDs))
      points(hid$longitud, hid$latitud, pch = 25, cex = 0.8,
             bg = if (bw) "white" else "lightblue")
    if (isTRUE(NCTDs) && "estn" %in% names(hid))
      text(hid$longitud, hid$latitud, labels = hid$estn,
           cex = 0.8, font = 2, col = 2)
  }
  
  # ── 6. LEYENDA ───────────────────────────────────────────────────────────
  if (lans) {
    legend("bottomright",
           legend = c(ifelse(zona == "cant", "Lances válidos",   "Valid tows"),
                      ifelse(zona == "cant", "Lances especiales", "Extra tows"),
                      ifelse(zona == "cant", "Lances nulos",      "Null tows")),
           pch    = c(21, 21, 13),
           pt.bg  = c(col_valid, col_spec, NA),
           pt.cex = c(1.1, 1.1, 1.1),
           bg = "white", inset = 0.05, cex = 0.9)
  }
  if (lans & CTDs) {
    legend("bottomright",
           legend = c(ifelse(zona == "cant", "Lances válidos",   "Valid tows"),
                      ifelse(zona == "cant", "Lances especiales", "Extra tows"),
                      ifelse(zona == "cant", "Lances nulos",      "Null tows"),
                      ifelse(zona == "cant", "CTDs", "CTDs")),
           pch    = c(21, 21, 13,25),
           pt.bg  = c(col_valid, col_spec, NA,"lightblue"),
           pt.cex = c(1.1, 1.1, 1.1,1.0),
           bg = "white", inset = 0.05, cex = 0.9)
  }
}