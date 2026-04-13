#' Mapa base Golfo de Cádiz (ARSA) con selección por regiones (modelo MapNort64)
#' Requiere objetos 'Arsa.map' y 'Arsa.str' en memoria (data/ del paquete).
#' @export
MapArsa64 <- function(xlims = c(-8.149, -5.52),
                      ylims = c(35.95, 37.3337),
                      lwdl = 1,
                      leg  = FALSE,             # sombrear estratos + leyenda
                      cuadr = FALSE,
                      cuadrMSFD = FALSE,
                      ICESrect  = FALSE,
                      FU   = NA, ColFU = "chartreuse", dens = NA, FUsLab = FALSE,
                      ax   = TRUE,
                      bw   = FALSE,
                      es   = TRUE,
                      places = TRUE,
                      sea_col  = "lightblue1",
                      land_col = "#D2B48C",     # tan (marrón/siena claro)
                      strata_cols = c("#CFE8FF","#9CC9FF","#6AA9FF","#2E7DD6","#0F4C81"),
                      wmf_file = NULL) {
  
  # --- abrir EMF opcional (Windows) ---
  dev_opened <- FALSE
  if (!is.null(wmf_file) && .Platform$OS.type == "windows") {
    asp <- diff(ylims) / (diff(xlims) * cos(mean(ylims) * pi/180))
    grDevices::win.metafile(filename = wmf_file, width = 10, height = 10*asp + .6, pointsize = 10)
    dev_opened <- TRUE
  }
  on.exit({ if (dev_opened) grDevices::dev.off() }, add = TRUE)
  
  # util: dibujar un map-subset con polypath
  .draw_map_regions <- function(db, regs, col, border = NA, lwd = 1, lty = 1) {
    if (length(regs) == 0) return(invisible())
    sub <- maps::map(db, regions = regs, fill = TRUE, plot = FALSE)
    if (!is.null(sub) && length(sub$x) > 1) {
      graphics::polypath(sub$x, sub$y, col = col, border = border, lwd = lwd, lty = lty)
    }
  }
  
  # marco
  par(mar = c(2, 2.5, 2, 2.5) + 0.3)
  maps::map("world", xlim = xlims, ylim = ylims, type = "n")
  plot.window(xlim = xlims, ylim = ylims, xaxs = "i", yaxs = "i")

    # mar
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
       col = if (bw) "white" else sea_col, border = NA)
  
  # rejillas
  if (cuadr)     abline(h = seq(31,45,by=1/12), v = seq(-12,0,by=0.089),       col = gray(.6), lwd = .6)
  if (ICESrect)  abline(h = seq(36,37,by=.5),    v = seq(-8,-5,by=1),          col = gray(.2), lwd = .6)
  if (cuadrMSFD) abline(h = seq(31,45,by=1/6),   v = seq(-12,0,by=0.2174213),  col = gray(.4), lwd = .5)
  
  # --- necesitas Arsa.map / Arsa.str en memoria ---
  if (!exists("Arsa.map", inherits = TRUE))
    stop("Arsa.map no está cargado (debe estar en data/ del paquete).")
  if (!exists("Arsa.str", inherits = TRUE))
    warning("Arsa.str no está cargado; no se dibujará el contorno discontínuo.")
  
  m <- get("Arsa.map", inherits = TRUE)
  
  # nombres disponibles (útil para depurar)
  nm <- if (!is.null(m$names)) m$names else character(0)
  
  # regiones por estrato (asumiendo nombres tipo "StrA...StrE")
  regsA <- grep("^StrA", nm, value = TRUE, ignore.case = TRUE)
  regsB <- grep("^StrB", nm, value = TRUE, ignore.case = TRUE)
  regsC <- grep("^StrC", nm, value = TRUE, ignore.case = TRUE)
  regsD <- grep("^StrD", nm, value = TRUE, ignore.case = TRUE)
  regsE <- grep("^StrE", nm, value = TRUE, ignore.case = TRUE)
  
  # regiones de tierra/Portugal = todas las que NO son StrA..StrE
  regs_str <- c(regsA, regsB, regsC, regsD, regsE)
  regs_land <- setdiff(nm, regs_str)
  
  # --- 1) estratos (debajo de la costa), solo si leg=TRUE ---
  maps::map(Arsa.map, add = TRUE, fill = TRUE,
            col = c(rep(NA, 5), ifelse(bw, "light gray", "wheat")),
            lwd = lwdl)
  maps::map(Arsa.map, add = TRUE, fill = TRUE,
            col = c(rep(NA, 6), ifelse(bw, "light gray", "wheat")),
            lwd = lwdl) 
  if (isTRUE(leg)) {
    if (!bw) {
      maps::map(Arsa.map,Arsa.map$names[grep("StrA",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue1")
      maps::map(Arsa.map,Arsa.map$names[grep("StrB",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue2")
      maps::map(Arsa.map,Arsa.map$names[grep("StrC",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="lightblue3")
      maps::map(Arsa.map,Arsa.map$names[grep("StrD",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="blue")
      maps::map(Arsa.map,Arsa.map$names[grep("StrE",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col="darkblue")
      legend("topright",c("A: 15-30 m","B: 30-70 m","C: 71-200 m","D: 201-500 m","E: 501-800 m"),fill=c("lightblue1","lightblue2","lightblue3","blue","darkblue"),title=ifelse(es,"Estr. prof","Depth strata"),cex=.8,inset=.05,bg="white")
    }
    else {
      maps::map(Arsa.map,Arsa.map$names[grep("StrA",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.9))
      maps::map(Arsa.map,Arsa.map$names[grep("StrB",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.8))
      maps::map(Arsa.map,Arsa.map$names[grep("StrC",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.6))
      maps::map(Arsa.map,Arsa.map$names[grep("StrD",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.5))
      maps::map(Arsa.map,Arsa.map$names[grep("StrE",Arsa.map$names,fixed=T)],add=TRUE,fill=TRUE,col=gray(.4))
      legend("topright",c("A: 15-30 m","B: 30-70 m","C: 71-200 m","D: 201-500 m","E: 501-800 m"),fill=c(gray(.9),gray(.8),gray(.6),gray(.5),gray(.4)),title=ifelse(es,"Estr. prof","Depth strata"),cex=.8,inset=.05,bg="white")
      legend("topright",
             c("A: 15-30 m","B: 30-70 m","C: 71-200 m","D: 201-500 m","E: 501-800 m"),
             fill  = cols_str,
             title = ifelse(es, "Estr. prof", "Depth strata"),
             cex = .8, inset = .05, bg = "white")
      }
    # }
  }  
  # --- 2) tierra/Portugal encima de los estratos ---
  # if (length(regs_land) > 0) {
  #   .draw_map_regions(m, regs_land, if (bw) "light gray" else land_col)
  # } else {
  #   # si no hay 'names', pinta todo m como tierra
  #   if (is.list(m) && all(c("x","y") %in% names(m)))
  #     graphics::polypath(m$x, m$y, col = if (bw) "light gray" else land_col, border = T)
  # }
  # --- 3) contorno discontínuo (Arsa.str) ---
  if (exists("Arsa.str", inherits = TRUE)) {
    s <- get("Arsa.str", inherits = TRUE)
    if (is.list(s) && all(c("x","y") %in% names(s))) {
      graphics::polypath(s$x, s$y, col = NA, border = gray(0.25), lwd = lwdl, lty = 2)
    } else {
      # fallback si Arsa.str fuese data.frame con long/lat
      nms <- tolower(names(s))
      lo  <- s[[nms[grep("long|lon|x", nms)[1]]]]
      la  <- s[[nms[grep("lat|y",  nms)[1]]]]
      graphics::polypath(lo, la, col = NA, border = gray(0.25), lwd = lwdl, lty = 2)
    }
  }
  
  # --- Functional Units (FU29/FU30) opcional ---
  draw_FU <- function(obj_name, lab) {
    if (!exists(obj_name, inherits = TRUE)) return()
    obj <- get(obj_name, inherits = TRUE)
    nms <- tolower(names(obj))
    lo  <- obj[[nms[grep("long|lon|x", nms)[1]]]]
    la  <- obj[[nms[grep("lat|y",  nms)[1]]]]
    graphics::polypath(lo, la,
                       density = if (is.na(dens)) NULL else dens,
                       col     = if (is.na(dens)) ColFU else NA,
                       border  = "red", lwd = 3)
    if (isTRUE(FUsLab)) {
      i0 <- which(lo == min(lo, na.rm = TRUE))[1]
      if (length(i0) && !is.na(i0)) text(lo[i0] - .10, la[i0] + .10, labels = lab, cex = .8, font = 2, pos = 1, col = 2)
    }
  }
  if (any(!is.na(FU))) {
    if (any(grepl("FU29", FU))) draw_FU("FU29", "FU29")
    if (any(grepl("FU30", FU))) draw_FU("FU30", "FU30")
  }
  
  # --- topónimos ---
  if (places) {
    points(c(-6.299667,-6.950833), c(36.53433,37.25833), pch = 20)
    text(-6.950833, 37.25833, "Huelva", cex=.85, font=2, pos=2)
    text(-6.299667, 36.53433, "Cádiz",  cex=.85, font=2, pos=3)
    text(-8, 37.25, "PORTUGAL", cex=1, font=2, pos=4)
  }
  
  # --- ejes en grados ---
  if (ax) {
    degsX <- seq(-8, -5, ifelse(abs(diff(xlims)) > 1, 1, .5))
    labX  <- sapply(degsX, function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degsX, lab=do.call(expression, labX), font.axis=2, cex.axis=.8, tck=-.01, mgp=c(1,.2,0))
    axis(3, at=degsX, lab=do.call(expression, labX), font.axis=2, cex.axis=.8, tck=-.01, mgp=c(1,.2,0))
    degsY <- seq(35, 38, ifelse(abs(diff(ylims)) > 1, 1, .5))
    labY  <- sapply(degsY, function(y) bquote(.(y)*degree ~ N))
    axis(2, at=degsY, lab=do.call(expression, labY), font.axis=2, cex.axis=.8, tck=-.01, las=2, mgp=c(1,.5,0))
    axis(4, at=degsY, lab=do.call(expression, labY), font.axis=2, cex.axis=.8, tck=-.01, las=2, mgp=c(1,.5,0))
  }
  
  box(lwd = lwdl)
  invisible(NULL)
}