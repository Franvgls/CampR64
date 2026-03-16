attach_ices_statrec_grid <- function(mm,
                                     area_df = get0("Area", inherits = TRUE),
                                     lat_col = "lat", long_col = "long",
                                     grid_lat = 1.0, grid_lon = 0.5,
                                     fallback_nn = TRUE,
                                     verbose = FALSE) {
  req_area <- c("ICESNAME","stat_x","stat_y")
  if (is.null(area_df) || !all(req_area %in% names(area_df))) {
    if (isTRUE(verbose)) message("Area no disponible o sin columnas: ", paste(req_area, collapse=", "))
    mm$StatRec <- mm$ices_rect <- mm$ices_div <- mm$ices_div27 <- NA_character_; return(mm)
  }
  if (!(lat_col %in% names(mm) && long_col %in% names(mm))) {
    if (isTRUE(verbose)) message("Faltan columnas lat/long: ", lat_col, "/", long_col)
    mm$StatRec <- mm$ices_rect <- mm$ices_div <- mm$ices_div27 <- NA_character_; return(mm)
  }
  
  make_longlab <- function() {
    c(paste0("A",0:3),
      unlist(lapply(c("B","C","D","E","F","G","H","J","K","L"), \(L) paste0(L,0:9))),
      paste0("M",0:8))
  }
  longlab <- make_longlab()
  brk_lon <- seq(from = -44, to = 69,  by = 1)
  brk_lat <- seq(from =  36, to = 85,  by = 0.5)
  
  long <- as.numeric(mm[[long_col]])
  lat  <- as.numeric(mm[[lat_col]])
  ok   <- is.finite(long) & is.finite(lat)
  
  rectlong <- rectlat <- rep(NA_character_, nrow(mm))
  rectlong[ok] <- as.character(cut(long[ok], breaks = brk_lon, labels = longlab, include.lowest = TRUE))
  rectlat [ok] <- sprintf("%02d", as.integer(cut(lat[ok],  breaks = brk_lat, labels = FALSE, include.lowest = TRUE)))
  
  StatRec <- ifelse(ok, paste0(rectlat, rectlong), NA_character_)
  StatRec[is.na(rectlat) | is.na(rectlong)] <- NA_character_
  
  mm$StatRec   <- StatRec
  mm$ices_rect <- StatRec
  
  idx <- match(mm$StatRec, area_df$ICESNAME)
  mm$ices_div   <- if ("Area"    %in% names(area_df)) area_df$Area   [idx] else NA_character_
  mm$ices_div27 <- if ("Area_27" %in% names(area_df)) area_df$Area_27[idx] else NA_character_
  
  if (isTRUE(fallback_nn)) {
    miss <- which(is.na(mm$ices_rect) | is.na(mm$ices_div))
    if (length(miss)) {
      rx <- as.numeric(area_df$stat_x); ry <- as.numeric(area_df$stat_y)
      for (i in miss) {
        if (!is.finite(long[i]) || !is.finite(lat[i])) next
        j <- which.min((long[i] - rx)^2 + (lat[i] - ry)^2)
        mm$ices_rect[i] <- as.character(area_df$ICESNAME[j])
        if ("Area"    %in% names(area_df)) mm$ices_div  [i] <- as.character(area_df$Area   [j])
        if ("Area_27" %in% names(area_df)) mm$ices_div27[i] <- as.character(area_df$Area_27[j])
      }
      if (isTRUE(verbose)) message("ICES por NN (fallback): ", length(miss), " lances")
    }
  }
  mm
}