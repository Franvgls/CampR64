#' StatRec y División ICES (Area) para una campaña — versión estricta sin helpers
#' Requisitos (en lances, todo en minúsculas):
#'   latitud_l, latitud_v, longitud_l, longitud_v, prof_l, prof_v, validez
#'   (y con redux=TRUE: lat, long, prof)
#' Requisitos (en Area): ICESNAME, Area
#' @export
getICESarea64 <- function(camp, zona, dns = "local",
                          incl2 = TRUE, incl0 = FALSE,
                          verbose = FALSE) {
  
  # 0) Lances normalizados; redux=TRUE crea lat/long/prof
  mm <- datlan.camp64(camp = camp, zona = zona, dns = dns,
                      redux = FALSE, incl2 = incl2, incl0 = incl0)
  # Calcular punto medio del lance (equivalente a redux=TRUE)
  mm$lat  <- (mm$latitud_l  + mm$latitud_v)  / 2
  mm$long <- (mm$longitud_l + mm$longitud_v) / 2
  mm$prof <- (mm$prof_l     + mm$prof_v)     / 2
  
  # 1) Requisitos duros (nombres en MINÚSCULAS)
  need_in  <- c("latitud_l","latitud_v","longitud_l","longitud_v","prof_l","prof_v","validez")
  need_out <- c("lat","long","prof")
  miss_in  <- setdiff(need_in,  names(mm))
  miss_out <- setdiff(need_out, names(mm))
  if (length(miss_in) || length(miss_out)) {
    miss_all <- c(miss_in, miss_out)
    stop("getICESarea64: faltan columnas obligatorias: ",
         paste(miss_all, collapse = ", "),
         ". Llama a datlan.camp64(redux=TRUE) y revisa los DBF de lances.")
  }
  
  # 2) Filtrado por validez
  keep_vals <- c(1L, if (incl2) 2L, if (incl0) 0L)
  mm <- mm[mm$validez %in% keep_vals, , drop = FALSE]
  
  # 3) Cargar 'Area' (ICESNAME + Area)
  if (!exists("Area", inherits = TRUE))
    stop("getICESarea64: no encuentro el objeto 'Area' (debe tener columnas ICESNAME y Area).")
  Area <- get("Area", inherits = TRUE)
  req_ar <- c("ICESNAME","Area")
  miss_ar <- setdiff(req_ar, names(Area))
  if (length(miss_ar)) {
    stop("getICESarea64: en 'Area' faltan columnas: ", paste(miss_ar, collapse=", "))
  }
  
  # 4) StatRec por rejilla ICES (1° lat × 0,5° lon) — como Win32
  longlab <- c(paste0("A",0:3),
               unlist(lapply(c("B","C","D","E","F","G","H","J","K","L"),
                             function(L) paste0(L,0:9))),
               paste0("M",0:8))
  brk_lon <- seq(from = -44, to = 69,  by = 1)
  brk_lat <- seq(from =  36, to = 85,  by = 0.5)
  
  ok <- is.finite(mm$long) & is.finite(mm$lat)
  if (!any(ok)) stop("getICESarea64: no hay coordenadas válidas en 'lat/long'.")
  
  rectlong <- rectlat <- rep(NA_character_, nrow(mm))
  rectlong[ok] <- as.character(cut(mm$long[ok], breaks = brk_lon, labels = longlab, include.lowest = TRUE))
  rectlat [ok] <- sprintf("%02d", as.integer(cut(mm$lat[ok],  breaks = brk_lat, labels = FALSE, include.lowest = TRUE)))
  
  statrec <- ifelse(ok, paste0(rectlat, rectlong), NA_character_)
  statrec[is.na(rectlat) | is.na(rectlong)] <- NA_character_
  mm$statrec <- statrec
  
  # 5) División ICES (Area) por match exacto
  idx <- match(mm$statrec, Area$ICESNAME)
  mm$ices_div <- NA_character_
  mm$ices_div[!is.na(idx)] <- as.character(Area$Area[idx[!is.na(idx)]])
  
  # 6) Salida compacta (ajusta 'hora' vs 'hora_l' según tu DBF real)
  out_cols <- c("lance","fecha","hora",
                "camp","sector","sector_geo","estrato_bat","arsect",
                "latitud_l","latitud_v","lat",
                "longitud_l","longitud_v","long",
                "prof_l","prof_v","prof",
                "statrec","ices_div","validez")
  out_cols <- intersect(out_cols, names(mm))  # defensivo si 'hora' se llama 'hora_l'
  mm[, out_cols, drop = FALSE]
}