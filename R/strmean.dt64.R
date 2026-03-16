#' @export
strmean.dt64 <- function(x, sector, area, Nas = FALSE) {
  if (length(x) != length(sector) || length(x) != length(area)) {
    stop("x, sector y area deben tener la misma longitud.")
  }
  if (!is.numeric(x)) stop("x debe ser numérico.")
  if (!is.numeric(area)) stop("area debe ser numérico.")
  if (anyNA(sector)) stop("sector contiene NA; no es posible estratificar con NA.")
  
  sector <- as.factor(sector)
  
  # Agregados básicos por sector detallado
  n_loc       <- tapply(x, sector, length)
  mean_loc    <- tapply(x, sector, mean)
  var_loc     <- tapply(x, sector, var)
  area_sector <- tapply(area, sector, mean)
  
  # SE local
  SE_loc <- sqrt(var_loc) / sqrt(n_loc)
  if (Nas) SE_loc[is.na(SE_loc)] <- 0
  
  # CV local
  sd_loc <- sqrt(var_loc)
  CV_loc <- (sd_loc * 100) / mean_loc
  CV_loc[!is.finite(CV_loc)] <- NA_real_
  
  # Varianza ponderada por sector (para agregaciones)
  var_w  <- (var_loc * (area_sector^2)) / n_loc
  
  # Claves estratificadoras
  key_geo <- as.factor(substr(names(area_sector), 1, 1))
  key_bat <- as.factor(substr(names(area_sector), 2, 2))
  
  # ---- Geográfico (1er carácter) ----
  sum_geo  <- tapply(mean_loc * area_sector, key_geo, sum)
  A_geo    <- tapply(area_sector,           key_geo, sum)
  avg_geo  <- sum_geo / A_geo
  SE_geo   <- sqrt(tapply(var_w, key_geo, sum, na.rm = Nas) / (A_geo^2))
  CV_geo   <- (SE_geo * 100) / avg_geo
  CV_geo[!is.finite(CV_geo)] <- NA_real_
  sectores <- rbind(avg_geo, SE_geo, CV_geo)
  rownames(sectores) <- c("media", "SE", "CV")
  
  # ---- Batimétrico (2º carácter) ----
  sum_bat  <- tapply(mean_loc * area_sector, key_bat, sum)
  A_bat    <- tapply(area_sector,           key_bat, sum)
  avg_bat  <- sum_bat / A_bat
  SE_bat   <- sqrt(tapply(var_w, key_bat, sum, na.rm = Nas) / (A_bat^2))
  CV_bat   <- (SE_bat * 100) / avg_bat
  CV_bat[!is.finite(CV_bat)] <- NA_real_
  estratos <- rbind(avg_bat, SE_bat, CV_bat)
  rownames(estratos) <- c("media", "SE", "CV")
  
  # ---- Total ----
  avg_total <- stats::weighted.mean(mean_loc, area_sector)
  SE_total  <- sqrt(sum(var_w, na.rm = Nas) / (sum(area_sector)^2))
  CV_total  <- (SE_total * 100) / avg_total
  if (!is.finite(CV_total)) CV_total <- NA_real_
  total <- rbind(avg_total, SE_total, CV_total)
  rownames(total) <- c("media", "SE", "CV")
  colnames(total) <- "total"
  
  locales <- rbind(mean_loc, SE_loc, CV_loc)
  rownames(locales) <- c("media", "SE", "CV")
  
  list(locales = locales, estratos = estratos, sectores = sectores, total = total)
}