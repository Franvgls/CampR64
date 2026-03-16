#' Estadísticos estratificados completos (media, SE, CV) para varias campañas
#'
#' Calcula media, error estándar (SE) y coeficiente de variación (CV)
#' estratificados por campaña, tanto para peso como para número,
#' usando \code{datos.camp64} y \code{str.mean.dt64}.
#'
#' @param gr Grupo de la especie.
#' @param esp Código de la especie.
#' @param camps Vector de campañas (ej. "N23", "P22", "121", "221").
#' @param zona Origen biogeográfico ("Cant", "Porc", "Arsa", "Pnew").
#' @param dns Fuente de datos ("local" o "serv").
#' @param cor.time Si TRUE corrige por duración del lance.
#' @param kg Si TRUE devuelve peso en kilos.
#' @param dec Número de decimales para redondeo.
#' @param excl.sect Sectores a excluir (patrones grep).
#' @param Nas Si TRUE los estratos con 1 lance usan SE=0.
#' @param verbose Mensajes informativos.
#'
#' @return Un data.frame con columnas:
#'   camp, mean_w, se_w, cv_w, mean_n, se_n, cv_n
#'
#' @export
CV.camps64_full <- function(gr, esp, camps, zona,
                            dns = "local",
                            cor.time = TRUE,
                            kg = TRUE,
                            dec = 2,
                            excl.sect = NA,
                            Nas = FALSE,
                            verbose = TRUE) {
  
  esp <- format(esp, width = 3, justify = "r")
  camps <- as.character(camps)
  
  # Contenedor de resultados
  out <- data.frame(
    camp  = camps,
    mean_w = NA_real_,
    se_w   = NA_real_,
    cv_w   = NA_real_,
    mean_n = NA_real_,
    se_n   = NA_real_,
    cv_n   = NA_real_,
    stringsAsFactors = FALSE
  )
  
  # Bucle por campaña
  for (i in seq_along(camps)) {
    
    cp <- camps[i]
    
    # --- Obtener datos de la campaña ---
    mm <- datos.camp64(
      gr    = gr,
      esp   = esp,
      camp  = cp,
      zona  = zona,
      dns   = dns,
      kg    = kg,
      cor.time = cor.time,
      verbose = verbose
    )
    
    # --- Exclusión de sectores ---
    if (any(!is.na(excl.sect))) {
      for (pat in excl.sect) {
        mm <- mm[-grep(pat, as.character(mm$sector)), ]
      }
      mm$sector <- factor(as.character(mm$sector))
    }
    
    # --- Peso ---
    res_w <- str.mean.dt(
      x      = mm$peso,
      sector = mm$sector,
      area   = mm$arsect,
      Nas    = Nas
    )$total
    
    # --- Número ---
    res_n <- str.mean.dt(
      x      = mm$numero,
      sector = mm$sector,
      area   = mm$arsect,
      Nas    = Nas
    )$total
    
    # --- Guardar en tabla ---
    out$mean_w[i] <- round(res_w["media", ], dec)
    out$se_w[i]   <- round(res_w["SE", ], dec)
    out$cv_w[i]   <- round(res_w["CV", ], dec)
    
    out$mean_n[i] <- round(res_n["media", ], dec)
    out$se_n[i]   <- round(res_n["SE", ], dec)
    out$cv_n[i]   <- round(res_n["CV", ], dec)
  }
  
  return(out)
}