#' Detalle de tallas por lance (CampR64) con correcciones de tiempo y peso
#' @param grupo,especie,camp,zona,dns Como en el resto del paquete
#' @param sexo TRUE → devuelve columna SEXO (1,2,3); FALSE → agrupa sexos
#' @param excl.sect Excluir sectores (si procede)
#' @param cor.time TRUE → divide por weight.time (histórico)
#' @param verbose Mensajes
#' @return data.frame: 
#'   - sexo=TRUE  → lance, sector, talla, sexo, n
#'   - sexo=FALSE → lance, sector, talla, n
#' @export
dtallan.camp64 <- function(grupo, especie, camp, zona,
                           dns = c("local","serv"),
                           sexo = TRUE,
                           excl.sect = NA,
                           cor.time = TRUE,
                           verbose = FALSE) {
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)
  
  # 1) Base por lance (sector, weight.time)
  abesp <- datos.camp64(grupo = grupo, especie = especie, camp = camp, zona = zona,
                        dns = dns, cor.time = FALSE, kg = FALSE,
                        verbose = verbose, incl2 = FALSE, incl3 = FALSE, incl0 = FALSE, strict = FALSE)
  if (!is.data.frame(abesp) || !nrow(abesp)) {
    return(if (isTRUE(sexo))
      data.frame(lance=integer(0), sector=integer(0), talla=numeric(0), sexo=integer(0), n=numeric(0))
      else
        data.frame(lance=integer(0), sector=integer(0), talla=numeric(0), n=numeric(0)))
  }
  if (!all(is.na(excl.sect))) abesp <- abesp[!(abesp$sector %in% excl.sect), , drop = FALSE]
  if (!nrow(abesp)) {
    return(if (isTRUE(sexo))
      data.frame(lance=integer(0), sector=integer(0), talla=numeric(0), sexo=integer(0), n=numeric(0))
      else
        data.frame(lance=integer(0), sector=integer(0), talla=numeric(0), n=numeric(0)))
  }
  wt_tbl <- abesp[, c("lance","sector","weight.time")]
  names(wt_tbl) <- c("LANCE","SECTOR","WEIGHT.TIME")
  
  # 2) NTALL con corrección por peso
  nt <- readCampDBF("ntall", zona = zona, camp = camp, dns = dns)
  names(nt) <- toupper(trimws(names(nt)))
  req <- c("LANCE","GRUPO","ESP","SEXO","PESO_M","PESO_GR","TALLA","NUMER")
  miss <- setdiff(req, names(nt))
  if (length(miss)) stop("NTALL no contiene: ", paste(miss, collapse = ", "))
  
  esp_num <- suppressWarnings(as.integer(especie))
  nt$ESP_NUM   <- suppressWarnings(as.integer(nt$ESP))
  nt$GRUPO_NUM <- suppressWarnings(as.integer(nt$GRUPO))
  sel <- (!is.na(nt$ESP_NUM) & nt$ESP_NUM == esp_num)
  if (!is.null(grupo)) sel <- sel & (!is.na(nt$GRUPO_NUM) & nt$GRUPO_NUM == suppressWarnings(as.integer(grupo)))
  nt <- nt[sel, c("LANCE","SEXO","PESO_M","PESO_GR","TALLA","NUMER"), drop = FALSE]
  
  if (!nrow(nt)) {
    # sin tallas: fila neutra por coherencia
    nt <- data.frame(LANCE = abesp$lance[1], SEXO = 3, PESO_M = 0.1, PESO_GR = 0,
                     TALLA = 1, NUMER = 0)
  }
  
  nt$PESO_M  <- suppressWarnings(as.numeric(nt$PESO_M))
  nt$PESO_GR <- suppressWarnings(as.numeric(nt$PESO_GR))
  nt$NUMER   <- suppressWarnings(as.numeric(nt$NUMER))
  nt$TALLA   <- suppressWarnings(as.numeric(nt$TALLA))
  nt$SEXO    <- suppressWarnings(as.integer(nt$SEXO))
  
  # Fallback global: si TODO PESO_M es 0/NA, fuerza una fila (legacy)
  if (all(is.na(nt$PESO_M) | nt$PESO_M == 0)) nt$PESO_M[1] <- 0.1
  
  n_corr <- nt$NUMER * (nt$PESO_GR / nt$PESO_M)
  n_corr[!is.finite(n_corr)] <- NA_real_
  
  d <- merge(nt[, c("LANCE","SEXO","TALLA")], wt_tbl, by = "LANCE", all.x = TRUE)
  d$n <- n_corr[match(d$LANCE, nt$LANCE)]
  d$WEIGHT.TIME[is.na(d$WEIGHT.TIME) | d$WEIGHT.TIME <= 0] <- 0.1
  if (isTRUE(cor.time)) d$n <- d$n / d$WEIGHT.TIME
  
  # Filtrado de sectores si procede (doble seguridad)
  if (!all(is.na(excl.sect))) d <- d[!(d$SECTOR %in% excl.sect), , drop = FALSE]
  d <- d[!is.na(d$n), , drop = FALSE]
  
  # Salida
  d$LANCE  <- as.integer(d$LANCE)
  d$SECTOR <- as.integer(d$SECTOR)
  d <- d[order(d$LANCE, d$TALLA, d$SECTOR), ]
  
  if (isTRUE(sexo)) {
    names(d) <- tolower(names(d))
    return(d[, c("lance","sector","talla","sexo","n")])
  } else {
    agg <- aggregate(n ~ LANCE + SECTOR + TALLA, d, sum, na.rm = TRUE)
    names(agg) <- tolower(names(agg))
    agg <- agg[order(agg$lance, agg$talla, agg$sector), ]
    return(agg[, c("lance","sector","talla","n")])
  }
}
