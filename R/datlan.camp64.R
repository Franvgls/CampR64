#' Características del lance (versión DBF, sin ODBC)
#'
#' Lee los ficheros LANCExx.DBF y CAMPxx.DBF de una o varias campañas,
#' calcula coordenadas medias, profundidad media, zona ICES, tiempos
#' de arrastre estandarizados y otros parámetros asociados al lance.
#'
#' Versión equivalente a datlan.camp() pero usando lectura directa de DBF.
#'
#' @export
datlan.camp64 <- function(camp, dns,
                          incl2 = TRUE, incl0 = FALSE,
                          excl.sect = NA, redux = FALSE,
                          year = TRUE, quarter = TRUE, bio = FALSE) {
  
  # ------------------------------------------------------------------
  # Función interna para procesar UNA campaña
  # ------------------------------------------------------------------
  foop <- function(camp, dns, incl2 = incl2, incl0 = incl0) {
    
    if (length(camp) > 1)
      stop("seleccionadas más de una campaña")
    
    ## -------------------------
    ## LECTURA LANCE
    ## -------------------------
    fichero_lan <- paste0("LANCE", camp, ".DBF")
    path_lan <- get_camp_file(fichero_lan, dns)
    if (!file.exists(path_lan)) stop("No encuentro: ", path_lan)
    
    lan <- leer_dbf(path_lan)
    names(lan) <- tolower(names(lan))
    if ("observ" %in% names(lan)) lan$observ <- NULL
    
    # Normalizar LANCE como entero
    lan$lance <- suppressWarnings(as.integer(trimws(as.character(lan$lance))))
    lan <- lan[order(lan$lance), ]
    
    # Normalizar ESTN (si existe)
    if ("estn" %in% names(lan)) {
      lan$estn <- suppressWarnings(as.integer(trimws(as.character(lan$estn))))
    }
    
    # Otros tipos críticos
    if ("validez" %in% names(lan)) lan$validez <- suppressWarnings(as.integer(lan$validez))
    # Normalizar sector EXACTAMENTE como CampR clásico
    if ("sector" %in% names(lan)) {
      lan$sector <- as.character(lan$sector)
      lan$sector <- trimws(lan$sector, which = "both")
    }
    if ("estrato" %in% names(lan)) lan$estrato <- as.character(lan$estrato)
    
    ## -------------------------
    ## LECTURA CAMP
    ## -------------------------
    fichero_camp <- paste0("CAMP", camp, ".DBF")
    # Ojo: algunos dominios usan CAMPNxx.DBF, CAMPPxx.DBF, etc.
    if (!file.exists(get_camp_file(fichero_camp, dns))) {
      # Intento con prefijo de dominio en el nombre, p.ej. CAMPN24.DBF
      fichero_camp <- paste0("CAMP", substr(dns, 1, 1), camp, ".DBF")
    }
    path_camp <- get_camp_file(fichero_camp, dns)
    if (!file.exists(path_camp)) stop("No encuentro: ", path_camp)
    
    dumb <- leer_dbf(path_camp)
    names(dumb) <- tolower(names(dumb))
    
    ## -------------------------
    ## ARSECT (áreas por sector y estrato)
    ## -------------------------
    
    # Detectar columnas de área por nombre (C1A–C5E)
    cols_area <- grep("^c[1-5][a-e]$", names(dumb), value = TRUE)
    
    if (length(cols_area) == 0) {
      stop("No se han encontrado columnas de área tipo C1A–C5E en CAMPxx.DBF")
    }
    
    # Conversión robusta y normalización de ARSECT
    mat <- dumb[, cols_area, drop = FALSE]
    
    # Forzar matriz consistente y obtener dimensiones fiables
    mat_m <- as.matrix(mat)
    n_r <- nrow(mat_m)
    n_c <- ncol(mat_m)
    if (is.null(n_c) || n_c == 0) stop("No se han encontrado columnas de área (cols_area vacío).")
    
    # Vectorizar y limpiar valores
    mat_vec <- as.character(as.vector(mat_m))
    mat_vec <- trimws(mat_vec)
    mat_vec[mat_vec == ""] <- NA
    mat_vec <- gsub(",", ".", mat_vec)
    mat_vec <- gsub("[^0-9eE\\-\\.+NA]", "", mat_vec)
    
    # Reconstruir matriz numérica con dimensiones fiables
    mat_num_vec <- suppressWarnings(as.numeric(mat_vec))
    mat_num <- matrix(mat_num_vec, nrow = n_r, ncol = n_c, byrow = FALSE)
    
    # Mapear nombres de columna a sector y estrato
    sectores <- substr(cols_area, 2, 2)
    estr_letras <- substr(cols_area, 3, 3)
    map_letra_a_num <- setNames(as.character(1:5), letters[1:5])
    estratos_num <- map_letra_a_num[estr_letras]
    
    area <- data.frame(
      sector  = rep(sectores, each = n_r),
      estrato = rep(estratos_num, each = n_r),
      arsect  = as.vector(mat_num),
      stringsAsFactors = FALSE
    )
    
    # eliminar filas sin área
    area <- area[!is.na(area$arsect), ]
    
    ## -------------------------
    ## LÓGICA ORIGINAL
    ## -------------------------
    lan$haul.mins <- dumb$durlan
    lan$barco     <- dumb$barco
    
    # sector y estrato se mantienen separados para el join
    lan$sector  <- as.character(lan$sector)
    lan$estrato <- as.character(lan$estrato)
    
    # Coordenadas decimales
    lan$latitud_l  <- round(sapply(lan$latitud_l, gradec) * ifelse(lan$nsl == "N", 1, -1), 4)
    lan$longitud_l <- round(sapply(lan$longitud_l, gradec) * ifelse(lan$ewl == "E", 1, -1), 4)
    lan$latitud_v  <- round(sapply(lan$latitud_v, gradec) * ifelse(lan$nsv == "N", 1, -1), 4)
    lan$longitud_v <- round(sapply(lan$longitud_v, gradec) * ifelse(lan$ewv == "E", 1, -1), 4)
    
    lan$lat  <- round((lan$latitud_l + lan$latitud_v) / 2, 4)
    lan$long <- round((lan$longitud_l + lan$longitud_v) / 2, 4)
    lan$prof <- (lan$prof_l + lan$prof_v) / 2
    
    # Zona ICES
    lan$zona <- NA
    for (i in seq_len(nrow(lan))) {
      if (lan$lat[i] > 48   & lan$lat[i] < 52.5 & lan$long[i] > -18 & lan$long[i] < -12) lan$zona[i] <- "7k"
      if (lan$lat[i] > 52.5 & lan$lat[i] < 54.5 & lan$long[i] > -18 & lan$long[i] < -12) lan$zona[i] <- "7c"
      if (lan$lat[i] > 52.5 & lan$lat[i] < 54.5 & lan$long[i] > -12) lan$zona[i] <- "7b"
      if (lan$lat[i] > 43   & lan$lat[i] < 44.5 & lan$long[i] > -2)  lan$zona[i] <- "8b"
      if (lan$lat[i] > 44.5 & lan$lat[i] < 46   & lan$long[i] > -4)  lan$zona[i] <- "8b"
      if (lan$lat[i] > 43   & lan$lat[i] < 44.5 & lan$long[i] > -11 & lan$long[i] < -2) lan$zona[i] <- "8c"
      if (lan$lat[i] > 35.95 & lan$lat[i] < 43 & lan$long[i] > -11 & lan$long[i] < -8.75) lan$zona[i] <- "9a"
      if (lan$lat[i] > 35.95 & lan$lat[i] < 37.75 & lan$long[i] > -7.5 & lan$long[i] < -5.50) lan$zona[i] <- "9a"
      if (dns == "Medi" & lan$lat[i] > 35.8 & lan$long[i] > -5.6556) lan$zona[i] <- "wm.37.1"
    }
    
    # Limpieza de ceros
    lan$dista_p[lan$dista_p == 0] <- NA
    lan$abert_v[lan$abert_v == 0] <- NA
    lan$abert_h[lan$abert_h == 0] <- NA
    lan$sali[lan$sali == 0]       <- NA
    lan$temp[lan$temp == 0]       <- NA
    
    # Fecha
    lan$fecha <- as.Date(ifelse(lan$fecha < "1980-12-31",
                                format(lan$fecha, "20%y-%m-%d"),
                                format(lan$fecha)))
    
    # Año y trimestre
    lan$year <- as.integer(format(lan$fecha, "%Y"))
    lan$quarter <- as.integer((as.integer(format(lan$fecha, "%m")) - 1) %/% 3 + 1)
    
    # Weight.time
    lan$weight.time <- ifelse(lan$haul.mins == 60, 1, 2) *
      ((trunc(lan$hora_v) + ((lan$hora_v - trunc(lan$hora_v)) / .6)) -
         (trunc(lan$hora_l) + ((lan$hora_l - trunc(lan$hora_l)) / .6)))
    
    lan$weight.time <- round(lan$weight.time, 3)
    
    # Filtros
    if (!incl0) lan <- lan[lan$validez != 0, ]
    if (!incl2) lan <- lan[as.numeric(lan$validez) <= 1, ]
    
    # Normalizar estrato en lan para que coincida con area (A..E -> 1..5)
    lan$estrato <- trimws(as.character(lan$estrato))
    lan$estrato_up <- toupper(lan$estrato)
    
    map_letra_a_num <- c(A = "1", B = "2", C = "3", D = "4", E = "5")
    
    # Si es letra A-E la mapeamos; si ya es numérico lo dejamos; NA se mantiene
    lan$estrato <- ifelse(
      is.na(lan$estrato_up),
      NA_character_,
      ifelse(lan$estrato_up %in% names(map_letra_a_num),
             map_letra_a_num[lan$estrato_up],
             lan$estrato_up)
    )
    
    # Asegurar tipo character y eliminar columna temporal
    lan$estrato <- as.character(lan$estrato)
    lan$estrato_up <- NULL
    
    # --- Normalizar sector y estrato para el join ---
    lan$sector <- trimws(as.character(lan$sector))
    lan$estrato <- trimws(as.character(lan$estrato))
    
    map_letra_a_num <- c(A = "1", B = "2", C = "3", D = "4", E = "5")
    estr_up <- toupper(lan$estrato)
    
    lan$estrato <- ifelse(
      is.na(estr_up),
      NA_character_,
      ifelse(estr_up %in% names(map_letra_a_num),
             map_letra_a_num[estr_up],
             estr_up)
    )
    
    lan$estrato <- as.character(lan$estrato)
    # --- fin normalización ---
    # Normalizar sector y estrato para el join
    lan$sector <- trimws(as.character(lan$sector))
    lan$estrato <- trimws(as.character(lan$estrato))
    
    map_letra_a_num <- c(A = "1", B = "2", C = "3", D = "4", E = "5")
    estr_up <- toupper(lan$estrato)
    
    lan$estrato <- ifelse(
      is.na(estr_up),
      NA_character_,
      ifelse(estr_up %in% names(map_letra_a_num),
             map_letra_a_num[estr_up],
             estr_up)
    )
    
    lan$estrato <- as.character(lan$estrato)
    # --- Forzar tipos y limpiar claves antes del join ---
    # Asegurar que sector y estrato son character en ambos data.frames
    lan$sector  <- as.character(trimws(lan$sector))
    lan$estrato <- as.character(trimws(lan$estrato))
    
    area$sector  <- as.character(trimws(area$sector))
    area$estrato <- as.character(trimws(area$estrato))
    
    # Normalizar posibles prefijos/ceros en sector (ejemplos comunes)
    # Descomenta o adapta si tus sectores vienen como "01" o "N1"
    # area$sector <- sub("^0+", "", area$sector)
    # lan$sector  <- sub("^0+", "", lan$sector)
    # lan$sector  <- sub("^N", "", lan$sector)
    
    # Diagnóstico: contar coincidencias antes del left_join
    matches_before <- nrow(dplyr::inner_join(lan[, c("sector","estrato")], area[, c("sector","estrato")], by = c("sector","estrato")))
    message(sprintf("DEBUG: áreas disponibles = %d; lances = %d; coincidencias exactas sector+estrato = %d",
                    nrow(area), nrow(lan), matches_before))
    # --- fin forzado/diagnóstico ---
    
        
    # Join con ARSECT (sector + estrato)
    datos <- dplyr::left_join(lan, area, by = c("sector", "estrato"))
    
    datos
  }
  
  # ------------------------------------------------------------------
  # Procesar todas las campañas
  # ------------------------------------------------------------------
  datos <- data.frame(camp = camp[1],
                      foop(camp[1], dns = dns, incl2 = incl2, incl0 = incl0))
  
  if (length(camp) > 1) {
    for (i in camp[2:length(camp)]) {
      datos <- dplyr::bind_rows(
        datos,
        data.frame(foop(i, dns = dns, incl2 = incl2, incl0 = incl0), camp = i)
      )
    }
  }
  
  # ------------------------------------------------------------------
  # Filtros globales
  # ------------------------------------------------------------------
  if (any(!is.na(excl.sect))) {
    datos$sector <- gsub("NA", "N", datos$sector)
    for (i in seq_along(excl.sect)) {
      idx <- grep(excl.sect[i], as.character(datos$sector))
      if (length(idx) > 0) datos <- datos[-idx, ]
    }
  }
  
  # Reducido
  if (redux) {
    datos <- dplyr::select(datos, -c("longitud_v", "longitud_l",
                                     "latitud_v", "latitud_l",
                                     "prof_v", "prof_l"))
    datos <- dplyr::relocate(datos, c("camp", "lance", "validez", "lat", "long", "prof"))
  }
  
  # Eliminar year/quarter si se pide
  if (!year) datos$year <- NULL
  if (!quarter) datos$quarter <- NULL
  
  # Bio
  if (bio) {
    datos <- datos[, c("camp", "lance", "sector", "validez",
                       "lat", "long", "prof", "estrato", "fecha", "zona")]
  }
  
  datos
}
