# ============================================================
#  LECTOR BASE — resuelve encoding CP850 y tipos de columna
#  Es el único punto de contacto con foreign::read.dbf
# ============================================================
read_dbf_simple <- function(path, encoding = "CP850") {
  if (!file.exists(path)) stop("No existe el fichero: ", path)
  
  x <- foreign::read.dbf(path, as.is = TRUE)
  
  # ── Nombres en minúsculas ──────────────────────────────────────────────
  names(x) <- tolower(names(x))
  names(x) <- gsub("\\s+",       "_", names(x))
  names(x) <- gsub("[^a-z0-9_]", "",  names(x))
  
  # ── Encoding CP850 → UTF-8 ─────────────────────────────────────────────
  x[] <- lapply(x, function(col) {
    if (is.character(col)) {
      col <- iconv(col, from = "CP850", to = "UTF-8", sub = "")
      col <- iconv(col, from = "UTF-8", to = "UTF-8", sub = "")
      trimws(col)
    } else {
      col
    }
  })
  
  # ── Eliminar filas 100% vacías ─────────────────────────────────────────
  # Tratar por separado char y numérico para evitar NAs en la comparación
  es_vacio <- function(col) {
    if (is.character(col)) is.na(col) | trimws(col) == ""
    else is.na(col)
  }
  mask_vacia <- rowSums(as.data.frame(lapply(x, es_vacio))) == ncol(x)
  if (any(mask_vacia, na.rm = TRUE)) x <- x[!mask_vacia, , drop = FALSE]
  
  as.data.frame(x)
}# ============================================================
#  FIX FECHAS — efecto 2000 en DBF Clipper
#
#  foreign::read.dbf interpreta años de 2 dígitos con la
#  época por defecto de R: años 00-99 → 1900-1999.
#  Para campañas IEO el límite seguro es 1980:
#    - N83 (1983) es la más antigua → ninguna fecha legítima < 1980
#    - Si año < 1980 tras leer → sumar 100 años
#
#  Acepta columnas Date (ya convertidas por foreign) o character.
# ============================================================
fix_dbf_date <- function(x, umbral_anio = 1980L) {
  
  corregir_anios <- function(dates) {
    y <- as.integer(format(dates, "%Y"))
    fix <- !is.na(y) & y < umbral_anio
    if (any(fix)) {
      m  <- as.integer(format(dates[fix], "%m"))
      d  <- as.integer(format(dates[fix], "%d"))
      dates[fix] <- as.Date(
        sprintf("%04d-%02d-%02d", y[fix] + 100L, m, d)
      )
    }
    dates
  }
  
  if (inherits(x, "Date")) {
    return(corregir_anios(x))
  }
  
  if (is.character(x)) {
    # Intentar los dos formatos habituales en Clipper
    d <- suppressWarnings(
      dplyr::coalesce(
        as.Date(x, format = "%Y%m%d"),   # "19900315" o "20120315"
        as.Date(x, format = "%d/%m/%Y"), # "15/03/1990"
        as.Date(x, format = "%d/%m/%y")  # "15/03/90"  ← el problemático
      )
    )
    return(corregir_anios(d))
  }
  
  x  # otros tipos: devolver sin tocar
}