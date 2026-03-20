# ============================================================
#  FIX fechas efecto 2000 (déjalo tal cual si ya lo tienes arriba)
# ============================================================
fix_dbf_date <- function(x) {
  if (inherits(x, "Date")) {
    y <- as.integer(format(x, "%Y"))
    fix <- !is.na(y) & y < 1980L
    if (any(fix)) {
      y[fix] <- y[fix] + 100L
      m <- as.integer(format(x[fix], "%m"))
      d <- as.integer(format(x[fix], "%d"))
      x[fix] <- as.Date(sprintf("%04d-%02d-%02d", y[fix], m, d))
    }
    return(x)
  }
  if (is.character(x)) {
    suppressWarnings(d <- as.Date(x))
    y <- as.integer(format(d, "%Y"))
    fix <- !is.na(y) & y < 1980L
    if (any(fix)) {
      y[fix] <- y[fix] + 100L
      m <- as.integer(format(d[fix], "%m"))
      dd <- as.integer(format(d[fix], "%d"))
      d[fix] <- as.Date(sprintf("%04d-%02d-%02d", y[fix], m, dd))
    }
    return(d)
  }
  x
}

# ============================================================
#   LECTOR UNIFICADO (con serv→red y verbose opcional)
#   Si options(CampR64.verbose=TRUE), imprime el path usado.
# ============================================================
#' Leer un fichero DBF de CampR64 según zona y origen
#' @export
readCampDBF <- function(tipo,
                        zona = c("cant","porc","arsa","medi"),
                        camp = NULL,
                        dns  = c("local","serv"))
{
  dns  <- tolower(match.arg(dns))
  zona <- tolower(match.arg(zona))
  
  # ---- clave: mapear serv/server/red → 'red', local→'local'
  dns_key <- if (dns %in% c("serv","server","red")) "red" else "local"
  
  if (!exists("CampR64_paths"))
    stop("CampR64_paths no está definido. ¿Se cargó configRoots_user.R?")
  
  verbose <- isTRUE(getOption("CampR64.verbose", FALSE))
  
  # message("DEBUG readCampDBF: tipo=", tipo, 
  #         " zona=", zona, 
  #         " camp=", camp, 
  #         " dns=", dns[1])
  
  # ----- ESPECIES -----
  if (identical(tolower(tipo), "especies")) {
    if (zona %in% c("cant","porc","medi")) {
      base <- CampR64_paths[[dns_key]]$base
      path <- file.path(base, "especies.dbf")
    } else if (zona == "arsa") {
      base <- CampR64_paths[[dns_key]][["arsa"]]
      path <- file.path(base, "especies.dbf")
    } else {
      stop("Zona no soportada para especies: ", zona)
    }
    
    if (verbose) message("readCampDBF[especies]: dns=", dns, " → dns_key=", dns_key,
                         " | zona=", zona, " | path=", path)
    
    if (!file.exists(path))
      stop("No existe especies.dbf en: ", path)
    
    return(read_dbf_simple(path))
  }
  
  # ----- Resto de tipos: <tipo><camp>.dbf -----
  if (is.null(camp))
    stop("Debe indicar 'camp' para tipos distintos de 'especies'.")
  
  fname <- paste0(tolower(tipo), tolower(camp), ".dbf")
  base  <- CampR64_paths[[dns_key]][[zona]]
  
  if (is.null(base))
    stop("No está configurada la ruta para dns='", dns, "' (dns_key='", dns_key,
         "') zona='", zona, "' en CampR64_paths.")
  
  path <- file.path(base, fname)
  
  if (verbose) message("readCampDBF[", tipo, "]: dns=", dns, " → dns_key=", dns_key,
                       " | zona=", zona, " | camp=", camp, " | path=", path)
  
  if (!file.exists(path))
    stop("No existe el fichero: ", path)
  
  df <- read_dbf_simple(path)
  
  # Fix fechas si vinieran Date
  is_date <- vapply(df, inherits, logical(1), what = "Date")
  if (any(is_date)) {
    for (nm in names(df)[is_date]) df[[nm]] <- fix_dbf_date(df[[nm]])
  }
  
  df
}