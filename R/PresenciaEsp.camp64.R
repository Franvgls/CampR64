#' Presencia de una especie en las campañas de una zona
#'
#' Función de acceso a datos que escanea todos los ficheros FAUNA*.dbf y NTALL*.dbf
#' del directorio de la zona seleccionada y devuelve las campañas en las que aparece
#' la especie, distinguiendo entre presencia (captura) y presencia con datos de talla.
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos,
#'   5 invertebrados, 6 desechos y otros
#' @param esp Código de la especie numérico o carácter con tres espacios. Solo admite una especie
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant",
#'   Golfo de Cádiz "arsa"
#' @param dns elige si se trabaja con archivos del ordenador ("local") o del servidor ("serv")
#' @param verbose Si TRUE emite warnings sobre ficheros vacíos o ilegibles
#' @return Lista invisible con tres elementos:
#'   \itemize{
#'     \item \code{especie}: nombre científico de la especie
#'     \item \code{capturas}: vector de campañas ordenadas cronológicamente donde
#'       la especie aparece con captura (peso o número > 0)
#'     \item \code{tallas}: vector de campañas ordenadas cronológicamente donde
#'       la especie tiene datos de talla
#'   }
#'   Además imprime por consola un resumen de campañas.
#' @examples 
#' \dontrun{
#' res <- PresenciaEsp.camp64(1, 220, "porc", "local")
#' res$capturas
#' res$tallas
#' }
#' @export
PresenciaEsp.camp64 <- function(gr, esp, zona="cant", dns=c("local","serv"),
                                verbose=FALSE) {
  if (length(esp) > 1 || any(esp == "999") || any(gr == "9"))
    stop("Seleccionadas más de una especie o comodines. Función para una sola especie.")
  
  dns  <- match.arg(dns)
  zona <- tolower(zona)
  dns_key <- if (dns == "serv") "serv" else "local"
  
  # --- Directorio de la zona ---
  base_dir <- CampR64_paths[[dns_key]][[zona]]
  if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
    stop("Directorio no encontrado para zona='", zona,
         "' dns='", dns, "': ", base_dir)
  }
  
  # --- Listar FAUNAs y NTALLs ---
  fauna_files <- list.files(base_dir, pattern="^FAUNA.*\\.DBF$",
                            ignore.case=TRUE, full.names=FALSE)
  ntall_files <- list.files(base_dir, pattern="^NTALL.*\\.DBF$",
                            ignore.case=TRUE, full.names=FALSE)
  
  if (!length(fauna_files))
    stop("No se encontraron ficheros FAUNA*.dbf en: ", base_dir)
  
  # Códigos de campaña (parte entre FAUNA/NTALL y .DBF)
  camps_f <- toupper(sub("^FAUNA(.*)\\.DBF$", "\\1", fauna_files, ignore.case=TRUE))
  camps_t <- toupper(sub("^NTALL(.*)\\.DBF$", "\\1", ntall_files, ignore.case=TRUE))
  
  gr_num  <- suppressWarnings(as.integer(gr))
  esp_num <- suppressWarnings(as.integer(esp))
  
  # --- Escanear FAUNAs (presencia = peso o número > 0) ---
  camps_captura <- character(0)
  for (i in seq_along(fauna_files)) {
    ruta <- file.path(base_dir, fauna_files[i])
    df <- tryCatch(foreign::read.dbf(ruta, as.is=TRUE),
                   error=function(e) NULL)
    if (is.null(df) || !nrow(df)) {
      if (verbose) warning("Fichero vacío o ilegible: ", fauna_files[i])
      next
    }
    names(df) <- toupper(trimws(names(df)))
    if (!all(c("GRUPO","ESP") %in% names(df))) {
      if (verbose) warning("Faltan columnas GRUPO/ESP en: ", fauna_files[i])
      next
    }
    df$GRUPO <- suppressWarnings(as.integer(df$GRUPO))
    df$ESP   <- suppressWarnings(as.integer(df$ESP))
    peso     <- if ("PESO_GR" %in% names(df)) suppressWarnings(as.numeric(df$PESO_GR)) else rep(0, nrow(df))
    numero   <- if ("NUMERO"  %in% names(df)) suppressWarnings(as.numeric(df$NUMERO))  else rep(0, nrow(df))
    peso[is.na(peso)]     <- 0
    numero[is.na(numero)] <- 0
    
    hit <- !is.na(df$GRUPO) & !is.na(df$ESP) &
      df$GRUPO == gr_num & df$ESP == esp_num &
      (peso > 0 | numero > 0)
    if (any(hit)) camps_captura <- c(camps_captura, camps_f[i])
  }
  
  # --- Escanear NTALLs (presencia de datos de talla) ---
  camps_tallas <- character(0)
  for (i in seq_along(ntall_files)) {
    ruta <- file.path(base_dir, ntall_files[i])
    df <- tryCatch(foreign::read.dbf(ruta, as.is=TRUE),
                   error=function(e) NULL)
    if (is.null(df) || !nrow(df)) {
      if (verbose) warning("Fichero vacío o ilegible: ", ntall_files[i])
      next
    }
    names(df) <- toupper(trimws(names(df)))
    if (!all(c("GRUPO","ESP") %in% names(df))) next
    df$GRUPO <- suppressWarnings(as.integer(df$GRUPO))
    df$ESP   <- suppressWarnings(as.integer(df$ESP))
    hit <- !is.na(df$GRUPO) & !is.na(df$ESP) &
      df$GRUPO == gr_num & df$ESP == esp_num
    if (any(hit)) camps_tallas <- c(camps_tallas, camps_t[i])
  }
  
  # --- Nombre de la especie ---
  nombre_esp <- tryCatch(
    buscaesp64(gr, esp, zona=zona, dns=dns),
    error=function(e) paste0("gr=", gr, ", esp=", esp)
  )
  
  # --- Orden cronológico por año de campaña ---
  ordenar_por_year <- function(x) {
    if (!length(x)) return(x)
    yrs <- tryCatch(as.numeric(camptoyear(x)),
                    error=function(e) rep(NA_real_, length(x)))
    if (all(is.na(yrs))) return(sort(x))
    x[order(yrs, x)]
  }
  camps_captura <- ordenar_por_year(camps_captura)
  camps_tallas  <- ordenar_por_year(camps_tallas)
  
  # --- Salida por consola ---
  if (length(camps_captura)) {
    message("Especie '", nombre_esp, "' presente en ",
            length(camps_captura), " campañas:")
    message("  ", paste(camps_captura, collapse=", "))
    if (length(camps_tallas)) {
      message("Con datos de tallas en ", length(camps_tallas), " campañas:")
      message("  ", paste(camps_tallas, collapse=", "))
    } else {
      message("No hay información de tallas para '", nombre_esp, "'")
    }
  } else {
    message("No hay capturas de '", nombre_esp, "' en ninguna campaña de zona '",
            zona, "'")
  }
  
  invisible(list(
    especie  = nombre_esp,
    capturas = camps_captura,
    tallas   = camps_tallas
  ))
}