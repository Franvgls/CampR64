#' Listado de campañas presentes en el directorio e información sobre ellas
#'
#' Función ROBUSTA de acceso a datos sin ODBC:
#' lista todos los DBF presentes en el directorio de la zona/dns indicada y
#' los alinea por código de campaña (últimos 3 caracteres) como clave única.
#' Si falta un archivo (ej. no hay HIDRO), deja el hueco con un punto en
#' lugar de descolocar la lista. Adicionalmente extrae el año desde el
#' archivo LANCE y el nombre interno de la campaña desde CAMP.
#'
#' @param zona Zona de trabajo: \code{"cant"}, \code{"porc"}, \code{"arsa"}
#'   o \code{"medi"}.
#' @param dns Origen de los datos: \code{"local"} (disco local) o
#'   \code{"serv"} (red). Las rutas se resuelven a través de
#'   \code{CampR64_paths}, configurado en
#'   \code{~/.CampR64/configRoots_user.R}.
#' @return Un \code{data.frame} (invisible) con una fila por campaña y
#'   columnas \code{NomCamp}, \code{Year}, \code{CAMP}, \code{LANCE},
#'   \code{FAUNA}, \code{NTALL}, \code{EDAD}, \code{HIDRO}. Se imprime por
#'   consola.
#' @examples
#' \dontrun{
#' CampsDNS.camp64("cant", "local")
#' CampsDNS.camp64("porc", "serv")
#' CampsDNS.camp64("arsa", "local")
#' }
#' @family Series datos
#' @export
CampsDNS.camp64 <- function(zona, dns = c("local", "serv")) {
  
  dns  <- match.arg(dns)
  zona <- tolower(zona)
  
  # 1. RESOLUCIÓN DEL DIRECTORIO via CampR64_paths
  if (!exists("CampR64_paths", envir = .GlobalEnv)) {
    stop("CampR64_paths no está definido. ¿Se cargó configRoots_user.R?")
  }
  paths_dns <- get("CampR64_paths", envir = .GlobalEnv)[[dns]]
  if (is.null(paths_dns)) {
    stop(sprintf("dns '%s' no está definido en CampR64_paths. Disponibles: %s",
                 dns,
                 paste(names(get("CampR64_paths", envir = .GlobalEnv)),
                       collapse = ", ")))
  }
  directorio <- paths_dns[[zona]]
  if (is.null(directorio) || !nzchar(directorio)) {
    stop(sprintf("Zona '%s' no está definida en CampR64_paths[['%s']]. Disponibles: %s",
                 zona, dns, paste(names(paths_dns), collapse = ", ")))
  }
  if (!dir.exists(directorio))
    stop(sprintf("No se encuentra el directorio: %s", directorio))
  
  message(sprintf("Listando campañas en: %s  (zona = %s, dns = %s)",
                  directorio, zona, dns))
  
  # 2. OBTENCIÓN DE TODOS LOS FICHEROS DBF
  todos_archivos <- list.files(path = directorio, pattern = "\\.dbf$",
                               ignore.case = TRUE)
  if (length(todos_archivos) == 0) {
    message("No se encontraron archivos DBF en el directorio.")
    return(invisible(NULL))
  }
  nombres_limpios <- toupper(sub("\\.dbf$", "", todos_archivos,
                                 ignore.case = TRUE))
  
  # 3. CLASIFICACIÓN POR TIPO
  tipos_validos <- c("CAMP", "LANCE", "FAUNA", "NTALL", "EDAD", "HIDRO")
  patron_regex  <- paste(tipos_validos, collapse = "|")
  archivos_utiles <- nombres_limpios[grepl(patron_regex, nombres_limpios) &
                                       nchar(nombres_limpios) < 12]
  if (length(archivos_utiles) == 0) {
    message("No se encontraron archivos válidos en el directorio.")
    return(invisible(NULL))
  }
  
  # ID = últimos 3 caracteres (N83, P01, 192…)
  extraer_id <- function(x) substr(x, nchar(x) - 2, nchar(x))
  
  df_base <- data.frame(
    archivo = archivos_utiles,
    id      = extraer_id(archivos_utiles),
    tipo    = NA_character_,
    stringsAsFactors = FALSE
  )
  for (t in tipos_validos) df_base$tipo[grepl(t, df_base$archivo)] <- t
  df_base <- df_base[!is.na(df_base$tipo), ]
  
  # 4. PIVOT: una fila por ID, una columna por tipo
  todos_ids   <- unique(df_base$id)
  tabla_final <- data.frame(ID_KEY = todos_ids, stringsAsFactors = FALSE)
  for (t in tipos_validos) {
    sub_t <- df_base[df_base$tipo == t, c("id", "archivo")]
    names(sub_t) <- c("ID_KEY", t)
    tabla_final  <- merge(tabla_final, sub_t, by = "ID_KEY", all.x = TRUE)
  }
  tabla_final[is.na(tabla_final)] <- "."
  
  # 5. AÑO (desde LANCE) y NomCamp (desde CAMP)
  tabla_final$Year    <- 0
  tabla_final$NomCamp <- "."
  
  for (i in seq_len(nrow(tabla_final))) {
    fichero_lance <- tabla_final$LANCE[i]
    if (fichero_lance != ".") {
      ruta <- file.path(directorio, paste0(fichero_lance, ".dbf"))
      datos_l <- try(foreign::read.dbf(ruta, as.is = TRUE), silent = TRUE)
      if (!inherits(datos_l, "try-error") && nrow(datos_l) > 0 &&
          "FECHA" %in% names(datos_l)) {
        fecha <- datos_l$FECHA[1]
        yr <- try({
          if (inherits(fecha, "Date")) as.numeric(format(fecha, "%Y"))
          else lubridate::year(as.Date(fecha, format = "%Y-%m-%d"))
        }, silent = TRUE)
        if (!inherits(yr, "try-error") && !is.na(yr)) {
          m <- yr %% 100
          tabla_final$Year[i] <- ifelse(m > 70, 1900 + m, 2000 + m)
        }
      }
    }
    
    fichero_camp <- tabla_final$CAMP[i]
    if (fichero_camp != ".") {
      ruta_c <- file.path(directorio, paste0(fichero_camp, ".dbf"))
      datos_c <- try(foreign::read.dbf(ruta_c, as.is = TRUE), silent = TRUE)
      if (!inherits(datos_c, "try-error") && nrow(datos_c) > 0 &&
          "IDENT" %in% names(datos_c)) {
        tabla_final$NomCamp[i] <- as.character(datos_c$IDENT[1])
      } else {
        tabla_final$NomCamp[i] <- tabla_final$ID_KEY[i]
      }
    }
  }
  
  # 6. SALIDA ORDENADA
  salida <- tabla_final[, c("NomCamp", "Year", "CAMP", "LANCE",
                            "FAUNA", "NTALL", "EDAD", "HIDRO")]
  salida$Year <- as.numeric(salida$Year)
  salida <- salida[order(salida$Year, salida$NomCamp), ]
  salida$Year[salida$Year == 0 | is.na(salida$Year)] <- "."
  rownames(salida) <- NULL
  
  print(salida)
  invisible(salida)
}