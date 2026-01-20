#' Listado de campañas presentes en el directorio e información sobre ellas
#'
#' Función ROBUSTA de acceso a datos sin ODBC:
#' Alinea correctamente los archivos (CAMP, LANCE, FAUNA, ETC) usando el código de campaña (últimos 3 caracteres)
#' como clave única. Si falta un archivo (ej. no hay HIDRO), deja el hueco con un punto en lugar de descolocar la lista.
#'
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa", Mediterráneo "Medi"
#' @return Un data.frame alineado con la lista de archivos presentes y el año
#' @examples CampsDNS.camp("Cant")
#' @family Series datos
#' @export
CampsDNS.camp64 <- function(dns) {

  # 1. SELECCIÓN DE DIRECTORIO
  if (dns %in% c("Porc", "Pnew", "Porcred")) {
    directorio <- "C:/camp/Porc"
  } else if (dns %in% c("Cant", "Cnew", "Cantred")) {
    directorio <- "C:/camp/Cant"
  } else if (dns %in% c("Arsa", "Arsared")) {
    directorio <- "C:/camp/Arsa"
  } else if (dns == "Medi") {
    directorio <- "C:/camp/Medi"
  } else {
    directorio <- "C:/camp"
  }

  if (!dir.exists(directorio)) stop(paste("No se encuentra el directorio:", directorio))
  message(paste("Listando campañas en:", directorio))

  # 2. OBTENCIÓN DE TODOS LOS FICHEROS
  todos_archivos <- list.files(path = directorio, pattern = "\\.dbf$", ignore.case = TRUE)
  nombres_limpios <- sub("\\.dbf$", "", todos_archivos, ignore.case = TRUE)
  nombres_limpios <- toupper(nombres_limpios) # Todo a mayúsculas

  # Filtramos ficheros basura o temporales, nos quedamos solo con los tipos conocidos
  tipos_validos <- c("CAMP", "LANCE", "FAUNA", "NTALL", "EDAD", "HIDRO")
  patron_regex <- paste(tipos_validos, collapse = "|")
  archivos_utiles <- nombres_limpios[grepl(patron_regex, nombres_limpios) & nchar(nombres_limpios) < 12]

  if(length(archivos_utiles) == 0) {
    message("No se encontraron archivos válidos en el directorio.")
    return(NULL)
  }

  # 3. EXTRACCIÓN DEL IDENTIFICADOR (ID) - La Clave del éxito
  # Asumimos que el ID son SIEMPRE los 3 últimos caracteres (N83, P01, 192...)
  # Esto funciona para CAMPN83 (ID=N83) y para EDADN83 (ID=N83)
  extraer_id <- function(x) {
    substr(x, nchar(x)-2, nchar(x))
  }

  df_base <- data.frame(
    archivo = archivos_utiles,
    id = extraer_id(archivos_utiles),
    tipo = NA,
    stringsAsFactors = FALSE
  )

  # Clasificamos cada archivo por su tipo
  for (t in tipos_validos) {
    df_base$tipo[grepl(t, df_base$archivo)] <- t
  }
  # Limpiamos duplicaciones raras (ej. un archivo que se llame CAMP_LANCE) quedándonos con el primer match
  df_base <- df_base[!is.na(df_base$tipo), ]

  # 4. CREACIÓN DE LA TABLA MAESTRA (Todos los IDs únicos encontrados)
  todos_ids <- unique(df_base$id)
  tabla_final <- data.frame(ID_KEY = todos_ids, stringsAsFactors = FALSE)

  # 5. POBLADO DE COLUMNAS (Un "Left Join" manual)
  for (t in tipos_validos) {
    # Subconjunto de archivos de este tipo
    subset_tipo <- df_base[df_base$tipo == t, c("id", "archivo")]
    names(subset_tipo) <- c("ID_KEY", t) # Renombramos columna 'archivo' al nombre del tipo (ej. 'CAMP')

    # Cruzamos con la tabla maestra
    tabla_final <- merge(tabla_final, subset_tipo, by = "ID_KEY", all.x = TRUE)
  }

  # Reemplazamos NAs con puntos "." para mantener tu estética
  tabla_final[is.na(tabla_final)] <- "."

  # 6. OBTENCIÓN DE AÑOS Y NOMBRE REAL (Desde el fichero LANCE y CAMP)
  # Solo leemos los ficheros que existen
  tabla_final$Year <- 0
  tabla_final$NomCamp <- "."

  for (i in 1:nrow(tabla_final)) {
    # --- Obtener AÑO desde LANCE ---
    fichero_lance <- tabla_final$LANCE[i]
    if (fichero_lance != ".") {
      ruta <- file.path(directorio, paste0(fichero_lance, ".dbf"))
      # Leemos cabecera rápida
      datos_l <- try(foreign::read.dbf(ruta, as.is = TRUE), silent = TRUE)

      if (!inherits(datos_l, "try-error") && nrow(datos_l) > 0 && "FECHA" %in% names(datos_l)) {
        fecha <- datos_l$FECHA[1]
        yr <- try({
          if (inherits(fecha, "Date")) as.numeric(format(fecha, "%Y"))
          else lubridate::year(as.Date(fecha, format = "%Y-%m-%d"))
        }, silent = TRUE)

        if (!inherits(yr, "try-error") && !is.na(yr)) {
          # Tu lógica de siglos
          m <- yr %% 100
          year_final <- ifelse(m > 70, 1900 + m, 2000 + m)
          tabla_final$Year[i] <- year_final
        }
      }
    }

    # --- Obtener NOMBRE desde CAMP (IDENT) ---
    fichero_camp <- tabla_final$CAMP[i]
    if (fichero_camp != ".") {
      # A veces el nombre del archivo no coincide exactamente con el IDENT interno,
      # aunque en tu función original usabas el IDENT interno. Lo sacamos.
      ruta_c <- file.path(directorio, paste0(fichero_camp, ".dbf"))
      datos_c <- try(foreign::read.dbf(ruta_c, as.is = TRUE), silent = TRUE)
      if (!inherits(datos_c, "try-error") && nrow(datos_c) > 0 && "IDENT" %in% names(datos_c)) {
        tabla_final$NomCamp[i] <- as.character(datos_c$IDENT[1])
      } else {
        # Si falla lectura, usamos el ID (los 3 caracteres) como fallback
        tabla_final$NomCamp[i] <- tabla_final$ID_KEY[i]
      }
    }
  }

  # 7. FORMATO FINAL Y ORDEN
  # Seleccionamos y renombramos columnas para coincidir con tu salida original
  salida <- tabla_final[, c("NomCamp", "Year", "CAMP", "LANCE", "FAUNA", "NTALL", "EDAD", "HIDRO")]

  # Convertimos Year a numérico para ordenar correctamente
  salida$Year <- as.numeric(salida$Year)

  # Ordenamos por Año y luego por Nombre
  salida <- salida[order(salida$Year, salida$NomCamp), ]

  # Volvemos a poner puntos en los años que sean 0 o NA (por si acaso)
  salida$Year[salida$Year == 0 | is.na(salida$Year)] <- "."

  print(salida)
  return(invisible(salida))
}
