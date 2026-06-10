#' Datos de claves talla-edad en campañas (CampR64)
#'
#' Función de acceso a datos. Localiza los ficheros EDAD*.dbf presentes en el
#' directorio que corresponde a la zona/origen y resume el número de individuos
#' con datos de edad por campaña y especie. Sustituye al acceso vía RODBC de la
#' versión de 32 bits (ALKs.dns.camp) por lectura directa de DBF con el lector
#' unificado read_dbf_simple().
#' @param zona Zona/origen de los datos: Cantábrico "cant", Porcupine "porc",
#'   Golfo de Cádiz "arsa", Mediterráneo "medi"
#' @param camp Filtro de campañas: prefijo que sigue a "EDAD" en el nombre del
#'   fichero (p.ej. "P" Porcupine, "N" demersales, "1"/"2" Arsa, "M" Medits...).
#'   Si es "" (por defecto) toma todos los ficheros EDAD* del directorio
#' @param dns Origen de las rutas: "local" o "serv"
#' @return Tabla (especies x campañas) con el nº de individuos con datos de edad
#' @examples
#' \dontrun{
#' ALKs64.dns.camp("porc", "P")
#' ALKs64.dns.camp("cant", "N")
#' }
#' @family edades
#' @seealso \link{readCampDBF}, \link{buscaesp64}
#' @export
ALKs64.dns.camp <- function(zona = c("cant", "porc", "arsa", "medi"),
                            camp = "",
                            dns  = c("local", "serv")) {

  zona <- tolower(match.arg(zona))
  dns  <- tolower(match.arg(dns))
  dns_key <- if (dns == "serv") "serv" else "local"

  # ---- resolución de carpeta, igual que readCampDBF ----
  if (!exists("CampR64_paths", envir = .GlobalEnv))
    stop("CampR64_paths no está definido. ¿Se cargó configRoots_user.R?")
  CampR64_paths <- get("CampR64_paths", envir = .GlobalEnv)

  base <- CampR64_paths[[dns_key]][[zona]]
  if (is.null(base) || !nzchar(base))
    stop("Ruta no configurada para dns='", dns, "' (dns_key='", dns_key,
         "') zona='", zona, "' en CampR64_paths. ¿Ejecutaste configurarCampR64()?")

  # ---- localizar ficheros EDAD*.dbf (sustituye al catálogo ODBC) ----
  ficheros <- list.files(base, pattern = "^EDAD.*\\.dbf$", ignore.case = TRUE)
  if (length(ficheros) == 0)
    stop("No hay claves talla-edad (EDAD*.dbf) en ", base)

  # código de campaña = nombre del fichero sin extensión (EDAD + identificador)
  camps.ed <- sub("\\.dbf$", "", ficheros, ignore.case = TRUE)

  # filtro por 'camp' (mismo criterio que la versión RODBC)
  pref     <- paste0("EDAD", camp)
  camps.ed <- camps.ed[substr(camps.ed, 1, nchar(pref)) == pref]
  if (length(camps.ed) == 0)
    stop("No hay claves talla-edad que empiecen por '", pref, "' en ", base)

  # ---- leer y apilar ----
  leer1 <- function(cod) {
    df <- read_dbf_simple(file.path(base, paste0(cod, ".dbf")))
    names(df) <- toupper(names(df))
    if (nrow(df) == 0) return(NULL)
    cbind(camp = cod, df)
  }
  lst <- lapply(camps.ed, leer1)
  lst <- lst[!vapply(lst, is.null, logical(1))]
  if (length(lst) == 0)
    stop("Los ficheros EDAD seleccionados no contienen registros.")
  ages <- do.call(rbind, lst)

  # validación defensiva de columnas necesarias
  faltan <- setdiff(c("NL", "ESP"), names(ages))
  if (length(faltan))
    stop("Faltan columnas en los ficheros EDAD: ", paste(faltan, collapse = ", "))

  # ---- resumen campaña x especie ----
  results <- tapply(ages$NL, ages[, c("camp", "ESP")], length)

  # nombres de especie (gr = 1 peces, asunción heredada de la versión original)
  cols     <- colnames(results)
  especies <- vapply(cols, function(e) buscaesp64(1, e, zona, dns),
                     character(1), USE.NAMES = FALSE)
  colnames(results) <- especies
  rownames(results) <- substr(rownames(results), 5, nchar(rownames(results)))

  t(results)
}
