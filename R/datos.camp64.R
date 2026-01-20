#' Datos de abundancia y biomasa de una especie (versión DBFREAD, sin ODBC)
#'
#' Extrae los datos de abundancia y biomasa estratificados (solo lances con
#' validez 1) de una especie o conjunto de especies a partir de los ficheros
#' FAUNA y LANCE de una campaña. Devuelve un data.frame con sector, lance,
#' peso, número y arsect.
#'
#' Esta versión elimina la dependencia de ODBC/32 bits y utiliza `dbfread`
#' para leer directamente los ficheros DBF. Es científicamente equivalente
#' a `datos.camp()` y adecuada para R de 64 bits.
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos,
#'   4 equinodermos, 5 invertebrados, 6 deshechos/otros, 9 todos excepto 6.
#' @param esp Código de especie (numérico o carácter de ancho 3). 999 para todas.
#' @param camp Campaña: "NXX", "PXX", "1XX", "2XX".
#' @param dns Origen de datos: "Cant", "Porc", "Arsa", "Medi".
#' @param cor.time Si TRUE, corrige abundancias por duración del lance.
#' @param kg Si TRUE, devuelve peso en kg; si FALSE, en gramos.
#' @param verbose Si TRUE, muestra avisos informativos.
#'
#' @return Un data.frame con columnas:
#'   sector, lance, weight.time, peso, numero, arsect.
#'
#' @examples
#' \dontrun{
#'   datos.camp64(1, 50, "P10", "Porc", kg = FALSE, cor.time = TRUE)
#' }
#'
#' @export
datos.camp64 <- function(gr, esp, camp, dns,
                         cor.time = TRUE, kg = TRUE, verbose = TRUE) {

  if (length(camp) > 1)
    stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")

  # Formateo de códigos
  esp_formatted <- format(esp, width = 3, justify = "right")
  gr_formated  <- format(gr, width = 1)

  # ------------------------------------------------------------------
  # 1. Lectura de FAUNAxx.DBF con dbfread
  # ------------------------------------------------------------------
  fichero_fauna <- paste0("FAUNA", camp, ".DBF")
  path_fauna <- get_camp_file(fichero_fauna, dns)

  if (!file.exists(path_fauna))
    stop(paste("No encuentro el fichero de fauna:", path_fauna))

  fauna <- foreign::read.dbf(path_fauna)
  names(fauna) <- tolower(names(fauna))
  names(fauna) <- gsub("_", ".", names(fauna))

  fauna <- fauna[, c("lance", "grupo", "esp", "peso.gr", "numero")]

  # ------------------------------------------------------------------
  # 2. Filtrado por grupo/especie
  # ------------------------------------------------------------------
  if (length(esp) == 1) {
    if (gr != "9" & esp != "999") {
      absp <- fauna[fauna$grupo == gr & fauna$esp == esp_formatted, ]
    } else if (gr != "9" & esp == "999") {
      absp <- fauna[fauna$grupo == gr, ]
    } else if (gr == "9" & esp == "999") {
      absp <- fauna[fauna$grupo != 6, ]
    }
  } else {
    absp <- fauna[fauna$grupo == gr & fauna$esp %in% esp_formatted, ]
  }

  # ------------------------------------------------------------------
  # 3. Agregación por lance
  # ------------------------------------------------------------------
  if (nrow(absp) > 0) {
    absp <- aggregate(cbind(peso.gr, numero) ~ lance,
                      data = absp, sum, na.rm = TRUE)
  } else {
    absp <- data.frame(lance = integer(0),
                       peso.gr = numeric(0),
                       numero = numeric(0))
  }

  names(absp)[names(absp) == "peso.gr"] <- "peso"

  # ------------------------------------------------------------------
  # 4. Datos de LANCE (usando datlan.camp64)
  # ------------------------------------------------------------------
  lan <- datlan.camp64(camp, dns, redux = TRUE, incl2 = FALSE, incl0 = FALSE)

  if (any(is.na(lan$sector) | is.na(lan$estrato)))
    stop("Lances con validez 1 fuera de estratificación en campaña ", camp)

  lan_base <- lan[, c("lance", "sector", "weight.time", "arsect")]

  # ------------------------------------------------------------------
  # 5. Unión fauna + lances
  # ------------------------------------------------------------------
  mm <- merge(lan_base, absp, by = "lance", all.x = TRUE)

  mm$numero[is.na(mm$numero)] <- 0
  mm$peso[is.na(mm$peso)]     <- 0

  # ------------------------------------------------------------------
  # 6. Conversión a kg y corrección por tiempo
  # ------------------------------------------------------------------
  if (kg)
    mm$peso <- mm$peso / 1000

  if (cor.time || camp %in% c("N83", "N84")) {
    if (any(mm$weight.time == 0)) {
      mm$weight.time[mm$weight.time == 0] <- 0.1
      warning("Hay lances con duración 0 minutos; weight.time forzado a 0.1")
    }
    mm$peso   <- mm$peso   / mm$weight.time
    mm$numero <- mm$numero / mm$weight.time
  }

  # ------------------------------------------------------------------
  # 7. Limpieza final
  # ------------------------------------------------------------------
  datos <- mm
  datos$arsect <- as.numeric(datos$arsect)

  if (verbose && length(esp) > 1)
    message("Códigos de especie agrupados: ", paste(esp, collapse = ", "))

  datos <- datos[order(datos$lance), ]
  datos <- datos[, c("sector", "lance", "weight.time", "peso", "numero", "arsect")]

  datos
}
