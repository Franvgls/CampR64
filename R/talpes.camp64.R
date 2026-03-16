#' Parámetros a y b de la relación talla-peso (CampR64)
#'
#' @param grupo   Integer (1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 invertebrados).
#' @param especie Integer o string; código (se convierte a integer).
#' @param zona    "cant","porc","arsa","medi". Por defecto "cant".
#' @param dns     "local" (default) o "serv".
#' @return numeric length 2: c(a, b)
#' @export
talpes.camp64 <- function(grupo, especie, zona = "cant",
                          dns = c("local","serv")) {
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)
  
  esp_num <- suppressWarnings(as.integer(especie))
  if (is.na(esp_num)) stop("Código de especie no interpretable como entero: ", especie)
  
  df <- readCampDBF("especies", zona = zona, dns = dns)
  if (!is.data.frame(df) || nrow(df) == 0)
    stop("No se pudo leer especies.dbf")
  
  nms <- tolower(names(df)); names(df) <- nms
  
  req <- c("esp","especie","a","b")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0)
    stop("Faltan columnas en especies.dbf: ", paste(miss, collapse = ", "))
  
  df$esp_num <- suppressWarnings(as.integer(df$esp))
  sel <- !is.na(df$esp_num) & df$esp_num == esp_num
  
  if ("grupo" %in% names(df)) {
    df$grp_num <- suppressWarnings(as.integer(df$grupo))
    sel <- sel & !is.na(df$grp_num) & df$grp_num == as.integer(grupo)
  }
  
  sub <- df[sel, , drop = FALSE]
  if (nrow(sub) == 0)
    stop("No hay registro en especies.dbf para grupo=", grupo, " esp=", esp_num)
  
  a <- suppressWarnings(as.numeric(sub$a[1]))
  b <- suppressWarnings(as.numeric(sub$b[1]))
  
  if (is.na(a) || is.na(b) || a == 0 || b == 0)
    stop("No existen A o B válidos para ", sub$especie[1], " (grupo=", grupo, ", esp=", esp_num, ")")
  
  c(a, b)
}