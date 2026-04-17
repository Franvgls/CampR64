#' Extrae ALK para una especie de los ficheros del camp 
#'  
#' Función de acceso a datos: recupera la clave talla edad de una especie y campaña determinadas
#' @param gr Grupo de la especie: 1 peces sólo hay claves de talla para peces y cigala?
#' @param esp Código de la especie numérico o carácter con tres espacios. Sólo admite una especie por gráfica
#' @param camp Campaña de la que se extraen los datos un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (proporciona los datos para Medits pero no saca mapas)
#' @param dns Elige el de dónde se toman los datos si del ordenador mismo "local" o del servidor en el IEO "serv"
#' @param plus Edad plus: Edad considerada como plus, todas las edades mayores se suman como edad +
#' @param n.ots Número de otolitos o proporción? Si T da el número de otolitos  
#' @param AltAlk ALK alternativa tomada de un fichero edadXYY.dbf sin ruta ni extensión
#' @param keep_sexo nantiene la columna de sexo para claves talla edad que usan el sexo por defecto =FALSE
#' @examples GetAlk.camp(1,43,"N93","Cant",8) 
#' @examples GetAlk.camp(1,45,"P03","Porc",AltAlk="edadXYY") 
#' @family edades
#' @export
GetAlk.camp64 <- function(gr, esp, camp, zona = "cant", dns = c("local","serv"),
                          plus = 8, n.ots = FALSE, AltAlk = FALSE, keep_sexo=TRUE) {
  if (length(camp) > 1)
    stop("seleccionadas más de una campaña")
  if (length(esp) > 1)
    stop("Sólo se puede incluir una especie")
  
  if (is.logical(AltAlk) | is.na(AltAlk)) {
    edad <- readCampDBF("edad", zona, camp, dns)
    edad$sexo <- as.numeric(edad$sexo)
    edad <- edad[edad$grupo == gr & edad$esp == esp, ]
    if (nrow(edad) == 0)
      stop(paste("no existe clave talla edad para la especie",
                 buscaesp64(gr, esp), "en la campaña", camp))
  } else {
    edad <- readCampDBF("edad", zona, AltAlk, dns)
    edad <- edad[edad$grupo == gr & edad$esp == esp, ]
  }
  
  edad[is.na(edad)] <- 0
  
  # Identificar columnas por nombre, no por posición
  cols_edad <- grep("^e[0-9]+$", names(edad), value = TRUE)
  
  # Filtrar filas vacías y eliminar NL
  edad <- edad[rowSums(edad[, cols_edad], na.rm = TRUE) > 0, ]
  edad <- edad[, !names(edad) %in% "nl"]
  edad <- edad[order(edad$talla, edad$sexo), ]
  
  # Agrupar edades > plus en clase plus
  if (plus < length(cols_edad)) {
    cols_keep <- cols_edad[1:plus]                          # E0..Eplus-1
    cols_plus <- cols_edad[(plus + 1):length(cols_edad)]    # Eplus..E15
    edad <- data.frame(
      edad[, c("talla", "sexo", cols_keep)],
      plus = rowSums(edad[, cols_plus, drop = FALSE])
    )
    cols_edad <- c(cols_keep, "plus")
  } 
  
  # Proporciones por fila (ALK) o números absolutos
  if (!n.ots) {
    totales <- rowSums(edad[, cols_edad])
    totales[totales == 0] <- 1   # evitar división por cero
    edad[, cols_edad] <- edad[, cols_edad] / totales
  }
  
  # Renombrar columnas de edad
  names(edad)[names(edad) %in% cols_edad] <-
    c(paste0("E", 0:(plus - 1)), paste0("E", plus, "+"))  
  
  # agebysex — para uso externo
  agebysex <<- any(edad$sexo != 3)
  
  if (keep_sexo) {
    edad[, c("talla", "sexo", 
             paste0("E", 0:(plus-1)), paste0("E", plus, "+"))]
  } else {
    edad[, c("talla", 
             paste0("E", 0:(plus-1)), paste0("E", plus, "+"))]
  }
}