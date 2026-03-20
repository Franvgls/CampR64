#' Datos de abundancia y biomasa de una especie 
#'
#' Función de acceso a datos: 
#' Extrae los datos de abundancia y biomasa estratificados (solo lances con validez 1) de una especie o conjunto de especies a partir de las faunísticas de una campaña. Crea una tabla con información del sector(número) y estrato (letra), lance, peso, número y arsect
#' Funciones para obtener data.frame de especies concretas a partir de los ficheros del Camp
#' @param gr grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param dns Elige el origen de las bases de datos: Porcupine "Porc", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param kg Si T datos en kgs, si F en gramos 
#' @param verbose si T avisa de que hay más de una especie y los datos mezclados pueden ser engañosos
#' @return Devuelve un data.frame con los datos de la especie por lance: sector,lance,peso,numero,arsect (área del sector al que pertence el lance)
#' @details Saca los datos de los lances estratificados, por ello se produce un error si encuentra un lance con validez 1 y estrato o sector sin información.
#' @examples datos.camp(1,50,"P10","Porc",kg=FALSE, cor.time=TRUE)
#' @export
datos.camp64 <- function(gr, esp, camp, zona, dns="local", 
                         cor.time=TRUE, kg=TRUE, verbose=TRUE,
                         incl2=FALSE) {
  
  dns <- match.arg(dns)
  if (length(camp) > 1) stop("más de una campaña seleccionada")
  
  fauna <- readCampDBF("fauna", zona, camp, dns)
  
  # Filtro de especie/grupo
  if (length(esp) == 1) {
    if (gr != "9" & esp != "999")
      absp <- fauna[fauna$grupo == as.integer(gr) & 
                      fauna$esp == as.integer(esp), c(1,4:5)]
    if (gr != "9" & esp == "999")
      absp <- fauna[fauna$grupo == as.integer(gr), c(1,4:5)]
    if (gr == "9" & esp == "999")
      absp <- fauna[fauna$grupo != 6, c(1,4:5)]
  } else {
    absp <- fauna[fauna$grupo == gr & 
                    fauna$esp %in% as.integer(esp), c(1,4:5)]
  }
  names(absp) <- gsub("_", ".", tolower(names(absp)))
  
  if (any(gr=="9" | esp=="999" | length(esp) > 1)) {
    absp <- data.frame(
      lance   = names(tapply(absp$peso.gr, absp$lance, sum)),
      peso.gr = tapply(absp$peso.gr, absp$lance, sum),
      numero  = tapply(absp$numero, absp$lance, sum))
  }
  absp$lance <- as.numeric(as.character(absp$lance))
  names(absp) <- c("lance", "peso", "numero")
  if (kg) absp$peso <- absp$peso / 1000
  
  # ── BIFURCACIÓN PRINCIPAL ──────────────────────────────────────────
  
  if (!incl2) {
    # ── Rama estándar: abundancia estratificada (IBTS) ────────────────
    lan <- datlan.camp64(camp, zona, dns, redux=TRUE, 
                         incl2=FALSE, incl0=FALSE)
    if (any(is.na(lan$sector) | is.na(lan$estrato)))
      stop(paste("Lances con validez 1 fuera de estratificación en", camp))
    
    lan <- lan[, c("lance","sector","weight.time")]
    
    # Áreas de estrato
    dumb <- readCampDBF("camp", zona, camp, dns)
    area <- dumb[, 21:45]
    if ((sum(is.na(area)) / length(area)) == 1)
      stop(paste("campXXX.dbf vacío, sin estratificación para", camp))
    area <- area[-which(is.na(area) | area == 0)]
    area <- as.data.frame(cbind(substr(names(area), 2, 3), 
                                as.numeric(t(area))))
    names(area) <- c("sector", "arsect")
    area$sector <- toupper(area$sector)
    
    # Merge lances estándar → capturas (NA→0 en lances sin captura)
    mm <- merge(lan, absp, by="lance", all.x=TRUE)
    mm$numero[is.na(mm$numero)] <- 0
    mm$peso[is.na(mm$peso)]     <- 0
    
    if (cor.time | camp %in% c("N83","N84")) {
      if (any(mm$weight.time == 0)) {
        mm$weight.time[mm$weight.time == 0] <- 0.1
        warning("Lances con duración 0 min — revisa validez")
      }
      mm$peso   <- mm$peso   / mm$weight.time
      mm$numero <- mm$numero / mm$weight.time
    }
    
    datos <- merge(mm, area, by="sector")
    datos$arsect <- as.numeric(as.character(datos$arsect))
    
  } else {
    # ── Rama total: suma directa con lances especiales ────────────────
    lan <- datlan.camp64(camp, zona, dns, redux=TRUE, 
                         incl2=TRUE, incl0=FALSE)
    lan <- lan[, c("lance","sector","weight.time")]
    
    mm <- merge(lan, absp, by="lance", all.x=TRUE)
    mm$numero[is.na(mm$numero)] <- 0
    mm$peso[is.na(mm$peso)]     <- 0
    
    # Sin ponderación por área — arsect = 1 para todos
    mm$arsect <- 1
    datos <- mm
  }
  
  if (length(esp) > 1 & verbose) 
    print(c("Códigos de especie: ", esp))
  
  datos[order(datos$lance), ]
}