#' Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns elige de dónde se toman los datos, del ordenador ("local")  o del servidor ("serv") si está configurado 
#' @param inclSpecie si T incluye el nombre de la especie y el Código, si no sólo el Aphia
#' @param quart si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @param export Si T crea un fichero csv con todos los datos corregidos (APHIAs) en el directorio CAMP donde está el especies.dbf este es importable al especies.dbf con un append from deli with, quitando todos los peces grupo="1"
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CAMPtoHLnw("P14","porc","local")
#' @import data.table
#' @importFrom dplyr %>% arrange filter mutate across where
#' @importFrom worrms wm_name2id
#' @export
CAMPtoHLnw64 <- function(camp, zona="cant" ,dns=c("local","serv"), inclSpecie = FALSE, quart = TRUE, incl2 = FALSE, export = FALSE) {

  # 1. Comprobaciones iniciales
  if (length(camp) > 1) {
    stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")
  }

  # 2. Cargar datos de lances (DB)
  DB <- data.table::data.table(datlan.camp64(camp, zona,dns, redux = F, incl0 = F, incl2 = incl2))
    # Leemos el DBF directamente
    ntalls <- readCampDBF("ntall",zona,camp,dns)
    ntalls <- data.table::as.data.table(ntalls)
    ntalls <- ntalls[ntalls$grupo == '1',] # Filtrado R equivalente al WHERE de SQL

  # 3. Procesamiento común (independiente del origen de datos)
  names(ntalls) <- tolower(names(ntalls))

  especies <- readCampDBF("especies",zona,camp,dns)
  # Nota: dbDisconnect se maneja con on.exit automáticamente

  names(especies) <- tolower(names(especies))
  especies <- subset(especies, especies$grupo == 1)
  especies <- dplyr::arrange(especies, esp)
  especies <- especies %>% mutate(across(where(is.factor), as.character))
  especies$especie[1] <- buscaesp64(especies$grupo[1], especies$esp[1])

  if (substr(x = especies$especie[1], start = nchar(especies$especie[1]) - 3,
             stop = nchar(especies$especie[1])) == " sp.") {
    especies$especie[1] <- sub(" sp.", "", buscaesp64(especies$grupo[1], especies$esp[1]), fixed = TRUE)
  }
  if (is.na(especies$aphia[1])) especies$aphia[1] <- wm_name2id(as.character(especies$especie[1]))

  if (export) {
    for (i1 in 2:nrow(especies)) {
      if (is.na(especies$aphia[i1])) {
        especies$especie[i1] <- buscaesp64(especies$grupo[i1], especies$esp[i1])
        if (substr(x = especies$especie[i1], start = nchar(especies$especie[i1]) - 3,
                   stop = nchar(especies$especie[i1])) == " sp.") {
          especies$especie[i1] <- sub(" sp.", "", buscaesp64(especies$grupo[i1], especies$esp[i1]), perl = T)
        }
        especies$aphia[i1] <- wm_name2id(especies$especie[i1])
        write.csv(especies[, c("especie", "aphia")], "c:/camp/peces.csv", row.names = F)
      }
    }
    write.csv(especies, "c:/camp/peces.csv", row.names = F)
  }

  if (zona == "cant") {
    DB$Survey = "G2784"
    DB$Gear = "BAK"
    if (any(DB$barco != "29MO")) { DB$barco = ifelse(DB$barco == "MOL", "29MO", ifelse(DB$barco == "CDS", "29CS")) }
    DB$GearExceptions = -9
    DB$DoorType = ifelse(DB$barco == "29CS", "W", "P")
    if (quart) DB$quarter <- "4"
    DB$lance <- format(DB$lance, width = 3, justify = "r")
    ntalls$lance <- format(as.integer(ntalls$lance), width = 3, justify = "r")
    DB$StationName = DB$lance
  }

  if (zona == "porc") {
    DB$Survey = "G5768"
    DB$barco = "29EZ"
    DB$Gear = "PORB"
    DB$GearExceptions = -9
    DB$DoorType = "P"
    if (quart) DB$quarter <- "3"
    DB$lance <- format(as.integer(DB$lance), width = 3, justify = "r")
    ntalls$lance <- format(as.integer(ntalls$lance), width = 3, justify = "r")
    DB$StationName <- format(as.integer(DB$cuadricula), width = 3, justify = "r")
  }

  if (zona == "Arsa") {
    DB$Survey = ifelse(substr(camp, 1, 1) == "1", "G7511", "G4309")
    if (all(DB$barco %in% c("29MO", "MOL"))) { DB$barco <- "29MO" }
    if (all(DB$barco %in% c("COR", "CDS", "29CS"))) { DB$barco <- "29CS" }
    if (all(DB$barco %in% c("29VE", "VIZ"))) { DB$barco <- "29VE" }
    if (!any(DB$barco %in% c("29MO", "29CS", "29VE"))) stop("Look platform")
    DB$Gear = "BAK"
    DB$GearExceptions = -9
    DB$DoorType = ifelse(substr(DB$barco, 1, 3) == "COR", "W", "P")
    if (quart) DB$quarter <- ifelse(substr(camp, 1, 1) == "1", "1", "4")
    DB$lance <- format(DB$lance, width = 3, justify = "r")
    ntalls$lance <- format(ntalls$lance, width = 3, justify = "r")
    DB$StationName = DB$lance
  }

  DB <- DB[, c("Survey", "year", "barco", "quarter", "Gear", "malletas", "GearExceptions", "DoorType", "lance", "StationName", "validez", "prof_l", "prof_v")]
  ntalls <- ntalls[ntalls$lance %in% DB$lance, ]
  ntalls <- subset(ntalls, grupo == 1)
  ntalls$SubsamplingFactor <- round(ntalls$peso_gr / ntalls$peso_m, 4)
  ntalls <- as.data.table(ntalls)

  dumb <- ntalls[, list(SubsampledNumber = sum(numer)), by = list(lance, esp, sexo, cate)]
  dumb <- dumb[, c("lance", "esp", "sexo", "cate", "SubsampledNumber")]

  ntallsdumb <- merge(ntalls, dumb, all.x = TRUE)
  ntallsdumb$esp <- as.integer(ntallsdumb$esp)

  ntallsdumb$SpeciesCode <- as.character(especies$aphia[match(as.integer(ntallsdumb$esp), as.integer(especies$esp))])
  ntallsdumb$Specie <- as.character(especies$especie[match(as.integer(ntallsdumb$esp), as.integer(especies$esp))])
  ntallsdumb$med <- as.character(especies$med[match(ntallsdumb$esp, especies$esp)])

  if (nrow(dplyr::filter(ntallsdumb, is.na(SpeciesCode))) > 1) {
    print(dplyr::filter(ntallsdumb, is.na(SpeciesCode)))
  }

  ntallsdumb$incr <- as.character(especies$increm[match(as.integer(ntallsdumb$esp), as.integer(especies$esp))])
  ntallsdumb$LengthCode <- NA
  ntallsdumb$LengthCode[ntallsdumb$med == 1] <- "1"
  ntallsdumb$LengthCode[ntallsdumb$med == 2] <- "."
  ntallsdumb$LengthCode[ntallsdumb$incr == 5] <- "0"

  DB1 <- merge(ntallsdumb, as.data.table(DB), all.x = T, by = "lance")

  DB1$SpeciesSex <- as.character(factor(
    DB1$sexo,
    levels = as.character(1:3),
    labels = c("M", "F", "U")
  ))

  if (inclSpecie == T) {
    HL_north <- data.table(
      RecordType = "HL",
      Quarter = DB1$quarter,
      Country = "ES",
      Platform = DB1$barco,
      Gear = DB1$Gear,
      SweepLength = DB1$malletas,
      GearExceptions = DB1$GearExceptions,
      DoorType = DB1$DoorType,
      StationName = DB1$StationName,
      HaulNumber = DB1$lance,
      Year = DB1$year,
      SpeciesCodeType = "W",
      SpeciesCode = DB1$SpeciesCode,
      Specie = DB1$Specie,
      SpeciesValidity = ifelse(DB1$validez == 1, 1, 0),
      SpeciesSex = DB1$SpeciesSex,
      TotalNumber = round(DB1$SubsampledNumber * DB1$SubsamplingFactor, 2),
      SpeciesCategory = DB1$cate,
      SubsampledNumber = DB1$SubsampledNumber,
      SubsamplingFactor = DB1$SubsamplingFactor,
      SubsampleWeight = DB1$peso_m,
      CatCatchWgt = DB1$peso_gr,
      LengthCode = DB1$LengthCode,
      LengthClass = DB1$talla,
      NumberAtLength = DB1$numer,
      DevelopmentStage = -9,
      LengthType = -9,
      DateofCalculation = -9,
      Valid_Aphia = -9,
      Survey = DB1$Survey
    )
  } else {
    HL_north <- data.table(
      RecordType = "HL",
      Quarter = DB1$quarter,
      Country = "ES",
      Platform = DB1$barco,
      Gear = DB1$Gear,
      SweepLength = DB1$malletas,
      GearExceptions = DB1$GearExceptions,
      DoorType = DB1$DoorType,
      StationName = DB1$StationName,
      HaulNumber = DB1$lance,
      Year = DB1$year,
      SpeciesCodeType = "W",
      SpeciesCode = DB1$SpeciesCode,
      SpeciesValidity = ifelse(DB1$validez == 1, 1, 0),
      SpeciesSex = DB1$SpeciesSex,
      TotalNumber = round(DB1$SubsampledNumber * DB1$SubsamplingFactor, 2),
      SpeciesCategory = DB1$cate,
      SubsampledNumber = DB1$SubsampledNumber,
      SubsamplingFactor = DB1$SubsamplingFactor,
      SubsampleWeight = DB1$peso_m,
      SpeciesCategoryWeight = DB1$peso_gr,
      LengthCode = DB1$LengthCode,
      LengthClass = DB1$talla,
      NumberAtLength = DB1$numer,
      DevelopmentStage = -9,
      LengthType = -9,
      Survey = DB1$Survey,
      DateofCalculation = -9,
      Valid_Aphia = -9
    )
  }
  if (any(is.na(HL_north$SpeciesCode))) {
    HL_north[is.na(HL_north$SpeciesCode), ]
    message("Algunas especies no tienen código AphiaID, conversión incompleta, revise especies.dbf")
  }
  HL_north[order(as.numeric(HL_north$HaulNumber), HL_north$SpeciesCode, HL_north$LengthClass), ]
}
