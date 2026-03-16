CAMPtoHH64 <- function(camp, dns, quart = TRUE, incl2 = FALSE) {
  
  if (length(camp) > 1)
    stop("Solo se puede procesar una campaña por llamada")
  
  # --- 1. Cargar datos desde datlan.camp64 ---
  DB <- datlan.camp64(camp, dns, redux = FALSE, incl0 = TRUE, incl2 = incl2)
  
  # --- 2. Normalizar barco ---
  normalize_ship <- function(x) {
    dplyr::case_when(
      x == "MOL" ~ "29MO",
      x == "CDS" ~ "CDS",
      TRUE ~ x
    )
  }
  DB$barco <- normalize_ship(DB$barco)
  
  # --- 3. Definir zona ---
  zona <- substr(dns, 1, 4)
  
  # --- 4. Funciones auxiliares para rectángulos ---
  make_rect <- function(lat, lon, lat_breaks, lat_labels, lon_breaks, lon_labels) {
    rectlat  <- cut(lat, breaks = lat_breaks, labels = lat_labels)
    rectlong <- cut(lon, breaks = lon_breaks, labels = lon_labels)
    paste0(rectlat, rectlong)
  }
  
  # --- 5. Configuración por zona ---
  if (zona %in% c("Cant", "Cnew")) {
    
    DB$icesrect <- make_rect(
      DB$latitud_l, DB$longitud_l,
      lat_breaks = seq(41.5, 44.5, 0.5),
      lat_labels = 12:17,
      lon_breaks = seq(-10, -1, 1),
      lon_labels = rev(paste0("E", 0:8))
    )
    
    DB$Gear <- "BAK"
    DB$Warpdia <- ifelse(DB$barco == "CDS", 22, 24)
    DB$DoorType <- ifelse(DB$barco == "CDS", "WR", "T4")
    DB$DoorSurface <- ifelse(DB$barco == "CDS", 3.6, 1.8)
    DB$DoorWght <- ifelse(DB$barco == "CDS", 650, 350)
    if (quart) DB$quarter <- "4"
    
    DB$lance <- formatC(DB$lance, width = 3, flag = "0")
    DB$StNo <- DB$lance
  }
  
  if (zona %in% c("Pnew", "Porc")) {
    
    DB$icesrect <- make_rect(
      DB$latitud_l, DB$longitud_l,
      lat_breaks = seq(50.5, 54, 0.5),
      lat_labels = 30:36,
      lon_breaks = seq(-15, -11, 1),
      lon_labels = rev(paste0("D", 5:8))
    )
    
    DB$barco <- "EZA"
    DB$Gear <- "PORB"
    DB$DoorType <- "P"
    DB$DoorSurface <- 4.5
    DB$DoorWght <- 850
    DB$Warpdia <- 20
    if (quart) DB$quarter <- "3"
    
    DB$lance <- formatC(DB$lance, width = 2, flag = "0")
    DB$StNo <- DB$cuadricula
    DB$estrato <- cut(DB$prof_l, breaks = c(120, 300, 450, 800), labels = c("E", "F", "G"))
  }
  
  if (zona == "Arsa") {
    
    DB$icesrect <- make_rect(
      DB$latitud_l, DB$longitud_l,
      lat_breaks = seq(36.0, 37.5, 0.5),
      lat_labels = sprintf("0%d", 1:3),
      lon_breaks = seq(-9, -6, 1),
      lon_labels = rev(paste0("E", 1:3))
    )
    
    DB$Gear <- "BAK"
    DB$barco <- ifelse(substr(DB$barco, 1, 3) == "COR", "CDS", DB$barco)
    DB$Warpdia <- ifelse(DB$barco == "CDS", 22, 24)
    DB$DoorType <- ifelse(DB$year < 2008, "WR", "T4")
    DB$DoorSurface <- ifelse(DB$year < 2008, 3.6, 1.8)
    DB$DoorWght <- ifelse(DB$year < 2008, 650, 350)
    if (quart) DB$quarter <- ifelse(substr(camp, 1, 1) == "1", "1", "4")
    
    DB$lance <- formatC(DB$lance, width = 2, flag = "0")
    DB$StNo <- DB$lance
    DB$estrato <- cut(DB$prof_l, breaks = c(1, 30, 100, 200, 500, 770),
                      labels = paste0("H", 1:5))
  }
  
  # --- 6. TimeShot ---
  DB$TimeShot <- sprintf("%02d%02d",
                         as.numeric(substr(DB$hora_l, 1, 2)),
                         as.numeric(substr(DB$hora_l, 4, 5)))
  
  # --- 7. Construir HH DATRAS ---
  HH <- data.table::data.table(
    RecordType = "HH",
    Quarter = DB$quarter,
    Country = "SPA",
    Ship = DB$barco,
    Gear = DB$Gear,
    SweepLngt = DB$malletas,
    GearExp = -9,
    DoorType = DB$DoorType,
    StNo = DB$StNo,
    HaulNo = DB$lance,
    Year = DB$year,
    Month = substr(DB$fecha, 4, 5),
    Day = substr(DB$fecha, 1, 2),
    TimeShot = DB$TimeShot,
    Stratum = DB$estrato,
    HaulDur = round(DB$haul.mins),
    DayNight = "D",
    ShootLat = DB$latitud_l,
    ShootLong = DB$longitud_l,
    HaulLat = DB$latitud_v,
    HaulLong = DB$longitud_v,
    StatRec = DB$icesrect,
    Depth = DB$prof_l,
    HaulVal = dplyr::case_when(
      DB$validez == 1 ~ "V",
      DB$validez %in% c(2, 3) ~ "A",
      TRUE ~ "I"
    ),
    HydroStNo = DB$estn,
    StdSpecRecCode = 1,
    BycSpecRecCode = 0,
    DataType = "R",
    Netopening = round(DB$abert_v, 1),
    Rigging = -9,
    Tickler = -9,
    Distance = round(DB$recorrido),
    Warplngt = DB$cable,
    Warpdia = DB$Warpdia,
    WarpDen = -9,
    DoorSurface = DB$DoorSurface,
    DoorWgt = DB$DoorWght,
    DoorSpread = trunc(DB$dista_p),
    WingSpread = round(DB$abert_h, 1),
    Bouyancy = -9,
    KiteDim = -9,
    WgtGroundRope = -9,
    TowDir = formatC(DB$rumbo, width = 3, flag = "0"),
    GroundSpeed = DB$velocidad,
    SpeedWater = -9,
    SurCurDir = -9,
    SurCurSpeed = -9,
    BotCurDir = -9,
    BotCurSpeed = -9,
    WindDir = DB$dir_viento,
    WindSpeed = DB$vel_viento,
    SwellDir = -9,
    SwellHeight = DB$est_mar,
    SurTemp = -9,
    BotTemp = DB$temp,
    SurSal = -9,
    BotSal = DB$sali,
    ThermoCline = -9,
    ThClineDepth = -9
  )
  
  # --- 8. Convertir NA → -9 ---
  cols_fix <- c("DoorSpread","Netopening","WingSpread","Distance",
                "WindDir","WindSpeed","SwellHeight","BotTemp","BotSal",
                "HydroStNo","TowDir")
  
  for (col in cols_fix) {
    HH[[col]][is.na(HH[[col]])] <- -9
  }
  
  # --- 9. Ordenar y devolver ---
  HH <- HH[order(HH$HaulNo), ]
  
  return(HH)
}
