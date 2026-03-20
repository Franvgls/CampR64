#' Datos hidrográficos de una campaña
#'
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: "cant", "porc", "arsa", "medi"
#' @param dns Elige de dónde se leen los datos, "local" para el ordenador, "serv" para el servidor
#' @param year si TRUE incluye una columna con el año al final de los datos
#' @param quarter si TRUE incluye una columna con el trimestre de los datos teniendo en cuenta la fecha del lance
#' @return Devuelve un data.frame con datos de cada estación hidrográfica y la correspondencia a los lances del fichero lance
#' @seealso {\link{MapLansGPS}}
#' @examples
#'   print(dathidro.camp64("N25","cant"))
#' @export
dathidro.camp64 <- function(camp, zona, dns = "local", year = TRUE, quarter = TRUE) {
  if (length(camp) > 1) stop("Seleccionadas más de una campaña, no se pueden sacar resultados de más de una")
  
  dathidro <- readCampDBF("hidro", zona, camp, dns)
  
  if (is.null(dathidro) || nrow(dathidro) == 0) {
    message(paste0("No existe fichero de CTDs para ", camp, " o está vacío"))
    return(invisible(NULL))
  }
  
  names(dathidro) <- tolower(names(dathidro))
  
  if ("observ" %in% names(dathidro)) dathidro <- within(dathidro, rm(observ))
  
  dathidro <- data.frame(camp = camp, dathidro, stringsAsFactors = FALSE)
  
  dathidro <- dplyr::rename(dathidro,
                            prof.ctd  = prof,
                            cable.ctd = cable,
                            hora.ctd  = hora,
                            fecha.ctd = fecha
  )
  
  dathidro$fecha.ctd <- as.Date(ifelse(
    dathidro$fecha.ctd < "1980-12-31",
    format(dathidro$fecha.ctd, "20%y-%m-%d"),
    format(dathidro$fecha.ctd)
  ))
  
  dathidro$lat.ctd  <- gradec(dathidro$latitud) * ifelse(dathidro$nosu == "N",  1, -1)
  dathidro$long.ctd <- gradec(dathidro$longitud) * ifelse(dathidro$eswe == "W", -1,  1)
  
  dathidro$lance <- as.numeric(dathidro$lance)
  dathidro$estn  <- as.numeric(dathidro$estn)
  
  if (any(!dathidro$nosu %in% c("N", "S"))) {
    message(paste("En la estacion", dathidro[!dathidro$nosu %in% c("N","S"), "estn"],
                  "el campo nosu debe ser N o S y es", dathidro[!dathidro$nosu %in% c("N","S"), "nosu"]))
  }
  if (any(!dathidro$eswe %in% c("W", "E"))) {
    message(paste("En la estación", dathidro[!dathidro$eswe %in% c("E","W"), "estn"],
                  "el campo eswe debe ser E o W y es", dathidro[!dathidro$eswe %in% c("E","W"), "eswe"]))
  }
  
  dathidro <- dplyr::select(dathidro, -longitud, -latitud, -nosu, -eswe)
  
  dathidro <- dplyr::select(dathidro,
                            camp, estn, lance, hora.ctd, fecha.ctd, lat.ctd, long.ctd, sonda, cable.ctd, prof.ctd,
                            temp0, sali0, sigma0, temp50, sali50, sigma50, temp100, sali100, sigma100,
                            temp, sali, sigma
  )
  
  dathidro$sali[dathidro$sali == 0] <- NA
  dathidro$temp[dathidro$temp == 0] <- NA
  
  dathidro$hora.ctd <- format(dathidro$hora.ctd, format = "%H")

  longlab=c(paste0("A",0:3),paste0(rep(c("B","C","D","E","F","G","H","J","K","L"),each=10),0:9),paste0("M",0:8))
  dathidro$rectlong<-cut(dathidro$long,breaks=seq(from=-44,to=69,by=1),labels=longlab) # ,"D9","D8"
  dathidro$rectlat<-cut(dathidro$lat,breaks=seq(from=36.0,to=85,by=.5),labels=sprintf("%02d", 1:98))
  dathidro$StatRec<-paste0(dathidro$rectlat,dathidro$rectlong)
  dathidro$zona<-Area[match(dathidro$StatRec,Area$ICESNAME),"Area_27"]
  dathidro$zona<-gsub("\\.","",dathidro$zona)
  for (i in 1:nrow(dathidro)) {
    if (zona=="Medi" & dathidro$lat[i]>35.8 & dathidro$long[i]>c(-5.6556)) {
      dathidro$zona[i]<-"wm.37.1"
      dathidro$StatRec<-NA
    }
  }
  dathidro$StatRec <- ifelse(
    substr(dathidro$rectlong,1,1) == "E",
    paste0("#", dathidro$rectlat, dathidro$rectlong),
    paste0(dathidro$rectlat, dathidro$rectlong)
  )
  dathidro$rectlat <- NULL
  dathidro$rectlong <- NULL
  
  # lebels <- c(paste0("C", 0:9), paste0("D", 0:9), paste0("E", 0:9),
  #             paste0("F", 0:9), paste0("G", 0:9), paste0("H", 0:9))
  # dathidro$rectlong <- cut(dathidro$long.ctd, breaks = seq(from = -30, to = 40, by = 1), labels = lebels)
  # dathidro$rectlat  <- cut(dathidro$lat.ctd,  breaks = seq(from = 36.0, to = 71, by = .5),
  #                          labels = formatC(c(1:70), flag = 0, width = 2))
  # 
  # dathidro$StatRec <- ifelse(
  #   substr(dathidro$rectlong, 1, 1) == "E",
  #   paste0("#", dathidro$rectlat, dathidro$rectlong),
  #   paste0(dathidro$rectlat, dathidro$rectlong)
  # )
  # dathidro$rectlat  <- NULL
  # dathidro$rectlong <- NULL
  
  dathidro$icesArea <- Area[match(sub("#", "", dathidro$StatRec), Area$ICESNAME), "Area"]
  
  if (any(is.na(dathidro$icesArea))) {
    message(paste0("Al menos una estación: ",
                   paste(dathidro$estn[is.na(dathidro$icesArea)], collapse = ","),
                   " sin Zona ICES asignada, revise resultados"))
  }
  
  if (quarter) dathidro$quarter <- substr(quarters(as.Date(dathidro$fecha.ctd)), 2, 2)
  if (year)    dathidro$year    <- lubridate::year(dathidro$fecha.ctd)
  
  return(dathidro)
}