#' Crea la salida para el CPUE por lance del Data call de cefalópodos
#'
#' Función de Salida de datos a DATRAS:
#' Extrae las características de las capturas por lance para una campaña desde el fichero NTALLxxx.DBF y los transforma en formato DATRAS HL. De momento sólo funciona con peces y en el SPNGFS y SPPORC (Para completar crustáceos y moluscos hay que añadir los AphiaID, y para ARSA añadirlos al especies de ARSA)
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elique el origen de los datos: Porcupine "porc", Cantábrico "cant, ###### Golfo de Cádiz "arsa" no implementado
#' @param dns Elige de donde se toman los datos, "local" para ordenador del usuario, "serv" para el servidor si está implmentado en "CampR64" 
#' @param quarter si F deja en cada lance el valor del trimestre en que se realizó el lance, si T se deja el que tiene la campaña por defecto, 1 para Arsa 1Q, 3 para Porcupine y 4 para Arsa 4Q y Demersales Northern Shelf
#' @param incl2 Si F deja fuera los lances especiales que actualmente no se transmiten a DATRAS, si T los incluye
#' @param incl0 si T se incluyen los lances sin captura de la especie, si F sólo salen los lances con capturas
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @examples # CephDataByH(3,10,"P14","Porc")
#' @export
CephDataCallStr64.camp <- function(camp, zona = "cant",
                                   dns = c("local","serv"),
                                   species = NULL, incl2 = FALSE) {
  dns <- match.arg(dns)
  if (length(camp) > 1) stop("Sólo una campaña por llamada")
  if (is.null(species)) species <- cefsps$CampCode
  
  yy <- camptoyear(camp)
  
  survey <- switch(zona,
                   cant = "SP-NORTH",
                   porc = "SP-PORC",
                   arsa = "SP-ARSA",      # confirmar acrónimo oficial en Sheet 3 del template
                   stop("zona no contemplada (cant|porc|arsa)"))
  
  qrt <- switch(zona, cant = 4L, porc = 3L, arsa = 4L)   # ARSA 4Q o 1Q según camp
  mth <- switch(zona, cant = 10L, porc = 9L, arsa = 11L) # ajustar
  
  out <- list()
  
  for (sp in species) {
    asfis <- cefsps[cefsps$CampCode == sp, "ASFIS_CODE"]
    
    d <- switch(zona,
                cant = databICESdiv64(3, sp, camp, "cant", dns),
                porc = datab64(3, sp, camp, "porc", dns),
                arsa = datab64(3, sp, camp, "arsa", dns))   # asumiendo misma firma
    
    rn  <- rownames(d)
    i_n <- which(grepl("_n$", rn))
    i_p <- which(grepl("_p$", rn))
    nc  <- ncol(d)
    
    if (zona == "cant") {
      cn   <- colnames(d)
      divs <- sub("_?Avg$", "", cn[seq(1, nc - 2, by = 2)])
      for (j in seq_along(divs)) {
        ca <- 2*j - 1; cs <- 2*j
        m  <- .ices_map_cant(divs[j])
        out[[length(out) + 1]] <- .ceph_strata_row(
          survey, yy, qrt, mth, m$div, m$strata, asfis,
          d[i_n, ca], d[i_n, cs], d[i_p, ca], d[i_p, cs])
      }
      
    } else {  # porc y arsa → sólo la pareja Tot
      div <- switch(zona,
                    porc = "7c.2 and 7k.2",
                    arsa = "27.9.a")
      strata <- switch(zona,
                       porc = NA_character_,
                       arsa = "9aS")    # o NA si decides no marcarlo
      
      out[[length(out) + 1]] <- .ceph_strata_row(
        survey, yy, qrt, mth, div, strata, asfis,
        d[i_n, nc - 1], d[i_n, nc], d[i_p, nc - 1], d[i_p, nc])
    }
  }
  
  data.table::rbindlist(out, fill = TRUE)
}