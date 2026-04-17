#' Recupera los AphiaId de las especies sin Aphiaid en el fichero especies.dbf
#'
#' Función de comprobación y recuperación de AphiaIDs, funciona sólo con el fichero especies.dbf del raiz del camp, no utiliza lances ni fauna de una campaña
#' Sirve para crear un fichero del que se pueden importar los códigos AphiaID al especies.dbf a través de los programas de dBase recaphia.prg o similares.
#' @param gr Grupo del que se desea buscar el AphiaID: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 otros invertebrados
#' @param zona Directorio en el que buscar el especies.dbf cant o porc para "camp" arsa para arsa
#' @param dns elige donde busca los datos si en el ordenador "local" o en el servidor "serv"
#' @param export Si T crea un fichero especies.csv con todos los datos corregidos (APHIAs) en el directorio CAMP donde está el especies.dbf este es importable al especies.dbf con un append from deli with, quitando todos los peces grupo="1"
#' @return Devuelve un data.table con datos de cada especie en el formato HL de DATRAS. DATRAS requiere que los datos no tengan cabecera y el trimestre sea el que corresponde a la campaña, además de no tener "". Por ello se debe pasar a fichero con la orden: write.table(CAMPtoHH(Xyy,dns),"nombrearchivo.csv",sep=",",quote=F,col.names=F,row.names=F))
#' @family datos_especies
#' @examples # BuscaAphia64(1,"cant","local")
#' @export
BuscaAphia64 <- function(gr = 1, zona = "cant", dns = c("local","serv"), export = TRUE) {
  
  # 1. Leer especies directamente con readCampDBF
  especies <- data.table::as.data.table(
    readCampDBF("especies", zona = zona, dns = dns, camp = NULL)
  )
  names(especies) <- tolower(names(especies))
  
  # 2. Filtrar por grupo
  especies <- subset(especies, grupo == gr)
  especies$aphia <- as.numeric(especies$aphia)
  
  if (nrow(especies[is.na(especies$aphia) | especies$aphia == 0, ]) == 0)
    stop("Todas las especies tienen AphiaID relleno")
  
  especies <- dplyr::arrange(especies, esp)
  especies <- dplyr::mutate_if(especies, is.factor, as.character)
  especies <- especies[is.na(especies$aphia) | especies$aphia == 0,
                       c("grupo", "esp", "especie", "aphia")]
  
  # Limpiar " sp." del nombre científico
  especies$especie <- sub(" sp.", "", especies$especie, fixed = TRUE)
  
  message("Especies a buscar:")
  print(especies)
  
  # 3. Buscar AphiaID en WoRMS
  for (i in seq_len(nrow(especies))) {
    if (is.na(especies$aphia[i]) || especies$aphia[i] == 0) {
      nombre <- especies$especie[i]
      # Limpiar " sp." residual
      if (endsWith(trimws(nombre), "sp."))
        nombre <- trimws(sub("sp\\.$", "", nombre))
      
      resultado <- tryCatch(
        worrms::wm_name2id(nombre, verbose = FALSE),  # ← coma aquí
        error = function(e) { 
          warning("No encontrado en WoRMS: ", nombre)
          NA_real_ 
        }
      )      
      especies$aphia[i] <- resultado
      message(nombre, " → ", resultado)
    }
  }
  
  print(especies)
  
  # 4. Exportar
  if (export) {
    # Ruta de salida usando las rutas del paquete
    ruta_out <- file.path(CampR64_paths$local$base, "especies_aphia.csv")
    write.csv(especies, ruta_out, row.names = FALSE)
    message("Fichero guardado en: ", ruta_out)
  }
  
  invisible(especies)
}
