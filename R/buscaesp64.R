#' Buscar el nombre de una especie a partir del código GRUPO–ESP (CampR64)
#'
#' Versión modernizada para CampR64. Busca el nombre científico, común
#' o en inglés de una especie a partir de su código de grupo y especie,
#' leyendo directamente el archivo `ESPECIES.DBF` mediante `leer_dbf()`.
#'
#' @param gr Código de grupo (1 peces, 2 crustáceos, 3 moluscos,
#'           4 equinodermos, 5 invertebrados, 9 total).
#' @param esp Código de especie (numérico o carácter). Use `999` para
#'            referirse a todas las especies del grupo.
#' @param id Tipo de nombre a devolver:
#'           `"l"` = científico (latín),
#'           `"e"` = español,
#'           `"i"` = inglés.
#' @param dns Origen de datos: "Camp", "Arsa", "Cant", "Porc", "Medits".
#'
#' @return Un carácter con el nombre de la especie o una descripción
#'         agregada (por ejemplo "Total peces").
#'
#' @export
buscaesp64 <- function(gr, esp, id = "l", dns = "Camp") {
  
  # --- Validación del parámetro id ---
  valores_validos <- c("l", "e", "i")
  if (!id %in% valores_validos)
    stop("id debe ser 'l' (latín), 'e' (español) o 'i' (inglés)")
  
  # --- Normalización de GRUPO y ESP como en CampR clásico ---
  gr  <- trimws(as.character(gr))
  esp <- sprintf("%3s", as.character(esp))   # ancho 3, alineado a la derecha
  
  # --- Localizar archivo ESPECIES.DBF ---
  fichero <- get_camp_file("ESPECIES.DBF", dns)
  if (!file.exists(fichero))
    stop("No encuentro ESPECIES.DBF en: ", fichero)
  
  # --- Leer DBF con conversión CP850 → UTF-8 ---
  especies <- leer_dbf(fichero)
  
  # --- Caso: varias especies a la vez ---
  if (length(esp) > 1) {
    return(if (id %in% c("l", "e")) "Varias especies" else "Several species")
  }
  
  # --- Casos especiales: grupo 9 y/o esp = 999 ---
  if (gr == "9" && esp == "999") {
    return(if (id == "i") "All Species" else "Total especies")
  }
  
  if (esp == "999") {
    if (id == "i") {
      return(paste("All",
                   c("Fish", "Crustaceans", "Molluscs",
                     "Echinoderms", "Other Invertebrates")[as.numeric(gr)]))
    } else {
      return(paste("Total",
                   c("peces", "crustáceos", "moluscos",
                     "equinodermos", "otros invertebrados")[as.numeric(gr)]))
    }
  }
  
  # --- Normalización interna de GRUPO y ESP en la tabla ---
  especies$GRUPO <- trimws(as.character(especies$GRUPO))
  especies$ESP   <- sprintf("%3s", as.character(especies$ESP))
  
  # --- Filtrar especie concreta ---
  fila <- especies[especies$GRUPO == gr & especies$ESP == esp, ]
  
  if (nrow(fila) == 0)
    return("ERROR CODIGO DESCONOCIDO")
  
  # --- Seleccionar campo según id ---
  nombre <- switch(
    id,
    "l" = fila$ESPECIE,
    "e" = fila$NOMBREE,
    "i" = fila$NOMBREI
  )
  
  as.character(nombre)
}
