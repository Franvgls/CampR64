# Funciones internas de CampR64 

tiempo_decimal <- function(h_ini, h_fin) {
  # h_ini y h_fin son numéricos tipo HH.MM decimal
  # Parte entera = horas
  h_ini_h <- floor(h_ini)
  h_fin_h <- floor(h_fin)
  
  # Parte decimal * 100 = minutos escritos como "centésimas"
  h_ini_m <- round((h_ini - h_ini_h) * 100)
  h_fin_m <- round((h_fin - h_fin_h) * 100)
  
  # Duración en minutos reales
  (h_fin_h - h_ini_h) * 60 + (h_fin_m - h_ini_m)
}

# Operador "null-coalesce" — devuelve b si a es NULL o cadena vacía
`%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b