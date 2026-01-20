comparar_tablas64 <- function(old, new, clave = "lance") {
  
  # 1. Comparar columnas
  cols_old <- names(old)
  cols_new <- names(new)
  
  cat("\n--- Columnas solo en OLD ---\n")
  print(setdiff(cols_old, cols_new))
  
  cat("\n--- Columnas solo en NEW ---\n")
  print(setdiff(cols_new, cols_old))
  
  # 2. Ordenar por clave
  old <- old[order(old[[clave]]), ]
  new <- new[order(new[[clave]]), ]
  
  # 3. Alinear por clave
  comunes <- intersect(old[[clave]], new[[clave]])
  old2 <- old[old[[clave]] %in% comunes, ]
  new2 <- new[new[[clave]] %in% comunes, ]
  
  # 4. Comparar valores
  cat("\n--- Diferencias por columna ---\n")
  for (col in intersect(names(old2), names(new2))) {
    dif <- old2[[col]] != new2[[col]]
    dif[is.na(dif)] <- FALSE
    if (any(dif)) {
      cat("Columna:", col, " → diferencias:", sum(dif), "\n")
    }
  }
  
  invisible(list(old = old2, new = new2))
}
