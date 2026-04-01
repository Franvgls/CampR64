#' Editar rutas de datos de CampR64 desde la consola
#'
#' Muestra las rutas actuales y permite modificarlas interactivamente.
#' Pulsa Enter sin escribir nada para mantener la ruta actual.
#' Los cambios se aplican en la sesion actual y se guardan en
#' ~/.CampR64/configRoots_user.R
#'
#' @export
edit_campR64_paths <- function() {
  cfg <- file.path(Sys.getenv("USERPROFILE"), ".CampR64", "configRoots_user.R")
  
  # Muestra las rutas actuales
  cat("Rutas actuales:\n")
  for (zona in c("cant", "porc", "arsa", "medi", "base")) {
    cat(sprintf("  %-8s: %s\n", zona, CampR64_paths$local[[zona]]))
  }
  cat("\n")
  
  # Pide nuevas rutas (Enter = mantener la actual)
  nuevas <- list()
  for (zona in c("cant", "porc", "arsa", "medi", "base")) {
    actual <- CampR64_paths$local[[zona]]
    nueva  <- readline(sprintf("  %-8s [%s]: ", zona, actual))
    nuevas[[zona]] <- if (nzchar(nueva)) nueva else actual
  }
  
  # Aplica en la sesión actual
  for (zona in names(nuevas)) {
    CampR64_paths$local[[zona]] <<- nuevas[[zona]]
    campRoots$local[[zona]]     <<- nuevas[[zona]]
  }
  
  # Guarda en el fichero de config
  writeLines(
    c(
      "# CampR64 - configuracion de rutas",
      sprintf("# Actualizado: %s", Sys.time()),
      "",
      "# --- campRoots ---",
      sprintf('campRoots$local$cant     <- "%s"', nuevas$cant),
      sprintf('campRoots$local$porc     <- "%s"', nuevas$porc),
      sprintf('campRoots$local$arsa     <- "%s"', nuevas$arsa),
      sprintf('campRoots$local$medi     <- "%s"', nuevas$medi),
      sprintf('campRoots$local$base     <- "%s"', nuevas$base),
      "",
      "# --- CampR64_paths ---",
      sprintf('CampR64_paths$local$cant <- "%s"', nuevas$cant),
      sprintf('CampR64_paths$local$porc <- "%s"', nuevas$porc),
      sprintf('CampR64_paths$local$arsa <- "%s"', nuevas$arsa),
      sprintf('CampR64_paths$local$medi <- "%s"', nuevas$medi),
      sprintf('CampR64_paths$local$base <- "%s"', nuevas$base)
    ),
    con = cfg
  )
  
  message("Rutas actualizadas y guardadas en: ", cfg)
  invisible(nuevas)
}