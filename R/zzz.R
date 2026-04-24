.onLoad <- function(libname, pkgname) {
  paths_default <- list(
    local = list(base="", cant="", porc="", arsa="", medi=""),
    serv  = list(base="", cant="", porc="", arsa="", medi="")
  )
  assign("CampR64_paths", paths_default, envir = .GlobalEnv)
  assign("campRoots",     paths_default, envir = .GlobalEnv)
  
  rutas <- c(
    file.path(Sys.getenv("USERPROFILE"), ".CampR64", "configRoots_user.R"),
    file.path(Sys.getenv("HOME"),        ".CampR64", "configRoots_user.R")
  )
  cfg <- rutas[file.exists(rutas)][1]
  
  if (length(cfg) > 0 && !is.na(cfg)) {
    source(cfg, local = FALSE)
    packageStartupMessage("Cargando configuracion: ", cfg)
  } else {
    packageStartupMessage(
      "CampR64: no se encontro configRoots_user.R\n",
      "  Crea el archivo en: ",
      file.path(Sys.getenv("USERPROFILE"), ".CampR64", "configRoots_user.R")
    )
  }
}