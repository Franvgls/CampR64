.onLoad <- function(libname, pkgname) {
  # Valores por defecto - Windows típico local
  paths_default <- list(
    local = list(base = "c:/camp",
                 cant = "c:/camp/cant",
                 porc = "c:/camp/porc",
                 arsa = "",
                 medi = "",
                 especies = "c:/camp"),
    serv  = list(base = "z:",
                 cant = "z:/camp/datos/norte",
                 porc = "z:/camp/datos/Porcupin",
                 arsa = "z:/camp/datos/ARSA",
                 medi = "z:/camp/datos/medits",
                 especies = "z:/camp")
  )
  
  # Buscar fichero de configuración del usuario
  rutas <- c(
    file.path(Sys.getenv("USERPROFILE"), ".CampR64", "configRoots_user.R"),
    file.path(Sys.getenv("HOME"),        ".CampR64", "configRoots_user.R")
  )
  cfg <- rutas[file.exists(rutas)][1]
  
  # Cargar la configuración del usuario en un environment aislado
  if (length(cfg) > 0 && !is.na(cfg)) {
    e <- new.env()
    e$CampR64_paths <- paths_default
    e$campRoots     <- paths_default   # alias por compatibilidad
    tryCatch(
      source(cfg, local = e),
      error = function(err) {
        warning("Error cargando ", cfg, ": ", conditionMessage(err))
      }
    )
    # Recuperar el resultado tras el source
    if (!is.null(e$CampR64_paths)) paths_default <- e$CampR64_paths
    else if (!is.null(e$campRoots)) paths_default <- e$campRoots
  }
  
  # CLAVE: asignar al NAMESPACE del paquete (donde las funciones lo buscan)
  # y también al GlobalEnv (para que el usuario lo vea)
  ns <- asNamespace(pkgname)
  # Los namespaces están bloqueados por defecto - hay que desbloquear la variable
  if (exists("CampR64_paths", envir = ns, inherits = FALSE)) {
    if (bindingIsLocked("CampR64_paths", ns)) {
      unlockBinding("CampR64_paths", ns)
    }
  }
  assign("CampR64_paths", paths_default, envir = ns)
  
  # También al GlobalEnv para acceso directo del usuario
  assign("CampR64_paths", paths_default, envir = .GlobalEnv)
  assign("campRoots",     paths_default, envir = .GlobalEnv)   # alias
}

.onAttach <- function(libname, pkgname) {
  rutas <- c(
    file.path(Sys.getenv("USERPROFILE"), ".CampR64", "configRoots_user.R"),
    file.path(Sys.getenv("HOME"),        ".CampR64", "configRoots_user.R")
  )
  cfg <- rutas[file.exists(rutas)][1]
  
  if (length(cfg) > 0 && !is.na(cfg)) {
    packageStartupMessage("Cargando configuracion: ", cfg)
  } else {
    packageStartupMessage(
      "CampR64: no se encontro configRoots_user.R\n",
      "  Crea el archivo en: ",
      file.path(Sys.getenv("USERPROFILE"), ".CampR64", "configRoots_user.R")
    )
  }
}
