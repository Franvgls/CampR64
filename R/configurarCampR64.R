#' Configurar rutas de CampR64
#'
#' Establece las rutas locales o de servidor para los datos de campañas
#' y el fichero ESPECIES.DBF. La configuración se guarda en
#' `~/.CampR64/configRoots_user.R` y es leída automáticamente al cargar
#' el paquete.
#'
#' @param env "local" o "server"
#' @param especies Ruta al fichero ESPECIES.DBF (puede ser NULL)
#' @param cant Ruta a los datos de Cantábrico/Norte (puede ser NULL)
#' @param porc Ruta a los datos de Porcupine (puede ser NULL)
#' @param arsa Ruta a los datos de ARSA (puede ser NULL)
#' @param medi Ruta a los datos de MEDITS (puede ser NULL)
#'
#' @export
configurarCampR64 <- function(env = "local",
                              especies = NULL,
                              cant     = NULL,
                              porc     = NULL,
                              arsa     = NULL,
                              medi     = NULL) {
  
  # Directorio donde .onLoad busca el fichero
  dir_user <- file.path(Sys.getenv("USERPROFILE"), ".CampR64")
  if (!dir.exists(dir_user)) dir.create(dir_user, recursive = TRUE)
  file_out <- file.path(dir_user, "configRoots_user.R")
  
  # helper: formatea valor o "" si es NULL
  fmt <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) '""' else sprintf('"%s"', x)
  
  # Escribe LOS DOS objetos que usa el paquete
  lineas <- c(
    "# CampR64 - configuracion de rutas",
    sprintf("# Generado: %s", Sys.time()),
    "",
    "# --- campRoots (interfaz de usuario) ---",
    sprintf('campRoots$%s$especies <- %s', env, fmt(especies)),
    sprintf('campRoots$%s$cant     <- %s', env, fmt(cant)),
    sprintf('campRoots$%s$porc     <- %s', env, fmt(porc)),
    sprintf('campRoots$%s$arsa     <- %s', env, fmt(arsa)),
    sprintf('campRoots$%s$medi     <- %s', env, fmt(medi)),
    sprintf('campRoots$%s$base     <- %s', env, fmt(dirname(especies %||% ""))),
    "",
    "# --- CampR64_paths (usado internamente por readCampDBF) ---",
    sprintf('CampR64_paths$%s$cant <- %s', env, fmt(cant)),
    sprintf('CampR64_paths$%s$porc <- %s', env, fmt(porc)),
    sprintf('CampR64_paths$%s$arsa <- %s', env, fmt(arsa)),
    sprintf('CampR64_paths$%s$medi <- %s', env, fmt(medi)),
    sprintf('CampR64_paths$%s$base <- %s', env, fmt(dirname(especies %||% "")))
  )
  
  writeLines(lineas, con = file_out)
  message("Configuracion guardada en: ", file_out)
  message("Reinicia R para que surta efecto.")
  invisible(file_out)
}
