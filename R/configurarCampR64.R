#' Configurar rutas de CampR64
#'
#' Permite establecer las rutas locales o de servidor para los datos
#' de campañas y el fichero ESPECIES.DBF. La configuración se guarda
#' en `~/.CampR64/configRoots_user.R`.
#'
#' @param env "local" o "server"
#' @param especies Ruta al directorio con ESPECIES.DBF
#' @param cant Ruta a los datos de Cantábrico
#' @param porc Ruta a los datos de Porcupine
#' @param arsa Ruta a los datos de ARSA
#' @param medi Ruta a los datos de MEDITS
#'
#' @export
configurarCampR64 <- function(env = "local",
                              especies, cant, porc, arsa, medi) {
  
  dir_user <- file.path(Sys.getenv("HOME"), ".CampR64")
  if (!dir.exists(dir_user)) dir.create(dir_user)
  
  file <- file.path(dir_user, "configRoots_user.R")
  
  cat(
    sprintf('campEnv <- "%s"\n', env),
    sprintf('campRoots$%s$especies <- "%s"\n', env, especies),
    sprintf('campRoots$%s$cant     <- "%s"\n', env, cant),
    sprintf('campRoots$%s$porc     <- "%s"\n', env, porc),
    sprintf('campRoots$%s$arsa     <- "%s"\n', env, arsa),
    sprintf('campRoots$%s$medi     <- "%s"\n', env, medi),
    file = file
  )
  
  message("Configuración guardada en: ", file)
}
