#' Configurar rutas de CampR64
#'
#' Establece las rutas locales o de servidor para los datos de campanas
#' y el fichero ESPECIES.DBF. La configuracion se guarda en
#' \code{~/.CampR64/configRoots_user.R} y es leida automaticamente
#' al cargar el paquete.
#'
#' Puede llamarse dos veces (una con env="local", otra con env="serv")
#' preservando la configuracion previa si se usa overwrite = FALSE.
#'
#' @param env "local" o "serv"
#' @param especies Ruta al fichero ESPECIES.DBF (puede ser NULL)
#' @param cant Ruta a los datos de Cantabrico/Norte (puede ser NULL)
#' @param porc Ruta a los datos de Porcupine (puede ser NULL)
#' @param arsa Ruta a los datos de ARSA (puede ser NULL)
#' @param medi Ruta a los datos de MEDITS (puede ser NULL)
#' @param base Ruta al directorio raiz de datos. Si NULL se deduce de especies.
#' @param overwrite Si TRUE (defecto), reescribe el archivo por completo.
#'   Si FALSE, conserva las lineas de otros entornos que ya estuvieran
#'   configuradas. Util para configurar "local" y "serv" en llamadas separadas.
#'
#' @export
configurarCampR64 <- function(env       = c("local", "serv"),
                              especies  = NULL,
                              cant      = NULL,
                              porc      = NULL,
                              arsa      = NULL,
                              medi      = NULL,
                              base      = NULL,
                              overwrite = TRUE) {
  
  env <- match.arg(env)
  
  # Normalizar rutas: barras unix y sin barra final
  norm_path <- function(x) {
    if (is.null(x) || !nzchar(x)) return(NULL)
    x <- gsub("\\\\", "/", x)
    x <- sub("/+$", "", x)
    x
  }
  especies <- norm_path(especies)
  cant     <- norm_path(cant)
  porc     <- norm_path(porc)
  arsa     <- norm_path(arsa)
  medi     <- norm_path(medi)
  base     <- norm_path(base)
  
  # Deducir base del directorio padre de especies si no se especifica
  if (is.null(base) && !is.null(especies)) {
    base <- norm_path(dirname(especies))
    # Si no se pudo deducir algo razonable, avisar
    if (is.null(base) || base %in% c(".", "", "/")) {
      warning("No se ha podido deducir 'base' a partir de 'especies' = '",
              especies, "'. Pasa 'base' explicitamente o indica la ruta ",
              "completa al fichero especies.dbf.", call. = FALSE)
      base <- NULL
    }
  }
  
  fmt <- function(x) if (is.null(x) || !nzchar(x)) '""' else sprintf('"%s"', x)
  
  dir_user <- file.path(Sys.getenv("USERPROFILE"), ".CampR64")
  if (!dir.exists(dir_user)) dir.create(dir_user, recursive = TRUE)
  file_out <- file.path(dir_user, "configRoots_user.R")
  
  # Lineas nuevas para este entorno
  lineas_nuevas <- c(
    sprintf('campRoots$%s$cant     <- %s', env, fmt(cant)),
    sprintf('campRoots$%s$porc     <- %s', env, fmt(porc)),
    sprintf('campRoots$%s$arsa     <- %s', env, fmt(arsa)),
    sprintf('campRoots$%s$medi     <- %s', env, fmt(medi)),
    sprintf('campRoots$%s$base     <- %s', env, fmt(base)),
    sprintf('campRoots$%s$especies <- %s', env, fmt(especies)),
    sprintf('CampR64_paths$%s$cant <- %s', env, fmt(cant)),
    sprintf('CampR64_paths$%s$porc <- %s', env, fmt(porc)),
    sprintf('CampR64_paths$%s$arsa <- %s', env, fmt(arsa)),
    sprintf('CampR64_paths$%s$medi <- %s', env, fmt(medi)),
    sprintf('CampR64_paths$%s$base <- %s', env, fmt(base))
  )
  
  # Si overwrite = FALSE, preservar lineas del OTRO entorno
  lineas_preservadas <- character(0)
  if (!overwrite && file.exists(file_out)) {
    contenido_actual <- readLines(file_out)
    patron_this <- paste0("\\$", env, "\\$")
    lineas_preservadas <- contenido_actual[
      grepl("^(campRoots|CampR64_paths)\\$", contenido_actual) &
        !grepl(patron_this, contenido_actual)
    ]
  }
  
  writeLines(
    c(
      "# CampR64 - configuracion de rutas",
      sprintf("# Generado: %s", Sys.time()),
      "",
      if (length(lineas_preservadas)) c(
        "# --- otros entornos preservados ---",
        lineas_preservadas,
        ""
      ),
      sprintf("# --- entorno: %s ---", env),
      lineas_nuevas
    ),
    con = file_out
  )
  
  message("Configuracion guardada en: ", file_out)
  message("Entorno configurado: ", env)
  if (length(lineas_preservadas)) {
    message("Se han preservado ", length(lineas_preservadas),
            " lineas de otros entornos.")
  }
  message("Reinicia R para que surta efecto.")
  invisible(file_out)
}