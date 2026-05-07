#' @keywords internal
#' @noRd
load_worldHires64 <- function() {
  if (!requireNamespace("mapdata", quietly = TRUE)) {
    warning("mapdata no instalado; usando maps::world (menos resolución).")
    return("world")
  }
  # maps::map() busca worldHiresMapEnv en globalenv(); lo inyectamos temporalmente.
  # El llamador debe encargarse del on.exit() para limpiarlo, ver patrón en maparea64.
  .e <- new.env(parent = emptyenv())
  utils::data("worldHiresMapEnv", package = "mapdata", envir = .e)
  assign("worldHiresMapEnv", .e$worldHiresMapEnv, envir = globalenv())
  "worldHires"
}

#' @keywords internal
#' @noRd
unload_worldHires64 <- function() {
  if (exists("worldHiresMapEnv", envir = globalenv(), inherits = FALSE))
    suppressWarnings(rm("worldHiresMapEnv", envir = globalenv()))
}