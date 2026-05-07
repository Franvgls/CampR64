#' @rdname armap64
#' @export
armap.camp64 <- function(...) {
  .Deprecated("armap64", package = "CampR64",
              msg = "armap.camp64() está obsoleta; usa armap64().")
  armap64(...)
}

#' @rdname armap64
#' @export
armap.tot64 <- function(camp, zona = "porc", ...) {
  .Deprecated("armap64", package = "CampR64",
              msg = "armap.tot64() está obsoleta; usa armap64() con xlims/ylims más amplios para incluir tierra de referencia.")
  armap64(camp = camp, zona = zona, ...)
}