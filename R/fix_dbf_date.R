#' Corregir fechas de DBF con efecto Y2K (años < 1980)
#'
#' Regla:
#'   - Si el año < 1980  → añadir 100 años (1925 -> 2025, 1979 -> 2079, etc.)
#'   - En otro caso       → se deja la fecha tal cual
#'
#' @param x Vector Date o carácter (YYYY-MM-DD). Otros tipos se devuelven sin cambios.
#' @return Vector Date con el ajuste aplicado (mismo largo que x).
#' @export
fix_dbf_date <- function(x) {
  # Si ya es Date, trabajar directamente
  if (inherits(x, "Date")) {
    y <- as.integer(format(x, "%Y"))
    fix <- !is.na(y) & (y < 1980L)
    if (any(fix)) {
      y[fix] <- y[fix] + 100L
      m <- as.integer(format(x[fix], "%m"))
      d <- as.integer(format(x[fix], "%d"))
      x[fix] <- as.Date(sprintf("%04d-%02d-%02d", y[fix], m, d))
    }
    return(x)
  }
  
  # Si viene como texto (YYYY-MM-DD) intentar parsear y aplicar misma lógica
  if (is.character(x)) {
    suppressWarnings(d <- as.Date(x))
    y <- as.integer(format(d, "%Y"))
    fix <- !is.na(y) & (y < 1980L)
    if (any(fix)) {
      y[fix] <- y[fix] + 100L
      m <- as.integer(format(d[fix], "%m"))
      r <- as.integer(format(d[fix], "%d"))
      d[fix] <- as.Date(sprintf("%04d-%02d-%02d", y[fix], m, r))
    }
    return(d)
  }
  
  # Otros tipos: devolver tal cual
  x
}