#' Histograma de la distribución de tallas media de una o varias campañas
#'
#' Dibuja la distribución de tallas promediada sobre el conjunto de
#' \code{camps}. Pensada para componer (informe de especie, comparativos)
#' llamándola varias veces dentro de un \code{par(mfrow=...)} o
#' \code{layout()} con \code{restore.par = FALSE}.
#'
#' @param gr Grupo: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 invertebrados
#' @param esp Código numérico o carácter con tres espacios. 999 = todas las del grupo
#' @param camps Una campaña o vector de campañas
#' @param zona "cant", "porc", "arsa", "medi"
#' @param dns "local" o "serv"
#' @param excl.sect Sectores a excluir
#' @param cor.time Si TRUE corrige por duración del lance
#' @param ti Título principal. \code{TRUE} (por defecto) = nombre científico
#'   de la especie. \code{FALSE} = sin título. Carácter o lista = se usa tal cual
#' @param sub Subtítulo. \code{TRUE} (por defecto) = rango de campañas
#'   ("YYYY-YYYY" si \code{years=TRUE}, "Nxx-Nyy" si no). \code{FALSE} = sin subtítulo.
#'   Carácter = se usa tal cual
#' @param years Si TRUE, en el subtítulo automático muestra años; FALSE muestra códigos de campaña
#' @param ymax Tope eje Y (NA → automático)
#' @param xlim Rango de tallas para el eje X (NA → automático). Útil para
#'   forzar el mismo eje entre paneles al componer
#' @param idi Idioma del nombre de especie en el título: "l" latín, "e" español, "i" inglés
#' @param cexleg Factor de escala para el tamaño del texto del título
#' @param restore.par Si TRUE (por defecto) restaura par() al salir.
#'   FALSE cuando se compone dentro de un layout externo
#' @return Devuelve invisiblemente el data.frame con \code{talla} y \code{numero}
#' @family Distribuciones de tallas
#' @seealso \code{\link{dtallmean64.serie}}, \code{\link{dtallmean64.camp}}
#' @examples
#' \dontrun{
#'   # Uso simple
#'   dtallbarplot64(1, 98, Nsh[14:24], "cant")
#'
#'   # Comparación serie vs última campaña
#'   op <- par(mfrow = c(2, 1), mar = c(4, 4, 3, 1) + 0.1)
#'   xr <- c(1, 65); ym <- 0.2
#'   dtallbarplot64(1, 98, Nsh[14:24], "cant",
#'                  xlim = xr, ymax = ym, restore.par = FALSE)
#'   dtallbarplot64(1, 98, "N24",      "cant",
#'                  xlim = xr, ymax = ym, restore.par = FALSE)
#'   par(op)
#' }
#' @export
dtallbarplot64 <- function(gr, esp, camps, zona = "cant",
                           dns = c("local","serv"),
                           excl.sect = NA, cor.time = TRUE,
                           ti = TRUE, sub = TRUE, years = TRUE,
                           ymax = NA, xlim = NA,
                           idi = "l", cexleg = 1,
                           restore.par = TRUE) {
  dns <- match.arg(dns)
  
  # ---- Datos ----
  x <- dtallmean64.serie(gr, esp, camps, zona, dns,
                         excl.sect = excl.sect, cor.time = cor.time)
  
  # Alinear al rango xlim si se especifica (rellena ceros donde falte talla)
  if (length(xlim) == 2 && !any(is.na(xlim))) {
    tallas_all <- seq(xlim[1], xlim[2])
    x <- merge(data.frame(talla = tallas_all), x,
               by = "talla", all.x = TRUE)
    x$numero[is.na(x$numero)] <- 0
  }
  
  # ---- Títulos ----
  tit <- if (is.logical(ti)) {
    if (ti) list(label = buscaesp64(gr, esp, zona, dns, id = idi),
                 font  = ifelse(idi == "l", 4, 2),
                 cex   = 1.2 * cexleg)
    else NULL
  } else if (is.list(ti)) ti else list(label = ti)
  
  subtit <- if (is.logical(sub)) {
    if (sub) {
      if (length(camps) == 1L) {
        if (years) as.character(camptoyear(camps)) else as.character(camps)
      } else {
        first <- if (years) camptoyear(camps[1])               else camps[1]
        last  <- if (years) camptoyear(camps[length(camps)])   else camps[length(camps)]
        paste0(first, "-", last)
      }
    } else NULL
  } else as.character(sub)
  
  # ---- Eje Y ----
  mx <- if (is.na(ymax)) {
    ceiling(max(x$numero, na.rm = TRUE) * 10) / 10
  } else ymax
  
  # ---- Etiquetas eje X ----
  tallas_lab <- rep(NA_character_, nrow(x))
  idx <- seq(1, nrow(x), by = 5)
  tallas_lab[idx] <- as.character(x$talla[idx])
  
  # ---- par ----
  if (restore.par) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
  }
  
  # ---- Plot ----
  barplot(x$numero, ylim = c(0, mx * 1.1),
          names.arg = tallas_lab,
          xlab = "Length (cm)",
          ylab = expression("Ind. haul"^-1),
          space = 0, las = 1, cex.names = 0.8)
  if (!is.null(tit))    title(main = tit$label,font.main=tit$font,cex.main=tit$cex, line = 1.8)
  if (!is.null(subtit)) title(line = .6, cex.main = .9 * cexleg, subtit)
  box()
  
  invisible(x)
}