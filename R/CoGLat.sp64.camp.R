#' Centro de gravedad latitudinal de una especie a lo largo de una serie de campañas
#'
#' Calcula la latitud mínima, máxima y el centro de gravedad latitudinal
#' (media de latitudes ponderada por abundancia) de la especie indicada
#' para cada campaña, y opcionalmente representa la serie temporal con
#' segmentos verticales conectando los tres valores. Especialmente útil en
#' campañas con geometría no lineal (p. ej. Porcupine), donde la latitud es
#' un eje de distribución informativo e independiente de la profundidad.
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 invertebrados
#' @param esp Código numérico o carácter con tres espacios
#' @param camps Una campaña o vector de campañas
#' @param zona "cant", "porc", "arsa", "medi"
#' @param dns "local" o "serv"
#' @param cor.time Si TRUE corrige por duración del lance
#' @param incl2 Si TRUE incluye lances especiales
#' @param plot Si TRUE genera el gráfico; FALSE devuelve sólo el data.frame
#' @param ti Título principal. \code{TRUE} (defecto) = nombre científico de la
#'   especie. \code{FALSE} = sin título. Carácter/lista = literal
#' @param sub Subtítulo. \code{TRUE} (defecto) = rango de campañas/años.
#'   \code{FALSE} = sin subtítulo. Carácter = literal
#' @param years Si TRUE, subtítulo y eje X en años; FALSE en códigos de campaña
#' @param es Si TRUE etiquetas en español
#' @param idi Idioma del nombre de especie: "l" latín, "e" español, "i" inglés
#' @param cexleg Factor de escala para tamaño del título
#' @param ymax Límites del eje Y. \code{NA} (defecto) = automático a partir del
#'   rango real de latitudes. Vector de dos elementos = límites manuales (p. ej.
#'   \code{c(51, 54)})
#' @param invert.y Si TRUE invierte el eje Y (latitud mayor abajo)
#' @param maxsymb símbolo de latitud máxima, si NA no aparece la máxima
#' @param minsymb símbolo de latitud mínima. Si NA no aparece
#' @param trend si T incluye una línea de tendencia del CoG a lo largo del tiempo
#' @param col Color para puntos y segmentos
#' @param out.dat Si T da como salida los datos del CoGlat
#' @param restore.par Si TRUE (defecto) restaura par() al salir. FALSE para componer
#' @return data.frame con columnas: \code{camp, minlat, cog.lat, maxlat, year}
#' @family Distribuciones geográficas
#' @examples
#' \dontrun{
#'   CoGLat.sp64.camp(1, 50, Psh, "porc")
#'   CoGLat.sp64.camp(1, 50, Psh, "porc", invert.y = TRUE)   # más al norte abajo
#' }
#' @export
CoGLat.sp64.camp <- function(gr, esp, camps, zona = "cant",
                          dns = c("local","serv"),
                          cor.time = TRUE, incl2 = FALSE,
                          plot = TRUE,legend=TRUE,trend=TRUE,
                          ti = TRUE, sub = TRUE, years = TRUE,
                          es = FALSE, idi = "l",
                          cexleg = 1, ymax = NA,
                          invert.y = FALSE, col = "blue",maxsymb=24,
                          minsymb=25,out.dat=TRUE,
                          restore.par = TRUE) {
  dns <- match.arg(dns)

  # ---- Cálculo: una fila por campaña ----
  rows <- vector("list", length(camps))
  for (i in seq_along(camps)) {
    dat <- datos.camp64(gr, esp, camps[i], zona, dns, incl2, cor.time)
    lan <- datlan.camp64(camps[i], zona, dns, redux = T)
    dat <- merge(dat, lan[, c("lance","lat")], by = "lance", all.x = TRUE)

    # Sólo lances con captura efectiva
    dat <- dat[!is.na(dat$lat) & dat$numero > 0, ]

    if (nrow(dat) == 0 || sum(dat$numero, na.rm = TRUE) == 0) {
      rows[[i]] <- data.frame(camp = camps[i],
                              minlat = NA_real_,
                              cog.lat = NA_real_,
                              maxlat = NA_real_,
                              stringsAsFactors = FALSE)
      next
    }

    rows[[i]] <- data.frame(
      camp    = camps[i],
      minlat = min(dat$lat),
      cog.lat     = sum(dat$numero * dat$lat) / sum(dat$numero),
      maxlat = max(dat$lat),
      stringsAsFactors = FALSE)
  }
  result      <- do.call(rbind, rows)
  result$year <- camptoyear(result$camp)

  # ---- modelo tendencia

  modelo<-lm(cog.lat~year,data=result)

  # ---- Plot ----
  if (plot) {
    if (restore.par) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
    }

    ax <- if (es) c("Año", "Latitud (\u00ba)") else c("Year", "Lat (º)")

    # Eje Y a partir del rango real de latitudes
    if (length(ymax) == 2L && all(!is.na(ymax))) {
      ylim_v <- ymax
    } else {
      rng <- range(c(result$minlat, result$maxlat), na.rm = TRUE)
      pad <- diff(rng) * 0.04
      ylim_v <- c(rng[1] - pad, rng[2] + pad)
    }
    if (invert.y) ylim_v <- rev(ylim_v)

    tit <- if (is.logical(ti)) {
      if (ti) list(label = buscaesp64(gr, esp, zona, dns, id = idi),
                   font  = ifelse(idi == "l", 4, 2),
                   cex   = 1.2 * cexleg) else NULL
    } else if (is.list(ti)) ti else list(label = ti)

    subtit <- if (is.logical(sub)) {
      if (sub) {
        if (length(camps) == 1L) {
          if (years) as.character(camptoyear(camps)) else as.character(camps)
        } else {
          first <- if (years) camptoyear(camps[1])             else camps[1]
          last  <- if (years) camptoyear(camps[length(camps)]) else camps[length(camps)]
          paste0(first, "-", last)
        }
      } else NULL
    } else as.character(sub)

    x_axis <- if (years) result$year else seq_along(result$camp)

    # Lienzo
    plot(x_axis, result$cog.lat, type = "n",
         ylim = ylim_v, xlab = ax[1], ylab = ax[2],
         xaxt = if (years) "s" else "n")
    if (!years)
      axis(1, at = x_axis, labels = result$camp, las = 2, cex.axis = 0.8)

    # Segmentos verticales min ↔ max (saltando NA)
    ok <- !is.na(result$minlat) & !is.na(result$maxlat)
    segments(x_axis[ok], result$minlat[ok],
             x_axis[ok], result$maxlat[ok], col = col)

    # Puntos
    points(x_axis, result$maxlat, pch = maxsymb, bg = col, col = col)
    points(x_axis, result$minlat, pch = minsymb, bg = col, col = col)
    points(x_axis, result$cog.lat,     pch = 21, cex=1.5,bg=col, col = col, type = "p")

    if (!is.null(tit))    title(main = tit$label,font.main=4,cex=tit$cex, line = 1.8)
    if (!is.null(subtit)) title(line = .6, cex.main = .9 * cexleg, subtit)

    if (trend) abline(modelo,col="red",lty=2,lwd=2)

    if (legend) {leg <- if (es) c("Lat. máx.","CoG lat.", "Lat. mín.")
    else    c("Max lat", "CoG","Min lat")
    legend(if (invert.y) "bottomright" else "topright",
           legend = leg, pch = c(maxsymb, 21, minsymb),
           bty = "n", pt.bg = col, col = col, inset = .02)
    }
    box()
  }
  if (out.dat) return(result)
  invisible(result)
}
