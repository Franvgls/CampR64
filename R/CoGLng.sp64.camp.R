#' Centro de gravedad batimétrico de una especie a lo largo de una serie de campañas
#'
#' Calcula la profundidad mínima, máxima y el centro de gravedad batimétrico
#' (media de profundidades ponderada por abundancia) de la especie indicada
#' para cada campaña, y opcionalmente representa la serie temporal con
#' segmentos verticales conectando los tres valores.
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
#' @param ymax Tope eje Y (NA → automático, múltiplo de 50 m por encima del máx)
#' @param invert.y Si TRUE invierte el eje Y (mayor profundidad abajo)
#' @param maxsymb símbolo de profuncidad máxima, si NA no aparece la máxima
#' @param minsymb símbolo de prof min. Si NA no aparece
#' @param trend si T incluye una línea de tendencia del CoG a lo largo del tiempo
#' @param col Color para puntos y segmentos
#' @param out.dat Si T da como salida los datos del CoGlat
#' @param restore.par Si TRUE (defecto) restaura par() al salir. FALSE para componer
#' @return data.frame con columnas: \code{camp, mindpth, cog, maxdpth, year}
#' @family Distribuciones batimétricas
#' @examples
#' \dontrun{
#'   cog.sp64.camp(1, 98, Nsh[14:24], "cant")
#'   cog.sp64.camp(1, 98, Nsh, "cant", invert.y = TRUE)   # más profundo abajo
#' }
#' @export
CoGLng.sp64.camp <- function(gr, esp, camps, zona = "cant",
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
    dat <- merge(dat, lan[, c("lance","long")], by = "lance", all.x = TRUE)
    
    # Sólo lances con captura efectiva
    dat <- dat[!is.na(dat$long) & dat$numero > 0, ]
    
    if (nrow(dat) == 0 || sum(dat$numero, na.rm = TRUE) == 0) {
      rows[[i]] <- data.frame(camp = camps[i],
                              minlng = NA_real_,
                              cog.lng = NA_real_,
                              maxlng = NA_real_,
                              stringsAsFactors = FALSE)
      next
    }
    
    rows[[i]] <- data.frame(
      camp    = camps[i],
      minlng = min(dat$long),
      cog.lng     = sum(dat$numero * dat$long) / sum(dat$numero),
      maxlng = max(dat$long),
      stringsAsFactors = FALSE)
  }
  result      <- do.call(rbind, rows)
  result$year <- camptoyear(result$camp)
  
  # ---- modelo tendencia
  
  modelo<-lm(cog.lng~year,data=result)
  
  # ---- Plot ----
  if (plot) {
    if (restore.par) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
    }
    
    ax <- if (es) c("Año", "Longitud (\u00ba)") else c("Year", "Long (º)")
    
    # Eje Y a partir del rango real de longitudes (negativas en el Cantábrico),
    # no de la lógica de profundidad (0 -> máx redondeado a 50)
    if (length(ymax) == 2L && all(!is.na(ymax))) {
      ylim_v <- ymax
    } else {
      rng <- range(c(result$minlng, result$maxlng), na.rm = TRUE)
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
    plot(x_axis, result$cog.lng, type = "n",
         ylim = ylim_v, xlab = ax[1], ylab = ax[2],
         xaxt = if (years) "s" else "n")
    if (!years)
      axis(1, at = x_axis, labels = result$camp, las = 2, cex.axis = 0.8)
    
    # Segmentos verticales min ↔ max (saltando NA)
    ok <- !is.na(result$minlng) & !is.na(result$maxlng)
    segments(x_axis[ok], result$minlng[ok],
             x_axis[ok], result$maxlng[ok], col = col)
    
    # Puntos
    points(x_axis, result$maxlng, pch = maxsymb, bg = col, col = col)
    points(x_axis, result$minlng, pch = minsymb, bg = col, col = col)
    points(x_axis, result$cog.lng,     pch = 21, cex=1.5,bg=col, col = col, type = "p")
    
    if (!is.null(tit))    title(main = tit$label,font.main=4,cex=tit$cex, line = 1.8)
    if (!is.null(subtit)) title(line = .6, cex.main = .9 * cexleg, subtit)
    
    if (trend) abline(modelo,col="red",lty=2,lwd=2)
    
    if (legend) {leg <- if (es) c("Long. máx.","CoG long.", "Long. mín.")
    else    c("Max long", "CoG","Min long")
    legend(if (invert.y) "bottomright" else "topright",
           legend = leg, pch = c(maxsymb, 21, minsymb),
           bty = "n", pt.bg = col, col = col, inset = .02)
    }
    box()
  }
  if (out.dat) return(result)
  invisible(result)
}