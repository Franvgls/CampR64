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
#' @param col Color para puntos y segmentos
#' @param restore.par Si TRUE (defecto) restaura par() al salir. FALSE para componer
#' @return data.frame con columnas: \code{camp, mindpth, cog, maxdpth, year}
#' @family Distribuciones batimétricas
#' @examples
#' \dontrun{
#'   cog.sp64.camp(1, 98, Nsh[14:24], "cant")
#'   cog.sp64.camp(1, 98, Nsh, "cant", invert.y = TRUE)   # más profundo abajo
#' }
#' @export
CoG.sp64.camp <- function(gr, esp, camps, zona = "cant",
                          dns = c("local","serv"),
                          cor.time = TRUE, incl2 = FALSE,
                          plot = TRUE,
                          ti = TRUE, sub = TRUE, years = TRUE,
                          es = FALSE, idi = "l",
                          cexleg = 1, ymax = NA,
                          invert.y = FALSE, col = "blue",
                          restore.par = TRUE) {
  dns <- match.arg(dns)
  
  # ---- Cálculo: una fila por campaña ----
  rows <- vector("list", length(camps))
  for (i in seq_along(camps)) {
    dat <- datos.camp64(gr, esp, camps[i], zona, dns, incl2, cor.time)
    lan <- datlan.camp64(camps[i], zona, dns)
    dat <- merge(dat, lan[, c("lance","prof")], by = "lance", all.x = TRUE)
    
    # Sólo lances con captura efectiva
    dat <- dat[!is.na(dat$prof) & dat$numero > 0, ]
    
    if (nrow(dat) == 0 || sum(dat$numero, na.rm = TRUE) == 0) {
      rows[[i]] <- data.frame(camp = camps[i],
                              mindpth = NA_real_,
                              cog     = NA_real_,
                              maxdpth = NA_real_,
                              stringsAsFactors = FALSE)
      next
    }
    
    rows[[i]] <- data.frame(
      camp    = camps[i],
      mindpth = min(dat$prof),
      cog     = sum(dat$numero * dat$prof) / sum(dat$numero),
      maxdpth = max(dat$prof),
      stringsAsFactors = FALSE)
  }
  result      <- do.call(rbind, rows)
  result$year <- camptoyear(result$camp)
  
  # ---- Plot ----
  if (plot) {
    if (restore.par) {
      op <- par(no.readonly = TRUE)
      on.exit(par(op))
    }
    
    ax <- if (es) c("Año", "Profundidad (m)") else c("Year", "Depth (m)")
    
    ym <- if (is.na(ymax)) {
      ceiling(max(result$maxdpth, na.rm = TRUE) / 50) * 50
    } else ymax
    ylim_v <- if (invert.y) c(ym, 0) else c(0, ym)
    
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
    plot(x_axis, result$cog, type = "n",
         ylim = ylim_v, xlab = ax[1], ylab = ax[2],
         xaxt = if (years) "s" else "n")
    if (!years)
      axis(1, at = x_axis, labels = result$camp, las = 2, cex.axis = 0.8)
    
    # Segmentos verticales min ↔ max (saltando NA)
    ok <- !is.na(result$mindpth) & !is.na(result$maxdpth)
    segments(x_axis[ok], result$mindpth[ok],
             x_axis[ok], result$maxdpth[ok], col = col)
    
    # Puntos
    points(x_axis, result$maxdpth, pch = 24, bg = col, col = col)
    points(x_axis, result$mindpth, pch = 25, bg = col, col = col)
    points(x_axis, result$cog,     pch = 21, bg = col, col = col, type = "p")
    
    if (!is.null(tit))    title(main = tit$label,font.main=4,cex=tit$cex, line = 1.8)
    if (!is.null(subtit)) title(line = .6, cex.main = .9 * cexleg, subtit)
    
    leg <- if (es) c("Prof. máx.", "CoG", "Prof. mín.")
    else    c("Max depth", "CoG", "Min depth")
    legend(if (invert.y) "bottomright" else "topright",
           legend = leg, pch = c(24, 21, 25),
           bty = "n", pt.bg = col, col = col, inset = .02)
    
    box()
  }
  
  invisible(result)
}