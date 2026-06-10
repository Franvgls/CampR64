#' **Talla media** de la distribución de tallas media estandarizada del conjunto de la campaña **Xxx**
#'
#' Muestra la talla media de la distribución de tallas de la especie *esp* el conjunto de la campaña seleccionada
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param camp Campaña a representar en el mapa de un año comcreto (xx): Demersales "Nxx", Porcupine "Pxx", Arsa primavera "1XX", Arsa otoño "2xx" Medits "Mxx"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa" (únicamente para sacar datos al IBTS, no gráficos)
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param cor.time Si T corrige abundancias con la duración del lance para llevarlo a 30 minutos
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param restore.par TRUE por defecto, si F hace que no se restauren los parámetros gráficos para poder componer gráficos
#' @param graf si F el gráfico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @return Devuelve un data.frame con campos: camp,mean.size
#' @seealso \link{dattal.camp64}
#' @examples 
#' \dontrun{
#' dtallmean64.camp(gr=1,esp=50,camps=Nsh,zona="cant",dns="local")
#' dtallmean64.camp(gr=2,esp=19,camps=Psh,zona="porc",dns="local",graf="miratu")
#' }
#' @export
dtallmean64.camp <- function(gr, esp, camps, zona = "cant",
                             dns = c("local","serv"),
                             cor.time = TRUE, excl.sect = NA,
                             plot = TRUE, es = FALSE, idi = "l",
                             cexleg = 1, graf = FALSE,
                             xpng = 1200, ypng = 800, ppng = 15,
                             grid = TRUE, ti = TRUE, sub = TRUE, restore.par=TRUE) {
  dns <- match.arg(dns)

  if (restore.par) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)               # add para no pisar el dev.off()
  }
  
  # ---- Cálculo: una fila por campaña ----
  rows <- vector("list", length(camps))
  for (i in seq_along(camps)) {
    kk <- dattal.camp64(gr, esp, camps[i], zona, dns, sex = FALSE)
    
    if (is.null(kk) || nrow(kk) == 0 ||
        sum(kk$numero, na.rm = TRUE) == 0) {
      rows[[i]] <- data.frame(camp = camps[i],
                              mean.size = NA_real_,
                              var.size  = NA_real_,
                              sde.var   = NA_real_,
                              stringsAsFactors = FALSE)
      next
    }
    
    talla <- kk$talla + 0.5
    w     <- kk$numero
    wn    <- w / sum(w)                  # frecuencias normalizadas
    mu    <- sum(wn * talla)
    v     <- sum(wn * (talla - mu)^2)    # varianza poblacional
    
    rows[[i]] <- data.frame(
      camp      = camps[i],
      mean.size = mu,
      var.size  = v,
      sde.var   = sqrt(v),
      stringsAsFactors = FALSE)
  }
  result      <- do.call(rbind, rows)
  result$year <- camptoyear(result$camp)
  
  # ---- Etiquetas ----
  increm <- unid64.camp(gr, esp, zona, dns)["increm"]   # minúsculas
  medida <- ifelse(unid64.camp(gr, esp, zona, dns)["med"] == 1, "cm",
                   ifelse(increm == 5, "x5 mm", "mm"))
  ax <- if (es) c(paste0("Talla media (", medida, ")"), "Año")
  else    c(paste0("Mean length (", medida, ")"), "Year")
  
  # ---- Plot ----
  if (plot) {
    if (!is.logical(graf)) png(filename = paste0(graf, ".png"),
                               width = xpng, height = ypng, pointsize = ppng)
    if (is.logical(graf))  par(xaxs = "i", yaxs = "i")
    
    if (is.logical(ti)) {
      tit <- if (ti) list(buscaesp64(gr, esp, zona, dns, id = idi),
                          font = ifelse(idi == "l", 4, 2),
                          cex  = 1.2 * cexleg) else NULL
    } else {
      tit <- if (is.list(ti)) ti else list(label = ti)
    }
    
    plot(mean.size ~ year, result, type = "o", lty = 1, pch = 21,
         col = "navy", bg = "steelblue",
         ylim = c(0, max(result$mean.size, na.rm = TRUE) * 1.05),
         ylab = ax[1], xlab = ax[2], lwd = 2, yaxs = "i")
    title(main = tit, line = 1.8)
    if (is.logical(sub) && sub) title(line = .6, cex.main = .9 * cexleg, dns)
    if (!is.logical(sub))       title(line = .6, cex     = .6 * cexleg, sub)
    if (grid) grid()
    box()
    if (!is.logical(graf)) {
      dev.off()
      message(paste0("figura: ", getwd(), "/", graf, ".png"))
    }
    if (!is.logical(graf)) par(mar = c(3.1, 3.1, 4, 3.1) + 0.1)
  }
  result
}
