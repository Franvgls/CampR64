#' Centro de gravedad geográfico de una especie sobre el mapa, con deriva temporal
#'
#' Combina el centro de gravedad longitudinal y latitudinal (medias ponderadas
#' por abundancia) de la especie para cada campaña y proyecta el centroide
#' resultante sobre el mapa del área, conectando los puntos en orden cronológico
#' para mostrar la deriva del centro de distribución a lo largo de la serie.
#' Pensado para campañas de geometría no lineal (p. ej. Porcupine), donde
#' latitud y longitud son ejes informativos e independientes.
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 invertebrados
#' @param esp Código numérico o carácter con tres espacios
#' @param camps Una campaña o vector de campañas
#' @param zona "cant", "porc", "arsa", "medi"
#' @param dns "local" o "serv"
#' @param cor.time Si TRUE corrige por duración del lance
#' @param incl2 Si TRUE incluye lances especiales
#' @param traj Si TRUE (defecto) une los centroides en orden cronológico
#' @param grad Si TRUE (defecto) colorea los puntos con un gradiente temporal
#' @param label Etiquetas de año: "ends" (defecto) sólo primero y último,
#'   "all" todos (usa plotrix::thigmophobe si está disponible), "none" ninguno
#' @param ti Título. TRUE (defecto) = nombre científico; carácter = literal; FALSE = sin título
#' @param sub Subtítulo. TRUE (defecto) = rango de años; carácter = literal; FALSE = sin subtítulo
#' @param idi Idioma del nombre de especie: "l" latín, "e" español, "i" inglés
#' @param pal Paleta para el gradiente temporal (ver \code{hcl.colors})
#' @param col.traj Color de la línea de trayectoria
#' @param cex Escala de los puntos del centroide
#' @param map.args Lista de argumentos para \code{maparea64()} (por defecto vacía:
#'   llamada sin argumentos). Úsalo si necesitas pasarle parámetros de área.
#' @return (invisible) data.frame con \code{camp, year, minlat, cog.lat, maxlat,
#'   minlng, cog.lng, maxlng}
#' @family Distribuciones geográficas
#' @examples
#' \dontrun{
#'   CoGmap.sp64.camp(1, 50, Psh, "porc")
#'   d <- CoGmap.sp64.camp(1, 50, Psh, "porc", label = "all")
#' }
#' @export
CoGmap.sp64.camp <- function(gr, esp, camps, zona = "porc",
                             dns = c("local","serv"),
                             cor.time = TRUE, incl2 = FALSE,
                             traj = TRUE, grad = TRUE,
                             label = c("ends","all","none"),
                             ti = TRUE, sub = TRUE, idi = "l",
                             pal = "viridis", col.traj = "grey60",
                             cex = 1.3, map.args = list()) {
  dns   <- match.arg(dns)
  label <- match.arg(label)
  
  # ---- Datos: los dos CoG sin sus gráficos propios ----
  dlat <- CoGLat.sp64.camp(gr, esp, camps, zona, dns, cor.time, incl2, plot = FALSE)
  dlng <- CoGLng.sp64.camp(gr, esp, camps, zona, dns, cor.time, incl2, plot = FALSE)
  
  dat <- merge(dlat, dlng)                       # une por camp y year
  dat <- dat[!is.na(dat$cog.lng) & !is.na(dat$cog.lat), ]
  dat <- dat[order(dat$year), ]
  n   <- nrow(dat)
  if (n == 0) stop("Sin centroides válidos: no hay capturas de la especie en las campañas dadas.")
  
  # ---- Colores del gradiente temporal ----
  cols <- if (grad) hcl.colors(n, pal) else rep("grey", n)
  
  # ---- Mapa base ----
  if (zona=="cant") do.call(MapNort64,map.args)
  if (zona=="porc") do.call(maparea64, map.args)
  if (zona=="arsa") do.call(MapArsa64, map.args)
  
  # ---- Título / subtítulo ----
  if (isTRUE(ti)) {
    title(main = buscaesp64(gr, esp, zona, dns, id = idi),
          font.main = ifelse(idi == "l", 4, 2), line = 2.5)
  } else if (is.character(ti)) {
    title(main = ti, line = 2.5)
  }
  if (isTRUE(sub)) {
    mtext(paste0(min(dat$year), "-", max(dat$year)), side = 3, line = 0.5, cex = 0.9)
  } else if (is.character(sub)) {
    mtext(sub, side = 3, line = 0.5, cex = 0.9)
  }
  
  # ---- Trayectoria cronológica ----
  if (traj && n > 1)
    lines(dat$cog.lng, dat$cog.lat, col = col.traj, lwd = 1)
  
  # ---- Centroides ----
  points(dat$cog.lng, dat$cog.lat, pch = 21, bg = cols, col = "grey30", cex = cex)
  
  # ---- Etiquetas de año ----
  if (label == "all") {
    if (requireNamespace("plotrix", quietly = TRUE)) {
      plotrix::thigmophobe.labels(dat$cog.lng, dat$cog.lat, dat$year, cex = 0.7)
    } else {
      text(dat$cog.lng, dat$cog.lat, dat$year, pos = 4, cex = 0.7)
      message("Para etiquetas sin solape instala el paquete 'plotrix'.")
    }
  } else if (label == "ends") {
    ext <- unique(c(1, n))
    text(dat$cog.lng[ext], dat$cog.lat[ext], dat$year[ext],
         pos = 4, cex = 0.8, font = 2)
  }
  
  # ---- Leyenda del gradiente ----
  if (grad && n > 1)
    legend("bottomleft", legend = c(dat$year[1], dat$year[n]),
           pch = 21, pt.bg = c(cols[1], cols[n]), pt.cex = cex,
           bty = "n", title = "Año", inset = .03)
  
  invisible(dat)
}