#' Revisión del histograma de distribución de tallas
#'
#' Revisa el histograma de distribución de tallas total de la especie en un lance concreto de una campaña.
#' Muestra la distribución de talla de cada categoría, los factores de ponderación por categoría,
#' el número total de individuos medidos y el número ideal de individuos a medir
#' (10 por talla: Gerritsen & McGrath 2007)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios
#' @param camp Campaña: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX", Arsa otoño "2XX"
#' @param zona Zona de las bases de datos: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param lance Número de lance a revisar
#' @param ti Si TRUE añade título al gráfico con el nombre de la especie en latín
#' @param legend Si TRUE añade leyenda de categorías
#' @param idi Nombre científico ("l") o nombre común ("e")
#' @param ymax Valor máximo del eje y (NA = automático)
#' @return Gráfico con el histograma de individuos medidos divididos en categorías y ponderaciones
#' @references Gerritsen & McGrath (2007). Precision estimates and suggested sample sizes for
#'   length-frequency data. Fish. Bull. 106: 116-120.
#' @seealso \code{\link{dtall.camp64}}
#' @examples
#' qcdtall.camp64(1, 43, "P08", zona="porc", dns="local", lance=35)
#' @family Control de calidad
#' @export
qcdtall64.camp <- function(gr, esp, camp = "P12", zona = "cant",
                            dns = c("local","serv"),
                            lance, ti = FALSE, legend = TRUE,
                            idi = "l", ymax = NA) {
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  # Formato de lance según zona (2 chars Porc/Arsa, 3 chars Cant)
  w_lance <- ifelse(zona == "cant", 3, 2)
  lance_f <- formatC(as.numeric(lance), flag = 0, width = w_lance)

  # ── Leer NTALL y filtrar ────────────────────────────────────────────────────
  ntalls <- readCampDBF("ntall", zona = zona, camp = camp, dns = dns)
  names(ntalls) <- tolower(names(ntalls))
  ntalls$lance <- formatC(as.numeric(ntalls$lance), flag = 0, width = w_lance)

  ntalls <- ntalls[ntalls$grupo == gr &
                   trimws(ntalls$esp) == trimws(esp) &
                   ntalls$lance == lance_f, ]

  if (nrow(ntalls) == 0)
    stop("No hay datos para gr=", gr, " esp=", esp,
         " lance=", lance_f, " en ", camp)

  # ── Cálculos ────────────────────────────────────────────────────────────────
  ntalls$wgnum  <- ntalls$numer * ntalls$peso_gr / ntalls$peso_m
  dtalls1 <- tapply(ntalls$wgnum, ntalls[, c("talla","cate")], sum,  na.rm = TRUE)
  dtalls2 <- tapply(ntalls$numer, ntalls[, c("talla","cate")], sum,  na.rm = TRUE)
  dtalls  <- data.frame(talla = as.numeric(rownames(dtalls1)),
                        tot   = rowSums(dtalls1, na.rm = TRUE))

  ordcat <- tapply(ntalls$talla[ntalls$numer > 0],
                   ntalls[ntalls$numer > 0, "cate"], min, na.rm = TRUE)
  ncat   <- length(ordcat)
  wghts  <- tapply(ntalls$peso_gr / ntalls$peso_m, ntalls$cate, mean, na.rm = TRUE)
  smps   <- tapply(ntalls$peso_m,  ntalls$cate, mean, na.rm = TRUE)

  dumb <- data.frame(talla = seq(1, trunc(max(dtalls$talla) / 10) * 10 + 10))
  dtall <- merge(dumb, dtalls, by = "talla", all.x = TRUE)

  for (i in 1:ncol(dtalls2)) {
    d2 <- data.frame(talla = as.numeric(rownames(dtalls2)),
                     n     = as.vector(dtalls2[, i]))
    names(d2) <- c("talla", i)
    dtall <- merge(dtall, d2, by = "talla", all.x = TRUE)
  }
  dtall[is.na(dtall)] <- 0

  leg   <- paste("Categ.", names(ordcat)[order(ordcat)])
  ntots <- colSums(dtall, na.rm = TRUE)

  # ── Gráfico ─────────────────────────────────────────────────────────────────
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

  par(mgp = c(3, 0.7, 0.7), cex.axis = 0.7)

  ylim_top <- if (is.na(ymax)) max(dtall$tot, na.rm = TRUE) * 1.1 else ymax

  barplot(dtall$tot,
          main      = if (ti) buscaesp64(gr, esp, zona = zona, dns = dns, id = idi) else "",
          font.main = 4,
          space     = 0, axes = FALSE,
          ylim      = c(0, ylim_top))
  axis(2, cex.axis = 0.8, line = 0)
  axis(1, at     = seq(1, nrow(dtall), by = 3) + 0.5,
          labels = dtall$talla[seq(1, nrow(dtall), by = 3)],
          cex.axis = 0.8, line = 0)
  box()
  barplot(t(as.matrix(dtall[, 3:ncol(dtall)])),
          add = TRUE, col = 2:5, space = 0, axes = FALSE)

  if (legend) legend("topright", rev(leg), fill = 2:5, inset = 0.05)

  mtext(paste("Campaña:", camp, "    No. lance:", lance_f),
        line = 0.5, side = 3, cex = 0.8, font = 2)

  valadj <- seq(0, 1, length.out = ncat + 2)[2:(ncat + 1)]
  counts <- 0
  for (i in names(ordcat)[order(ordcat)]) {
    counts <- counts + 1
    mtext(paste("Categ.", i), line = 1.8, side = 1,
          cex = 0.8, font = 2, adj = valadj[counts])
    mtext(paste("n =", round(ntots[i], 2),
                "  F.pond =", round(wghts[i], 2)),
          line = 2.8, side = 1, cex = 0.8, font = 2, adj = valadj[counts])
    if (!is.na(wghts[i]) && wghts[i] > 1)
      mtext(paste("n min. ideal =", sum(dtall[, i] > 0) * 10),
            side = 1, line = 3.8, cex = 0.8, font = 2, adj = valadj[counts])
  }

  invisible(dtall)
}
