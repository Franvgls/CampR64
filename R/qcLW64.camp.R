#' Outliers capturas según tallas medidas y relación talla-peso
#'
#' Busca outliers en las capturas a partir de la relación talla-peso y los pesos y tallas
#' muestreadas. El tamaño de los círculos refleja el número de peces medidos y los distintos
#' colores marcan las distintas categorías si existe muestreo por categorías.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos, 4 equinodermos, 5 invertebrados
#' @param esp Código de la especie numérico o carácter
#' @param camp Campaña: Demersales "NXX", Porcupine "PXX", Arsa "1XX"/"2XX"
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param margerr Margen de error que delimita los datos dentro o fuera del intervalo (default 20)
#' @param out.dat Si TRUE devuelve los datos estimados, observados y el error (default FALSE)
#' @param mm Si TRUE la especie está medida en milímetros (default FALSE)
#' @param areg Coeficiente "a" alternativo de la regresión talla-peso (NA = usar el del CAMP)
#' @param breg Coeficiente "b" alternativo de la regresión talla-peso (NA = usar el del CAMP)
#' @return Gráfico de errores por lance y tabla con los lances fuera del margen de error
#' @examples
#' qcLW.camp64(1, 50, "N25", zona="porc", dns="local")
#' @family Control de calidad
#' @export
qcLW64.camp <- function(gr, esp, camp = "P11", zona = "porc",
                         dns = c("local","serv"),
                         margerr = 20, out.dat = FALSE,
                         mm = FALSE, areg = NA, breg = NA) {
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  esp_orig <- esp

  # Ancho de lance según zona
  w_lance <- ifelse(zona == "cant", 3, 2)

  # ── Leer NTALL y filtrar ────────────────────────────────────────────────────
  ntall <- readCampDBF("ntall", zona = zona, camp = camp, dns = dns)
  names(ntall) <- tolower(names(ntall))
  tall_esp <- ntall[ntall$grupo == gr & trimws(ntall$esp) == trimws(esp),
                    c("lance","peso_m","cate","talla","numer")]
  tall_esp$cate <- as.factor(tall_esp$cate)
  # ── Leer FAUNA y filtrar ────────────────────────────────────────────────────
  fauna <- readCampDBF("fauna", zona = zona, camp = camp, dns = dns)
  names(fauna) <- tolower(names(fauna))
  fauna <- fauna[fauna$grupo == gr & trimws(fauna$esp) == trimws(esp), ]

  # Lances con tallas pero sin fauna y viceversa
  dumblan <- unique(trimws(tall_esp$lance))
  dumbtal <- unique(trimws(fauna$lance))
  sin_tall <- dumbtal[!dumbtal %in% dumblan]
  if (length(sin_tall) > 0)
    print(paste("Lances:", paste(sin_tall, collapse=", "),
                "sin distribución de tallas de",
                buscaesp64(gr, esp_orig, zona = zona, dns = dns)))

  # ── Leer ESPECIES para coeficientes a y b ──────────────────────────────────
  esps <- readCampDBF("especies", zona = zona, dns = dns)
  names(esps) <- tolower(names(esps))

  a <- ifelse(is.na(areg),
              esps$a[esps$grupo == gr & trimws(esps$esp) == trimws(esp)],
              areg)
  b <- ifelse(is.na(breg),
              esps$b[esps$grupo == gr & trimws(esps$esp) == trimws(esp)],
              breg)

  if (length(a) == 0 || is.na(a) || length(b) == 0 || is.na(b))
    stop("No se encuentran coeficientes a/b para gr=", gr, " esp=", esp)

  # ── Cálculo del peso estimado por talla-peso ────────────────────────────────
  if (mm) tall_esp$peso <- (a * ((tall_esp$talla / 10) + .25)^b) * tall_esp$numer
  else    tall_esp$peso <- (a *  (tall_esp$talla + .5)^b)         * tall_esp$numer

  regr   <- tapply(tall_esp$peso,   tall_esp[, c("lance","cate")], sum,  na.rm = TRUE)
  muestr <- tapply(tall_esp$peso_m, tall_esp[, c("lance","cate")], mean, na.rm = TRUE)
  nmuest <- tapply(tall_esp$numer,  tall_esp[, c("lance","cate")], sum,  na.rm = TRUE)

  dats <- data.frame(lance = NULL, estim = NULL, cate = NULL,
                     obs = NULL, n = NULL)
  for (i in 1:dim(muestr)[2]) {
    dats <- rbind(dats,
                  data.frame(lance = as.numeric(rownames(regr)),
                             estim = as.vector(regr[, i]),
                             cate  = i,
                             obs   = as.vector(muestr[, i]),
                             n     = as.vector(nmuest[, i])))
  }

  dats$error <- (dats$estim - dats$obs) * 100 / dats$obs
  dats <- dats[!is.na(dats$estim), ]
  dats <- dats[order(dats$lance, dats$cate), ]

  # ── Gráfico ─────────────────────────────────────────────────────────────────
  ylim <- c(range(dats$error)[1] * margerr / 10,
            abs(range(dats$error)[2]) * margerr / 10)

  plot(error ~ lance, dats,
       cex      = sqrt(dats$n / max(dats$n, na.rm = TRUE)) * 5,
       bg       = dats$cate + 1,
       pch      = 21,
       ylim     = ylim,
       main     = buscaesp64(gr, esp_orig, zona = zona, dns = dns),
       font.main = 4)
  mtext(paste("Campaña", camp, " a =", a, " b =", b),
        line = 0.5, side = 3, cex = 0.8, font = 2)
  mtext(expression("Error" == sum("Peso" - ("a" %*% ("Tal" + .5)^"b"))),
        line = 0.5, side = 3, cex = 0.8, font = 2, adj = 1)
  abline(h = c(0, median(dats$error)),
         lty = c(1, 2), col = c(gray(0.5), 1))
  abline(h = c(median(dats$error) - margerr,
               median(dats$error) + margerr),
         lty = 2, col = "red")

  errgr <- dats[(dats$error) > (margerr + median(dats$error)) |
                  dats$error  < (median(dats$error) - margerr), ]
  if (nrow(errgr) > 0)
    text(errgr$lance, errgr$error,
         labels = errgr$lance, pos = 1, font = 2, cex = 0.8)
  print(errgr)

  if (out.dat) return(invisible(dats))
}
