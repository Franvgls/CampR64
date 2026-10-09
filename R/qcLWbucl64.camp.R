#' Outliers en capturas según la relación talla-peso de todas las especies medidas en una campaña
#'
#' Recorre todas las especies presentes en el fichero NTALL de la campaña y para cada una
#' ejecuta \code{\link{qcLW64.camp}}, buscando outliers en los pesos de las capturas a partir
#' de la relación talla-peso. Las especies se procesan ordenadas de más a menos lances con tallas.
#' Se omiten (avisando) las especies sin coeficientes a y b en el fichero de especies.
#' La unidad de medida (cm/mm) se toma del campo MED del fichero de especies.
#' Se puede salir a mitad con Esc (RStudio) o cerrando la ventana gráfica.
#' @param camp Campaña: Demersales "NXX", Porcupine "PXX", Arsa "1XX"/"2XX"
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param gr Grupo(s) a revisar: 1 peces, 2 crustáceos, 3 moluscos... NULL (por defecto) revisa todos los grupos presentes en NTALL
#' @param nlans Sólo se revisan las especies con tallas en al menos \code{nlans} lances (default 2)
#' @param margerr Margen de error que delimita los datos dentro o fuera del intervalo (default 20)
#' @param ask Si TRUE espera confirmación antes de pasar al siguiente gráfico (default TRUE)
#' @return Un gráfico por especie y, de forma invisible, un data.frame resumen con grupo, especie,
#'   nombre, número de lances con tallas y número de lances/categorías fuera del margen de error
#' @examples
#' \dontrun{
#' qcLWbucl64.camp("N25", zona = "cant", dns = "local")
#' res <- qcLWbucl64.camp("P25", zona = "porc", gr = 1, nlans = 3)
#' }
#' @seealso \code{\link{qcLW64.camp}}
#' @family Control de calidad
#' @export
qcLWbucl64.camp <- function(camp = "P11", zona = "porc",
                            dns = c("local","serv"),
                            gr = NULL, nlans = 2, margerr = 20,
                            ask = TRUE) {
  if (length(camp) > 1)
    stop("Seleccionada más de una campaña, no se pueden sacar resultados de más de una")
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  # ── Especies presentes en NTALL y número de lances con tallas ──────────────
  ntall <- readCampDBF("ntall", zona = zona, camp = camp, dns = dns)
  names(ntall) <- tolower(names(ntall))
  ntall$esp   <- trimws(as.character(ntall$esp))
  ntall$grupo <- trimws(as.character(ntall$grupo))
  if (!is.null(gr)) ntall <- ntall[ntall$grupo %in% as.character(gr), ]
  if (nrow(ntall) == 0) stop("No hay tallas en NTALL de ", camp, " para los grupos seleccionados")

  lanesp <- unique(ntall[, c("grupo","esp","lance")])
  dumblist <- aggregate(lance ~ grupo + esp, data = lanesp, FUN = length)
  names(dumblist)[names(dumblist) == "lance"] <- "nlan"

  # ── Coeficientes a/b y unidad de medida desde ESPECIES ─────────────────────
  esps <- readCampDBF("especies", zona = zona, dns = dns)
  names(esps) <- tolower(names(esps))
  esps$esp   <- trimws(as.character(esps$esp))
  esps$grupo <- trimws(as.character(esps$grupo))
  idx <- match(paste(dumblist$grupo, dumblist$esp), paste(esps$grupo, esps$esp))
  dumblist$a   <- suppressWarnings(as.numeric(esps$a[idx]))
  dumblist$b   <- suppressWarnings(as.numeric(esps$b[idx]))
  dumblist$med <- if ("med" %in% names(esps)) suppressWarnings(as.numeric(esps$med[idx])) else 1

  sinab <- dumblist[is.na(dumblist$a) | is.na(dumblist$b) |
                      dumblist$a <= 0 | dumblist$b <= 0, ]
  if (nrow(sinab) > 0)
    message("Especies sin coeficientes a/b (no se revisan): ",
            paste0(sinab$grupo, "-", sinab$esp, collapse = ", "))

  dumblist <- dumblist[!is.na(dumblist$a) & !is.na(dumblist$b) &
                         dumblist$a > 0 & dumblist$b > 0 &
                         dumblist$nlan >= nlans, ]
  if (nrow(dumblist) == 0) stop("Ninguna especie cumple las condiciones para revisar")
  dumblist <- dumblist[order(dumblist$nlan, decreasing = TRUE), ]
  rownames(dumblist) <- NULL

  # ── Bucle por especie ──────────────────────────────────────────────────────
  if (ask) {
    oldask <- par(ask = TRUE)
    on.exit(par(oldask), add = TRUE)
  }
  resumen <- data.frame(grupo = dumblist$grupo, esp = dumblist$esp,
                        especie = NA_character_, nlan = dumblist$nlan,
                        n_fuera = NA_integer_, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(dumblist))) {
    g <- as.numeric(dumblist$grupo[i])
    e <- dumblist$esp[i]
    resumen$especie[i] <- tryCatch(buscaesp64(g, e, zona = zona, dns = dns),
                                   error = function(err) NA_character_)
    cat("\n── [", i, "/", nrow(dumblist), "] gr=", g, " esp=", e, " ",
        resumen$especie[i], " (", dumblist$nlan[i], " lances)\n", sep = "")
    dats <- tryCatch(
      qcLW64.camp(g, e, camp = camp, zona = zona, dns = dns,
                  margerr = margerr, out.dat = TRUE,
                  mm = isTRUE(dumblist$med[i] == 2)),
      error = function(err) {
        message("  Error en gr=", g, " esp=", e, ": ", conditionMessage(err))
        NULL
      })
    if (!is.null(dats) && nrow(dats) > 0) {
      med_err <- median(dats$error, na.rm = TRUE)
      resumen$n_fuera[i] <- sum(dats$error > med_err + margerr |
                                  dats$error < med_err - margerr, na.rm = TRUE)
    }
  }
  invisible(resumen)
}
