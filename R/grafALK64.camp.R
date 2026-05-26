#' Clave talla-edad gráfica (Age-Length Key)
#'
#' Barras apiladas que suman 1 por talla, mostrando la composición por edad
#' en cada clase de talla. Las tallas con reparto imputado (sin otolitos
#' reales) se distinguen con tramado diagonal y un asterisco en lugar del
#' \emph{n} muestreado.
#'
#' Complementaria a \code{\link{grafedtal.camp64}}: aquí no se representa
#' la distribución de tallas, sólo la composición por edad dentro de cada
#' una. Los datos provienen de \code{\link{GetAlk.camp64}} con
#' \code{n.ots = TRUE}, lo que permite recuperar el \emph{n} muestreado por
#' talla y detectar imputaciones.
#'
#' @section Detección de imputaciones:
#' En las bases de datos legacy de CampR, el campo de número de otolitos
#' tiene anchura de dos dígitos (\code{N(2,0)}), por lo que el código
#' \code{99} se utiliza como bandera para indicar:
#' \itemize{
#'   \item Tallas sin otolitos reales en las que se ha asignado una edad
#'         única (celda con valor 99 y resto a 0).
#'   \item Tallas en las que la composición por edad se ha repartido
#'         proporcionalmente entre las edades observadas en tallas vecinas
#'         (suma de la fila igual a 99).
#' }
#' Esta heurística da resultados correctos en la práctica totalidad de los
#' casos reales, pero puede generar falsos positivos en tallas muy bien
#' muestreadas que casualmente contengan 99 otolitos. El parámetro
#' \code{imp.detect} permite controlar este comportamiento.
#'
#' @param gr,esp,camp,zona,dns Parámetros estándar del paquete; ver
#'   \code{\link{GetAlk.camp64}}.
#' @param plus Edad del grupo terminal; las edades \eqn{\geq} \code{plus}
#'   se agrupan como "plus+".
#' @param AltAlk ALK alternativa; ver \code{\link{GetAlk.camp64}}.
#' @param ti Título: \code{TRUE} usa el nombre de la especie,
#'   \code{FALSE} sin título, o un \code{character} con el título deseado.
#' @param leg Lógico: si \code{TRUE} dibuja la leyenda de edades en la
#'   parte superior y, si hay imputaciones, una nota explicativa.
#' @param n.tal Lógico: si \code{TRUE} muestra el \emph{n} muestreado
#'   encima de cada barra (asterisco rojo en tallas imputadas).
#' @param cexleg Tamaño relativo de leyenda y título.
#' @param es Idioma: \code{TRUE} español, \code{FALSE} inglés.
#' @param cols Vector de colores con longitud \code{plus + 1}.
#'   Por defecto \code{rainbow(plus + 1, end = 5/6)}.
#' @param imp.flag Valor centinela que marca imputaciones. Por defecto
#'   \code{99} (limitación de los DBF originales). En bases de datos
#'   modernas con campos más anchos puede ajustarse a \code{NA} u otro
#'   valor.
#' @param imp.detect Estrategia de detección de imputaciones. Una de
#'   \code{"auto"} (default; celda \emph{o} suma de fila igual a
#'   \code{imp.flag}), \code{"cell"} (sólo celdas con el flag),
#'   \code{"sum"} (sólo filas cuya suma sea el flag), o \code{"none"}
#'   (no marcar ninguna).
#' @param out.dat Si \code{TRUE} devuelve invisible una lista con la
#'   matriz de proporciones, los \emph{n} reales por talla y un vector
#'   lógico de tallas imputadas.
#' @param restore.par si TRUE restaura párametros gráficos, FALSE si se quieren componer gráficos
#' @return Invisible: \code{NULL}, o una \code{list(prop, n, imputed)}
#'   si \code{out.dat = TRUE}.
#' @seealso \code{\link{grafedtal.camp64}}, \code{\link{GetAlk.camp64}}
#' @family ALK
#' @examples
#' \dontrun{
#' grafAlk64.camp(1, 90, "N23", "cant")                  # sardina
#' grafAlk64.camp(1, 72, "N23", "cant", es = FALSE)      # merluza, en inglés
#' grafAlk64.camp(1, 90, "N23", "cant", imp.detect = "none")  # sin marcar imputaciones
#' res <- grafalk64.camp(1, 72, "N23", "cant", out.dat = TRUE)
#' res$imputed
#' }
#' @export
GrafAlk64.camp <- function(gr, esp, camp, zona = "cant",
                           dns = c("local", "serv"),
                           plus = 8, AltAlk = NA,
                           ti = TRUE, leg = TRUE, n.tal = TRUE,
                           cexleg = 1, es = TRUE,
                           cols = NULL,
                           imp.flag = 99,
                           imp.detect = c("auto", "cell", "sum", "none"),
                           out.dat = FALSE,
                           restore.par=TRUE) {
  
  if (length(camp) > 1) stop("Sólo una campaña por llamada.")
  if (length(esp)  > 1) stop("Sólo una especie por llamada.")
  imp.detect <- match.arg(imp.detect)
  
  # ── Datos ────────────────────────────────────────────────────────────────
  edad <- GetAlk.camp64(gr, esp, camp, zona, dns, plus,
                        n.ots = TRUE, AltAlk = AltAlk, keep_sexo = FALSE)
  if (is.null(edad) || nrow(edad) == 0)
    stop(sprintf("No hay clave talla-edad para %s en %s",
                 buscaesp64(gr, esp, zona, dns), camp))
  
  # Quedarse con columnas de edad: E0, E1, ..., E8+ (o numéricas sueltas)
  age_cols <- grep("^E[0-9]+\\+?$|^[0-9]+$", names(edad), value = TRUE)
  if (length(age_cols) == 0)
    age_cols <- setdiff(names(edad), c("talla", "sexo", "total", "Total"))
  
  tab <- as.matrix(edad[, age_cols, drop = FALSE])
  rownames(tab) <- edad$talla
  tab[is.na(tab)] <- 0
  storage.mode(tab) <- "numeric"
  
  # ── Detección de imputaciones ────────────────────────────────────────────
  imputed <- switch(imp.detect,
                    auto = apply(tab, 1, function(r) any(r == imp.flag) || sum(r) == imp.flag),
                    cell = apply(tab, 1, function(r) any(r == imp.flag)),
                    sum  = apply(tab, 1, function(r) sum(r) == imp.flag),
                    none = rep(FALSE, nrow(tab))
  )
  
  # n real = rowSum, NA en imputadas
  n_real <- rowSums(tab)
  n_real[imputed] <- NA
  
  # Filtrar tallas vacías
  keep <- rowSums(tab) > 0
  tab     <- tab[keep, , drop = FALSE]
  imputed <- imputed[keep]
  n_real  <- n_real[keep]
  if (nrow(tab) == 0) stop("Todas las tallas están vacías tras filtrar.")
  
  prop <- prop.table(tab, margin = 1)
  
  # ── Estética ─────────────────────────────────────────────────────────────
  if (is.null(cols)) cols <- rainbow(length(age_cols), end = 5/6)
  
  idi <- ifelse(es, "e", "l")
  tit <- if (isTRUE(ti))           buscaesp64(gr, esp, zona, dns, id = idi)
  else if (is.character(ti)) ti
  else                       ""
  
  age_labs <- age_cols
  age_labs[length(age_labs)] <- paste0(sub("\\+?$", "", tail(age_labs, 1)), "+")
  
  # ── Plot ─────────────────────────────────────────────────────────────────
  top_mar <- if (leg) 6.0 else 3.5
  if (restore.par) {
    op <- par(mar = c(4.2, 4.5, top_mar, 1.0), mgp = c(2.6, .7, 0))
    on.exit(par(op), add = TRUE)               # add para no pisar el dev.off()
  }

  mp <- barplot(t(prop), col = cols, border = "gray30",
                ylim = c(0, 1.08),
                xlab = ifelse(es, "talla (cm)", "length (cm)"),
                ylab = ifelse(es, "proporción", "proportion"),
                main = tit, font.main = ifelse(idi == "l", 4, 2),
                cex.main = cexleg, las = 1, space = 0)
  abline(h = c(.25, .5, .75), col = "gray80", lty = 3)
  
  # Hatching sobre tallas imputadas (rect directo, alineación segura)
  if (any(imputed)) {
    bw <- if (length(mp) > 1) diff(mp)[1] * 0.5 else 0.5
    for (i in which(imputed)) {
      rect(mp[i] - bw, 0, mp[i] + bw, 1,
           col = NA, border = NA,
           density = 18, angle = 45)
    }
  }
  
  # n encima de cada barra (asterisco rojo si imputada)
  if (n.tal) {
    labs <- ifelse(is.na(n_real), "*", as.character(n_real))
    text(mp, 1.03, labels = labs,
         cex = 0.7 * cexleg, xpd = NA,
         col = ifelse(is.na(n_real), "red3", "gray30"),
         font = ifelse(is.na(n_real), 2, 1))
  }
  
  if (leg) {
    usr <- par("usr")
    legend(x = mean(usr[1:2]),
           y = usr[4] + (usr[4] - usr[3]) * 0.10,
           xjust = 0.5, yjust = 0.5,
           horiz = TRUE, bty = "n",
           legend = age_labs, fill = cols,
           border = "gray30", cex = 0.8 * cexleg, xpd = NA,
           x.intersp = 0.6)
    if (any(imputed))
      mtext(ifelse(es,
                   "* talla con reparto imputado (sin otolitos reales)",
                   "* length with imputed age composition (no real otoliths)"),
            side = 3, line = 0.3, cex = 0.65 * cexleg,
            col = "red3", adj = 1)
  }
  
  invisible(if (out.dat) list(prop = prop, n = n_real, imputed = imputed)
            else NULL)
}