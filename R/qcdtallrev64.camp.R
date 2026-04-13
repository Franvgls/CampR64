#' Revisión de los histogramas de tallas de todas las especies de una campaña
#'
#' Revisa la distribución de tallas de todas las especies de peces que hayan aparecido
#' en una campaña, una por una en pantalla. Se puede salir cerrando el gráfico.
#' @param camp Campaña: Demersales "NXX", Porcupine "PXX", Arsa "1XX"/"2XX"
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param nlans Mínimo de lances en que ha aparecido la especie para mostrarla (default 2)
#' @seealso \code{\link{qcdtall.camp64}}
#' @examples
#' qcdtallrev64.camp(camp="N25", zona="cant", dns="local", nlans=2)
#' @family Control de calidad
#' @export
qcdtallrev64.camp <- function(camp = "N12", zona = "cant",
                               dns = c("local","serv"), nlans = 2) {
  if (length(camp) > 1)
    stop("Solo se puede revisar una campaña a la vez.")
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  dumblist <- ListFauna.camp64(gr = 1, camp = camp, zona = zona, dns = dns)
  dumblist <- dumblist[order(dumblist$nlan, decreasing = TRUE), ]

  if (!par("ask")) {
    par(ask = TRUE)
    on.exit(par(ask = FALSE), add = TRUE)
  }

  for (i in 1:nrow(dumblist)) {
    dtall.camp64(1, dumblist$esp[i], camp = camp, zona = zona,
                 dns = dns, out.dat = FALSE, ti = TRUE)
    if (dumblist$nlan[i] == nlans) break
  }
}
