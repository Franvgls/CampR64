#' Comprueba que están medidas todas las especies en capturas y viceversa en la campaña
#'
#' Recorre todos los lances de una campaña llamando a \code{\link{qcTalPez.lan64}} en cada uno.
#' Comprueba coherencia entre faunaXXX.dbf y NtallXXX.dbf y avisa de discordancias.
#' @param camp Campaña en formato Camp Xyy
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param gr Grupo: 1 peces, 2 crustáceos, 3 cefalópodos (default 1)
#' @return Imprime en consola las discordancias encontradas lance por lance
#' @examples
#' qcTalPez.camp64("N25", zona="cant", dns="local", gr=1)
#' @seealso \code{\link{qcTalPez.lan64}}
#' @family Control de calidad
#' @export
qcTalPez64.camp <- function(camp, zona = "cant", dns = c("local","serv"), gr = 1) {
  if (length(camp) > 1) stop("Solo se puede revisar una campaña a la vez.")
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  lancamp <- datlan.camp64(camp = camp, zona = zona, dns = dns,
                           incl0 = FALSE)$lance

  for (i in lancamp) {
    qcTalPez64.lan(camp = camp, zona = zona, dns = dns, lan = i, gr = gr)
  }

  invisible(NULL)
}
