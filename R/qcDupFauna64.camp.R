#' Control de calidad: especies duplicadas en las faunisticas de una campaña
#'
#' En el fichero de faunistica (\code{faunaXXX.dbf}) cada especie debe figurar
#' una sola vez por lance, identificada por la pareja \code{grupo + esp}. Esta
#' función localiza las combinaciones \code{lance + grupo + esp} que se repiten
#' dentro de la campaña. Estas repeticiones suelen corresponder a errores de
#' tecleo en CAMP o a registros parciales de la misma especie que en realidad
#' habría que fusionar (sumando \code{peso_gr} y \code{numero}).
#'
#' Los campos \code{lance}, \code{grupo} y \code{esp} se leen como carácter con
#' relleno de espacios a la izquierda (formato Clipper/Harbour), por lo que se
#' normalizan a entero (\code{trimws} + \code{as.integer}) antes de comparar:
#' así no se escapa un duplicado por diferencias de formato (p.ej. \code{" 9"}
#' frente a \code{"9"}).
#'
#' @param zona Zona / origen de datos tal como lo espera \code{readCampDBF}
#'   (\code{Cant}, \code{Porc}, \code{Arsa}, \code{Medi}...). Sin comillas.
#' @param camp Código de campaña, sin comillas (p.ej. \code{N15}).
#' @param dns Ruta / DSN de la zona, sin comillas, según el patrón habitual de
#'   \code{readCampDBF} en CampR64.
#' @param verbose Lógico. Si \code{TRUE} (por defecto) imprime por consola un
#'   resumen del número de especies y registros duplicados.
#'
#' @return De forma invisible, un \code{data.frame} con los registros
#'   duplicados ordenado por \code{lance}, \code{grupo}, \code{esp}, con las
#'   columnas \code{peso_gr}, \code{numero} y una columna añadida \code{n_reg}
#'   con el número de registros que comparten esa terna. Si no hay duplicados
#'   devuelve un \code{data.frame} de 0 filas.
#'
#' @examples
#' \dontrun{
#'   # Cantábrico/Galicia, campaña N15
#'   qcDupFauna64.camp(Cant, N15, Cant)
#'
#'   dups <- qcDupFauna64.camp(Cant, N15, Cant, verbose = FALSE)
#'   nrow(dups)
#' }
#' @export
qcDupFauna64.camp <- function(zona, camp, dns, verbose = TRUE) {

  # Lectura de la faunistica. read_dbf_simple aplica tolower() a los nombres,
  # de modo que las columnas son: lance, grupo, esp, peso_gr, numero.
  fauna <- readCampDBF("fauna", zona, camp, dns)

  necesarias <- c("lance", "grupo", "esp", "peso_gr", "numero")
  faltan <- setdiff(necesarias, names(fauna))
  if (length(faltan) > 0)
    stop("Faltan columnas en la faunistica: ", paste(faltan, collapse = ", "))

  # Normalización de la clave (campos C con relleno de espacios en CAMP)
  lance <- as.integer(trimws(as.character(fauna$lance)))
  grupo <- as.integer(trimws(as.character(fauna$grupo)))
  esp   <- as.integer(trimws(as.character(fauna$esp)))

  # Nº de registros que comparten cada terna lance/grupo/esp
  n_reg <- stats::ave(seq_len(nrow(fauna)), lance, grupo, esp, FUN = length)

  dup <- n_reg > 1
  res <- fauna[dup, , drop = FALSE]

  # Devolvemos la clave ya normalizada a entero (más limpia que el carácter)
  res$lance <- lance[dup]
  res$grupo <- grupo[dup]
  res$esp   <- esp[dup]
  res$n_reg <- n_reg[dup]

  res <- res[order(res$lance, res$grupo, res$esp), , drop = FALSE]
  rownames(res) <- NULL

  if (verbose) {
    if (nrow(res) == 0) {
      message("QC faunistica ", deparse(substitute(camp)),
              ": sin especies duplicadas por lance. OK.")
    } else {
      ncombos <- nrow(unique(res[c("lance", "grupo", "esp")]))
      message("QC faunistica ", deparse(substitute(camp)), ": ",
              ncombos, " especie(s) duplicada(s) en ",
              nrow(res), " registros:")
      print(res)
    }
  }

  invisible(res)
}
