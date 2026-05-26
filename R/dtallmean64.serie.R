#' Distribución de tallas media de una serie de campañas
#'
#' Devuelve la distribución de tallas promediada sobre el conjunto de
#' campañas indicado en \code{camps}. Útil como insumo para gráficos
#' comparativos o cálculos posteriores.
#'
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos, 3 moluscos,
#'   4 equinodermos, 5 invertebrados
#' @param esp Código numérico o carácter (3 espacios) de la especie
#' @param camps Vector de campañas
#' @param zona "cant", "porc", "arsa"
#' @param dns "local" o "serv"
#' @param excl.sect Sectores a excluir
#' @param cor.time Si TRUE corrige por duración del lance
#' @return data.frame con columnas: \code{talla}, \code{numero}
#' @seealso \code{\link{dtallbarplot64}}, \code{\link{dtall.camp64}}
#' @examples
#' \dontrun{
#'   dtallmean64.serie(1, 98, Nsh[14:24], "cant")
#' }
#' @export
dtallmean64.serie <- function(gr, esp, camps, zona = "cant",
                              dns = c("local","serv"),
                              excl.sect = NA, cor.time = TRUE) {
  dns <- match.arg(dns)
  m <- dtall.camp64(gr, esp, camps, zona, dns,
                    excl.sect = excl.sect, cor.time = cor.time,
                    out.dat = TRUE, plot = FALSE)
  data.frame(
    talla  = as.numeric(rownames(m)),
    numero = rowMeans(m, na.rm = TRUE),
    row.names = NULL,
    stringsAsFactors = FALSE)
}