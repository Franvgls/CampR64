#' Histograma y densidad de tallas combinados
#'
#' Combina en una sola figura el histograma de distribución de tallas por
#' sexos (`dtall.camp64()`) y la función de densidad con la mediana
#' (`denstall.camp64()`), usando el sistema `split` de lattice.
#'
#' @param gr Grupo taxonómico: 1 peces, 2 crustáceos, 3 moluscos.
#' @param esp Código numérico de especie.
#' @param camp Campaña(s) a representar.
#' @param zona Zona: `"cant"`, `"porc"`, `"arsa"`, `"medi"`.
#' @param dns Origen de datos: `"local"` o `"serv"`.
#' @param cor.time Si `TRUE` corrige por duración del lance.
#' @param ti Si `TRUE` añade título con nombre de la especie.
#' @param idi Nombre científico `"l"` o común `"e"`.
#' @param bw Si `TRUE` gráfico en blanco y negro.
#' @param es Si `TRUE` rótulos en español.
#' @param sex Si `TRUE` histograma desglosado por sexos.
#' @param leg Si `TRUE` añade leyenda de sexos.
#' @param cexleg Tamaño de texto de leyenda y ejes.
#' @param layout Vector `c(filas, columnas)` para multi-campaña.
#' @param years Si `TRUE` convierte códigos de campaña a años.
#' @param ymax Valor máximo del eje Y del histograma.
#' @param excl.sect Sectores a excluir.
#' @param prop Proporciones del split: `c(izquierda, derecha)` sumando 1.
#'   Por defecto `c(0.55, 0.45)` — histograma algo más ancho.
#' @param out.dat Si `TRUE` devuelve lista con los dos objetos trellis.
#' @param verbose Si `TRUE` muestra avisos de consistencia.
#'
#' @returns Invisiblemente, si `out.dat = TRUE`, lista con
#'   `$hist` (trellis histograma) y `$dens` (trellis densidad).
#'
#' @seealso [dtall.camp64()], [denstall.camp64()]
#' @family Distribuciones de tallas
#'
#' @examples
#' \dontrun{
#' # Gallo (esp=42) campaña N25 — una campaña
#' dtalldens.camp64(gr=1, esp=42, camp="N25", zona="cant", dns="local")
#'
#' # En blanco y negro sin sexos
#' dtalldens.camp64(gr=1, esp=42, camp="N25", zona="cant",
#'                  dns="local", bw=TRUE, sex=FALSE)
#'
#' # Varias campañas
#' dtalldens.camp64(gr=1, esp=42, camp=Nsh[25:29], zona="cant",
#'                  dns="local", layout=c(2,3))
#' }
#'
#' @export
dtalldens.camp64 <- function(gr, esp, camp, zona="cant", dns=c("local","serv"),
                             cor.time=TRUE, ti=FALSE, idi="l",
                             bw=TRUE, es=TRUE, sex=TRUE, leg=TRUE,
                             cexleg=1, layout=NA, years=TRUE,
                             ymax=NA, excl.sect=NA,
                             prop=c(0.55, 0.45),
                             out.dat=FALSE, verbose=FALSE) {
  dns <- match.arg(dns, c("local","serv"))
  
  # Generar los dos objetos trellis sin imprimir
  p_hist <- dtall.camp64(gr=gr, esp=esp, camp=camp, zona=zona, dns=dns,
                         cor.time=cor.time, ti=ti, idi=idi, bw=bw, es=es,
                         sex=sex, leg=leg, cexleg=cexleg, layout=layout,
                         years=years, ymax=ymax, excl.sect=excl.sect,
                         verbose=verbose, plot=FALSE)
  
  p_dens <- denstall.camp64(gr=gr, esp=esp, camp=camp, zona=zona, dns=dns,
                            cor.time=cor.time, ti=FALSE,  # título solo en hist
                            bw=bw, es=es, idi=idi, layout=layout,
                            years=years, cexleg=cexleg, plot=FALSE)
  
  # Componer con split — histograma izquierda, densidad derecha
  print(p_hist, split=c(1, 1, 2, 1), more=TRUE)
  print(p_dens, split=c(2, 1, 2, 1), more=FALSE)
  
  if (out.dat) invisible(list(hist=p_hist, dens=p_dens))
}