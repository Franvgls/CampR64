#' VedaCigala: Veda Cigala Porcupine
#'
#' Polígono que delimita la zona de veda de la cigala en el Porcupine Bank, la zona de veda fue propuesta por los armadores españoles, y puesta en vigor en 2010 desde el 1 de mayo al 31 de julio. Desde entonces ha estado vigente en diversos periodos, pero al menos todo el mes de mayo desde 2013 a 2016.
#' @name VedaCigala
#' @docType data
#' @title Polígono área de veda cigala Porcupine
#' @usage data(VedaCigala)
#' @format A data.frame with long and lat points defining the closed area
#' @references Medidas de gestión del stock de Cigala del banco de Porcupine FU 16 \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @source Generado con \code{data-raw/VedasCigala.R}
#' @examples
#' MapPorc64(); lines(lat ~ long, VedaCigala, col = 2, lwd = 2)
#' @seealso \code{\link{VedaCigala.out}}
#' @family mapas
#' @family Porcupine
"VedaCigala"
