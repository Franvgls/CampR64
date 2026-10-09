#' VedaCigala.out: Zona ampliada de la zona de veda de Cigala en Porcupine
#'
#' Polígono que delimita una zona ampliada más grande que la zona de veda de la cigala en el Porcupine Bank, la zona de veda fue propuesta por los armadores españoles, y puesta en vigor en 2010 desde el 1 de mayo al 31 de julio. Desde entonces ha estado vigente en diversos periodos, pero al menos todo el mes de mayo desde 2013 a 2016.
#' @name VedaCigala.out
#' @docType data
#' @title Polígono zona ampliada veda cigala Porcupine
#' @usage data(VedaCigala.out)
#' @format A data.frame with long and lat points defining the area
#' @references Medidas de gestión del stock de Cigala del banco de Porcupine FU 16 \url{http://www.nwwac.org/_fileupload/Opinions and Advice/Year 12/Dictamen_CCANOC_Gestion_Cigala_Porcupine_UF16_2017_ES.pdf}
#' @source Generado con \code{data-raw/VedasCigala.R}
#' @examples
#' MapPorc64(); lines(lat ~ long, VedaCigala, col = 2, lwd = 2)
#' lines(lat ~ long, VedaCigala.out, col = "blue", lwd = 2)
#' @seealso \code{\link{VedaCigala}}
#' @family mapas
#' @family Porcupine
"VedaCigala.out"
