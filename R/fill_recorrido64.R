#' Rellena el recorrido ausente de los lances con la distancia Haversine
#'
#' En el CAMP un recorrido no calculado se guarda como -9 (o NA). Sustituye esos valores
#' por \code{dist.hf} allí donde esta se pueda calcular y avisa con un \code{warning()}.
#'
#' @param recorrido vector de recorridos del CAMP (m)
#' @param dist.hf distancia Haversine entre largada y virada (m); NA donde no deba usarse
#' @param lance números de lance, solo para el aviso
#' @param camp campaña, solo para el aviso
#' @param nota texto adicional opcional para el aviso
#' @return \code{recorrido} con los valores ausentes rellenados
#' @keywords internal
#' @noRd
.fill_recorrido64<-function(recorrido,dist.hf,lance,camp,nota="") {
  rellenar<-(is.na(recorrido) | recorrido<=0) & !is.na(dist.hf)
  if (any(rellenar)) {
    recorrido[rellenar]<-dist.hf[rellenar]
    warning(ifelse(all(rellenar),"Todos los lances",paste0(sum(rellenar)," de ",length(rellenar)," lances")),
            " de ",camp," tienen recorrido NA/-9 en el CAMP; se ha rellenado con la distancia Haversine (dist.hf).",
            nota," Lances: ",paste(utils::head(lance[rellenar],20),collapse=","),
            ifelse(sum(rellenar)>20,", ...",""),call.=FALSE)
  }
  recorrido
}
