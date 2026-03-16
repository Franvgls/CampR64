#' Unidades en que se mide una especie
#'
#' Da información de las unidades en que se miden las especies y permite añadir datos a los gráficos
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @export
unid.camp64<-function(gr,esp,zona="cant",dns="local") {
  #esp<-format(esp,width=3,justify="r")
  if (length(gr)>1 | length(esp)>2) {
    stop("Esta función no permite más de una especie por vez")
  }
  else {especie<-readCampDBF("especies",zona,dns)}
  especie<-especie[especie$grupo==gr & especie$esp==as.character(esp),c("med","increm")] 
  #RODBC::sqlQuery(ch1,paste("select MED,INCREM from Especies where grupo='",gr,"' and esp='",esp,"'",sep=""))
  #@RODBC::odbcClose(ch1)
  especie
}