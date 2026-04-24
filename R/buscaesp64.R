#' Función de búsqueda del nombre de una especie
#'
#' Busca el nombre de una especie a partir del código del grupo y especie.Puede dar el nombre científico, común o en inglés
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo
#' @param id Salida como: "l" Nombre científico en latín, "e" nombre en español, "i" nombre en inglés "a" código APHIA
#' @family datos_especies
#' @examples buscaesp(1,50)
#' @export
buscaesp64<- function(gr,esp,zona,dns=c("local","serv"),id="l") {
  esp<-as.character(esp)  #,width=3,justify="r")
  values<-c("i","e","l","a")
  if (!id %in% values) stop("Campo id debe ser l: latín, i: inglés, e: español o a: codigo AphiaID")
  if (length(esp)>1) {
    if (id=="l" | id=="e") {especie<-"Varias especies"}
    if (id=="i") { especie<-"Several species" }
  }
  else {
    especie<-readCampDBF("especies",zona,dns=dns)
    if (gr!="9" & esp!="999") {
      if (id=="l") {
        especie<-especie[especie$grupo==as.character(gr) & especie$esp==esp,"especie"]         
        }
      if (id=="i") {
        especie<-especie[especie$grupo==as.character(gr) & especie$esp==esp,"nombrei"]         
        }
      if (id=="e") {
        especie<-especie[especie$grupo==as.character(gr) & especie$esp==esp,"nombree"]         
        }
      if (id=="a") {
        especie<-especie[especie$grupo==as.character(gr) & especie$esp==esp,"aphia"]         
      }
    }
    if (gr!="9" & esp=="999") {
      if (id=="i") {especie<-paste("All",c("Fish","Crustaceans","Molluscs","Echinoderms","Other Invertebrates")[as.numeric(gr)])}
      else {especie<-paste("Total",c("peces","crustáceos","moluscos","equinodermos","otros invertebrados","otros")[as.numeric(gr)])}
      id<-"e"}
    if (gr=="9" & esp=="999") {
      if (id=="i") {especie<-"All Species"}
      else {especie<-"Total especies"}
      id<-"e"}
  }
  if (length(especie)==0) especie<-c("ERROR CODIGO DESCONOCIDO")
  as.character(especie)
  }
 