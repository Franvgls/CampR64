#' Abundancias en número y peso totales para todas las especies de un grupo en una campaña
#'
#' Función de resultados. Similar a \code{ListFauna.camp64} pero incluye la familia, permite todos los grupos (gr=9), porcentajes y un mínimo de peso
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados 6 para deshechos y otros. 9 incluye todos los grupos a excepción del 6
#' @param camp Campaña de la que se solicitan los resultados un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param kg Si T saca los resultados en Kg, si F en gramos
#' @param dec Número de decimales de los resultados
#' @param percents Si T devuelve los resultados en porcentaje del total
#' @param cor.time si T corrige abundancias a duración de lance de 30 minutos
#' @param excl.sect Excluye los sectores o subsectores dados como caracteres
#' @param minim Excluye las especies con peso menor del valor que se elija, en kg o g según kg (aunque percents=T, los porcentajes se calculan sobre todas las especies), también se puede hacer a posteriori para cualquiera de los campos con `tabla[tabla$NombColumn>XX,]`
#' @return Devuelve un data.frame con valores en orden decreciente de abundancia con columnas gr,esp,especie,familia,camp,weight,number,nlan (número de lances en que aparece cada especie)
#' @seealso \link{ListFauna.camp64} \link{CV.camps64}
#' @examples
#' \dontrun{
#' CVlist.camp64(gr=1,camp="N23",zona="cant",dns="local")
#' CVlist.camp64(gr=9,camp="P23",zona="porc",dns="local",percents=TRUE,minim=1)
#' }
#' @export
CVlist.camp64<- function(gr="1",camp,zona,dns=c("local","serv"),kg=TRUE,dec=3,percents=FALSE,cor.time=TRUE,excl.sect=NA,minim=FALSE) {
  if (length(camp)>1) {stop("seleccionadas más de una campaña, no se pueden sacar resultados de más de una")}
  gr<-as.character(gr)
  listsps<-readCampDBF("fauna",zona,camp,dns)[,c("lance","grupo","esp","peso_gr")]
  espfam<-readCampDBF("especies",zona,camp=NULL,dns)[,c("grupo","esp","especie","familia")]
  listsps$grupo<-as.character(listsps$grupo)
  espfam$grupo<-as.character(espfam$grupo)
  if (gr!="9") {
    listsps<-listsps[listsps$grupo==gr,]
  }
  else {
    listsps<-listsps[listsps$grupo!="6",]
  }
  if (nrow(listsps)==0) stop(paste("No hay especies del grupo",gr,"en la campaña",camp))
  listsps$esp<-as.numeric(listsps$esp)
  espfam$ke<-paste(espfam$grupo,as.numeric(espfam$esp))
  #### nlan: lances (de los que hay en lance.dbf) en los que aparece cada especie
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,incl2=TRUE)
  listsps<-listsps[listsps$lance %in% lan$lance & listsps$peso_gr>0,]
  sps<-unique(listsps[,c("grupo","esp")])
  sps<-sps[order(as.numeric(sps$grupo),sps$esp),]
  ke<-paste(sps$grupo,sps$esp)
  dumb<-data.frame(gr=sps$grupo,esp=sps$esp,especie=espfam$especie[match(ke,espfam$ke)],
                   familia=espfam$familia[match(ke,espfam$ke)],camp=camp,weight=NA,number=NA,
                   nlan=as.vector(tapply(listsps$lance,paste(listsps$grupo,listsps$esp),function(x) length(unique(x)))[ke]),
                   stringsAsFactors=FALSE)
  for (i in 1:nrow(dumb)) {
    temp<-CV.camps64(dumb$gr[i],dumb$esp[i],camp,zona,dns,cor.time=cor.time,kg=kg,dec=dec,excl.sect=excl.sect,verbose=FALSE)
    dumb$weight[i]<-as.numeric(temp$weight)
    dumb$number[i]<-as.numeric(temp$number)
  }
  dumb$especie[is.na(dumb$especie)]<-"ERROR: CODIGO DESCONOCIDO"
  peso<-dumb$weight
  if (percents) {
    dumb$weight<-round(dumb$weight*100/sum(dumb$weight,na.rm=TRUE),dec)
    dumb$number<-round(dumb$number*100/sum(dumb$number,na.rm=TRUE),dec)
  }
  if (!is.logical(minim)) {dumb<-dumb[peso>minim,]}
  dumb<-dumb[order(dumb$weight,decreasing=TRUE),]
  row.names(dumb)<-NULL
  dumb
}
