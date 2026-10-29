#' Capturas y tallas mínima y máxima de uno o varios grupos de especies en una o varias campañas
#'
#' Muestra las capturas totales sin estratificar, en peso y número, el número de lances en que aparece cada especie, el número de ejemplares medidos y las tallas mínima y máxima de todas las especies de uno o varios grupos en todos los lances de una campaña o de una serie de campañas. Sustituye a \code{ListFaunaTals.camp}, \code{ListFaunaTals.camps} y \code{ListFaunaTals.groups} de CampR.
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. 6 desechos y no orgánicos no se puede incluir. Admite varios grupos, i.e. c(1,2), y 9 para todos los grupos del 1 al 5 (como ListFaunaTals.groups)
#' @param camp Campaña o vector de campañas de las que se extrae el listado: un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX". Admite una serie, i.e. Nsh[25:27]
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige si los datos se toman del ordenador ("local") o del servidor ("serv")
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos, NA no excluye ninguno
#' @param incl2 Si T incluye los lances especiales "2"
#' @return Devuelve un data.frame con columnas: camp (sólo si se piden varias campañas),gr,esp,especie,nlans (nº de lances en que aparece la especie),peso (kg),numero,nmed (nº de ejemplares medidos),Lmin,Lmax (tallas mínima y máxima en la unidad en que se mide la especie) y medida (esa unidad: "cm", "mm" o "x5 mm" para las medidas a .5 cm, con las tallas en mm). El peso y el número salen del fichero de capturas, por lo que se incluyen también las especies sin tallas medidas (con nmed=0 y Lmin y Lmax NA). Ordenado por grupo, especie y, con varias campañas, por campaña dentro de cada especie en el orden dado en camp
#' @examples
#' \dontrun{
#' ListFaunaTals.camp64(gr="1",camp="N25",zona="cant",dns="local")
#' ListFaunaTals.camp64(gr="2",camp=Nsh[25:27],zona="cant",dns="local",excl.sect="E")
#' ListFaunaTals.camp64(gr=9,camp="N25",zona="cant",dns="local")
#' }
#' @family ListadosFauna
#' @export
ListFaunaTals.camp64<- function(gr="1",camp,zona,dns=c("local","serv"),excl.sect=NA,incl2=TRUE) {
  if (any(gr==9)) gr<-1:5
  if (any(gr==6)) {stop("No tiene sentido tallas de deshechos")}
  dns<-match.arg(dns)
  if (length(camp)>1) {
    dumb<-do.call(rbind,lapply(camp,function(x) {
      res<-ListFaunaTals.camp64(gr,x,zona,dns,excl.sect=excl.sect,incl2=incl2)
      if (nrow(res)==0) return(NULL)
      cbind(camp=x,res)
    }))
    dumb<-dumb[order(as.numeric(dumb$gr),dumb$especie,match(dumb$camp,camp)),]
    rownames(dumb)<-NULL
    return(dumb)
  }
  if (length(gr)>1) {
    dumb<-do.call(rbind,lapply(gr,function(x) ListFaunaTals.camp64(x,camp,zona,dns,excl.sect=excl.sect,incl2=incl2)))
    dumb<-dumb[order(as.numeric(dumb$gr),dumb$especie),]
    rownames(dumb)<-NULL
    return(dumb)
  }
  lan<-datlan.camp64(camp,zona,dns,redux=TRUE,excl.sect=excl.sect,incl2=incl2,incl0=FALSE)$lance
  if (length(lan)==0) stop(paste("Ningún lance en",camp,"tras la exclusión de sectores",paste(excl.sect,collapse=",")))
  listsps<-readCampDBF("fauna",zona,camp,dns)
  listsps<-listsps[listsps$grupo==gr & listsps$lance %in% lan,]
  if (nrow(listsps)==0) return(data.frame(gr=character(0),esp=numeric(0),especie=character(0),nlans=integer(0),
                                          peso=numeric(0),numero=numeric(0),nmed=numeric(0),Lmin=numeric(0),Lmax=numeric(0),medida=character(0)))
  dumbres<-data.frame(gr=gr,esp=sort(unique(as.numeric(listsps$esp))))
  dumbres$nlans<-as.vector(tapply(listsps$lance,as.numeric(listsps$esp),function(x) length(unique(x)))[as.character(dumbres$esp)])
  dumbres$peso<-as.vector(tapply(listsps$peso_gr,as.numeric(listsps$esp),sum)[as.character(dumbres$esp)])/1000
  dumbres$numero<-as.vector(tapply(listsps$numero,as.numeric(listsps$esp),sum)[as.character(dumbres$esp)])
  talls<-readCampDBF("ntall",zona,camp,dns)
  talls<-talls[talls$grupo==gr & talls$lance %in% lan,]
  dumbres$nmed<-0
  dumbres$Lmin<-NA
  dumbres$Lmax<-NA
  if (nrow(talls)>0) {
    esptal<-as.character(dumbres$esp)
    nmed<-tapply(talls$numer,as.numeric(talls$esp),sum)[esptal]
    dumbres$nmed[!is.na(nmed)]<-nmed[!is.na(nmed)]
    dumbres$Lmin<-as.vector(tapply(talls$talla,as.numeric(talls$esp),min)[esptal])
    dumbres$Lmax<-as.vector(tapply(talls$talla,as.numeric(talls$esp),max)[esptal])
  }
  especies<-readCampDBF("especies",zona,dns=dns)
  especies<-especies[especies$grupo==gr,]
  sps<-match(dumbres$esp,as.numeric(especies$esp))
  dumbres$especie<-especies$especie[sps]
  dumbres$especie[is.na(dumbres$especie)]<-"ERROR CODIGO DESCONOCIDO"
  enmm<-!is.na(sps) & especies$med[sps] %in% "2"
  dumbres$medida<-ifelse(enmm,ifelse(especies$increm[sps] %in% "5","x5 mm","mm"),"cm")
  dumbres<-dumbres[order(dumbres$especie),c("gr","esp","especie","nlans","peso","numero","nmed","Lmin","Lmax","medida")]
  rownames(dumbres)<-NULL
  dumbres
}
