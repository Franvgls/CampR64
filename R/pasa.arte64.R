#' Geometría del arte por lance a partir del registro del PescaWin
#'
#' Función auxiliar para el PescaWin:
#' Lee el fichero de arte de una campaña (Arte_XXX.csv, registro de los sensores
#' del arte en el PescaWin), saca los gráficos de abertura vertical, distancia
#' entre puertas y abertura de calones por lance a lo largo del arrastre y
#' calcula las medianas de cada lance con los registros válidos (`Val==1`) en
#' FIRME, para pasarlas a los campos dista_p, abert_h y abert_v del lanceXXX.dbf.
#' Antes `pasa.arte` en CampR.
#' @param camp Campaña de la que se extraen los datos: año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el directorio de marcas del PescaWin si `folder=NA`: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige la raíz del PescaWin si `folder=NA`: "local" "c:/gps/", "serv" "z:/gps/". Se pueden cambiar con `options(CampR64.gps.local=...)` y `options(CampR64.gps.serv=...)`
#' @param lan Lance o lances que se quieren revisar, si se deja en NA se toman todos los lances del fichero
#' @param FileArte Prefijo del fichero de arte, el fichero leído es `paste0(folder,FileArte,camp,".csv")`
#' @param folder Directorio del fichero de arte. Si NA la raíz según `dns` más "mrk/norte/", "mrk/porcupine/" o "mrk/arsa/" según `zona`
#' @param gps Si T guarda las medianas por lance como lanXXX.csv en `folder`
#' @param graphs Si T saca los gráficos de control por lance: un dispositivo por variable si hay varios lances, los tres juntos si sólo hay uno
#' @param nmin Número mínimo de registros válidos en FIRME para dar las medianas de un lance
#' @return Devuelve (invisible) un data.frame con lance, dista_p (distancia entre puertas), abert_h (abertura de calones), abert_v (abertura vertical) en metros y n (registros válidos usados, tengan o no dato de los sensores). Los valores que no se han podido calcular son NA
#' @note El PescaWin registra un 0 cuando no recibe dato de un sensor, y estos ceros se tratan como dato ausente. Si un sensor no da dato en un lance, su mediana es NA y su panel sale vacío (lo que ocurre con los calones de los lances 1, 3, 4 y 5 de P26). Si son los tres sensores (lances 14 a 25 de P26), el lance sigue en la salida con NA y se avisa con un mensaje. Si un sensor no tiene datos en ninguno de los lances seleccionados, su gráfico no se saca y también se avisa.
#' @details En los gráficos los puntos rellenos son registros válidos (`Val==1`) y los huecos no válidos; la línea azul continua es la mediana de los válidos, la roja el suavizado loess y la azul de puntos en la abertura vertical el primer minuto en que el arte baja de 1.1 veces la mediana (estimación de llegada al fondo).
#' @seealso \link{pasa.lan64}, \link{ArtePars64}
#' @examples
#' \dontrun{
#' pasa.arte64("N25","cant")
#' pasa.arte64("N25","cant","local",lan=c(16,17))
#' }
#' @family PescaWin
#' @export
pasa.arte64<-function(camp,zona="cant",dns=c("local","serv"),lan=NA,FileArte="Arte_",folder=NA,gps=FALSE,graphs=TRUE,nmin=3) {
  if (length(camp)>1) stop("Seleccionada más de una campaña, sólo se puede pasar una")
  dns<-match.arg(dns)
  if (is.na(folder)) {
    # Raíz del PescaWin en el ordenador o en el servidor, sin pasar por CampR64_paths
    raiz<-getOption(paste0("CampR64.gps.",dns),ifelse(dns=="local","c:/gps/","z:/gps/"))
    folder<-switch(tolower(substr(zona,1,4)),
                   cant="mrk/norte/",
                   porc="mrk/porcupine/",
                   arsa="mrk/arsa/",
                   stop("zona debe ser 'cant', 'porc' o 'arsa', o indicar folder"))
    folder<-paste0(sub("[/\\\\]+$","",raiz),"/",folder)
  }
  fich<-paste0(folder,FileArte,camp,".csv")
  if (!file.exists(fich)) stop("No existe el fichero ",fich)
  # Todo como texto: el PescaWin repite la cabecera en cada lance
  DatArte<-utils::read.csv(fich,colClasses="character",check.names=TRUE,strip.white=TRUE)
  DatArte<-DatArte[DatArte$Maniobra=="FIRME",]
  for (i in c("Lance","Puertas","Calones","Vertical","Esp.libre","Val")) {
    DatArte[[i]]<-suppressWarnings(as.numeric(DatArte[[i]]))
  }
  DatArte<-DatArte[!is.na(DatArte$Lance),]
  if (!any(is.na(lan))) DatArte<-DatArte[DatArte$Lance %in% lan,]
  if (nrow(DatArte)==0) stop("Los lances seleccionados no existen o no tienen datos en FIRME")
  # Tiempo de arrastre: m:ss, mm:ss o mm:ss:00 (exportado por Excel pasados 24'), a minutos decimales
  tt<-strsplit(DatArte$Tiempo,":",fixed=TRUE)
  DatArte$t.arr<-vapply(tt,function(x) as.numeric(x[1])+as.numeric(x[2])/60,numeric(1))
  # Ceros son sensores sin dato; la vertical con espacio libre en los primeros 6' es el arte aún bajando
  DatArte$Puertas[which(DatArte$Puertas==0)]<-NA
  DatArte$Calones[which(DatArte$Calones==0)]<-NA
  DatArte$Vertical[which(DatArte$Vertical==0)]<-NA
  DatArte$Vertical[which(DatArte$Esp.libre>0 & DatArte$t.arr<6)]<-NA
  DatArte$Val[is.na(DatArte$Val)]<-0
  if (graphs) {
    grafArte<-function(var,titulo,ylim,ytxt,span,dec) {
      dat<-DatArte[!is.na(DatArte[[var]]) & DatArte$t.arr<40,]
      if (nrow(dat)==0) return(NULL)
      dat$val<-dat$Val==1
      # Niveles de todos los lances: los que no tienen dato salen como panel vacío
      dat$lance<-factor(dat$Lance,levels=sort(unique(DatArte$Lance)))
      lattice::xyplot(stats::as.formula(paste(var,"~t.arr|lance")),dat,groups=dat$val,
                      as.table=TRUE,ylim=ylim,drop.unused.levels=FALSE,par.settings=lattice::col.whitebg(),
                      main=list(label=titulo,cex=1),xlab=list(label="Trawling time (min.)",cex=.7),
                      ylab=list(label="Meters",cex=.7),par.strip.text=list(cex=.7,font=2),
                      scales=list(alternating=FALSE,tck=c(1,0),cex=.6,x=list(at=c(0,seq(5,25,by=10)))),
                      panel=function(x,y,subscripts,groups,...) {
                        ok<-groups[subscripts]
                        med<-if (any(ok)) stats::median(y[ok]) else NA
                        lattice::panel.grid(h=-1,v=0,col=grDevices::gray(.6),lty=3)
                        if (!is.na(med)) lattice::panel.abline(h=med,col=4,lty=1)
                        if (var=="Vertical" && !is.na(med) && any(ok & y<=med*1.1))
                          lattice::panel.abline(v=trunc(min(x[ok & y<=med*1.1])),col=4,lty=3)
                        lattice::panel.xyplot(x[!ok],y[!ok],pch=1,cex=.6,col=grDevices::gray(.5))
                        lattice::panel.xyplot(x[ok],y[ok],pch=20,col=grDevices::gray(.2))
                        lattice::ltext(20,ytxt,label=ifelse(is.na(med),"NA",paste("med=",round(med,dec))),cex=.7)
                        if (sum(ok)>8) suppressWarnings(lattice::panel.loess(x[ok],y[ok],span=span,col=2,family="gaussian"))
                      })
    }
    grafs<-list(Vertical=grafArte("Vertical","Vertical opening",c(0,9.2),8.5,.5,1),
                Puertas=grafArte("Puertas","Door spread",c(0,170),20,.3,0),
                Calones=grafArte("Calones","Wing spread",c(0,50),5,.3,0))
    sindat<-names(grafs)[vapply(grafs,is.null,logical(1))]
    if (length(sindat)>0) message("Sin datos para graficar (sensor a cero o sin registros): ",paste(sindat,collapse=", "))
    grafs<-Filter(Negate(is.null),grafs)
    if (length(unique(DatArte$Lance))>1) {
      for (g in grafs) {grDevices::dev.new(); print(g)}
    }
    else if (length(grafs)>0) {
      if (grDevices::dev.cur()==1) grDevices::dev.new()
      for (i in seq_along(grafs)) print(grafs[[i]],split=c(i,1,length(grafs),1),more=i<length(grafs))
    }
  }
  # Medianas por lance con los registros válidos en FIRME
  val<-DatArte[DatArte$Val==1,]
  if (nrow(val)==0) stop("Los lances seleccionados no tienen registros válidos (Val==1)")
  med<-function(x) if (all(is.na(x))) NA_real_ else round(stats::median(x,na.rm=TRUE),1)
  dumb<-do.call(rbind,lapply(split(val,val$Lance),function(d)
    data.frame(lance=d$Lance[1],dista_p=med(d$Puertas),abert_h=med(d$Calones),
               abert_v=med(d$Vertical),n=nrow(d))))
  dumb<-dumb[dumb$n>=nmin,]
  rownames(dumb)<-NULL
  sinval<-setdiff(unique(DatArte$Lance),dumb$lance)
  if (length(sinval)>0) message("Lances sin ",nmin," registros válidos: ",paste(sinval,collapse=", "))
  sinsens<-dumb$lance[is.na(dumb$dista_p) & is.na(dumb$abert_h) & is.na(dumb$abert_v)]
  if (length(sinsens)>0) message("Lances con los tres sensores a cero (salen con NA): ",paste(sinsens,collapse=", "))
  print(dumb)
  if (gps) {
    fsal<-paste0(folder,"lan",camp,".csv")
    utils::write.csv(dumb,fsal,row.names=FALSE)
    message("Guardado ",fsal)
  }
  invisible(dumb)
}
