#' Información anual sobre cigala por FUs en el Cantábrico y Galicia
#'
#' Función de acceso a datos:
#' Extrae las características de los lances para una campaña determinada y las capturas de cigala por unidad funcional
#'
#' Un problema que ocurre al utilizar el CampR con ficheros dbf de las primeras campañas
#' puede ser que al fichero lanceXXX.dbf le falte algún campo, habitualmente
#' el campo **ESTN** utilizado en las últimas versiones del **CAMP** para ligar lances con las estaciones de CTD.
#' El error usual es **$ operator is invalid for atomic vectors**
#' Si se detecta este error revisar la estructura de lanceXXX.dbf con la de
#' otros ficheros de lances de los últimos años
#'
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Sólo disponible para el Cantábrico "cant", Golfo de Cádiz "arsa", combinados con "dnsred" busca los datos en el servidor de Santander si se han creado las odbcs
#' @param dns de dónde se toman los datos "local" del propio ordenador, "serv" del servidor
#' @param year si T incluye una columna con el año al final de los datos
#' @param plot Saca el gráfico (T) o lo omite para dejar sólo los datos (F)
#' @param es si T letreros en español, si F en inglés (F por defecto)
#' @param plotnep si T presenta todos los lances en los que ha habido cigala en la campaña, además de los lances en cada FU, con o sín captura. Si F sólo saca los lances en cada FU sin marcar si ha habido cigala o no.
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param ICESrectcol Sin efecto: se mantiene por compatibilidad, MapIberia64 no colorea los rectángulos ICES individualmente
#' @param places Si T saca etiquetas de principales ciudades en el mapa, si F se omiten los letreros
#' @param ICESlab Sin efecto: se mantiene por compatibilidad, MapIberia64 no incluye etiquetas por rectángulo ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param FU pinta una o varias unidades funcionales, a elegir FU26, FU25 o FU31 con grueso lwd=2 y color rojo
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos de abundancia de cigala en cada FU con datos de biomasa, numero, desviaciones estándar y número de lances en cada FU.
#' @param bw Si T mapa base en blanco y negro (MapIberia64), si F en color
#' @param graf Si FALSE el gráfico va a pantalla; si es un string guarda como PNG con ese nombre
#' @param xpng Anchura del PNG en píxeles; si NA (por defecto) usa 1200 para zona "cant" y 600 para "arsa"
#' @param ypng Altura del PNG en píxeles; si NA (por defecto) usa 600 para zona "cant" y 600 para "arsa"
#' @param ppng Pointsize del PNG (default 20)
#' @return Produce un gráfico con los lances en los que ha habido cigala en el lance y especialmente los lances en cada FU dentro de Demersales FU25,FU26 y FU31
#' @family mapas
#' @examples
#' \dontrun{
#'   NepFUs.camp64(camp="N20",zona="cant",dns="local",plot=T,plotnep=T)
#'   NepFUs.camp64(camp="N24",zona="cant",dns="local",graf="NepFUs_2024")
#'   }
#' @export
NepFUs.camp64<-function(camp=Nsh[length(Nsh)],zona="cant",dns=c("local","serv"),plot=TRUE,plotnep=TRUE,es=FALSE,ti=TRUE,ICESlab=TRUE,
                      ICESrectcol=2,ICESrect=TRUE,FU=NA,places=TRUE,out.dat=TRUE,bw=FALSE,
                      graf=FALSE,xpng=NA,ypng=NA,ppng=20) {
  if (!substr(zona,1,4) %in% c("cant","arsa")) {stop("Esta función sólo permite sacar resultados DEMERSALES o ARSA *dns* debe ser Xxxx o Xxxxred")}
  if (length(camp)>1) {warning("Si escoge mas de una camp los resultados son para el total de campañas, para ver resultados último año coja sólo ese año")}
  # --- Tamaño del PNG por defecto según zona: cant rectangular, arsa cuadrada ---
  if (is.na(xpng)) xpng <- ifelse(zona=="cant",1200,600)
  if (is.na(ypng)) ypng <- ifelse(zona=="cant",600,600)
  # --- Dispositivo gráfico: PNG si graf es un nombre de fichero, si no pantalla ---
  if (plot) {
    if (!is.logical(graf)) {
      png(filename = paste0(graf, ".png"), width = xpng, height = ypng, pointsize = ppng)
      on.exit(dev.off(), add = TRUE)
    } else {
      if (dev.cur() != 1) graphics.off()
      dev.new(width = xpng/96, height = ypng/96, noRStudioGD = TRUE)
    }
  }
  Nep<-maphist64(2,19,camp,zona,dns,plot=F,out.dat=T)
  Nep$year<-camptoyear(Nep$camp)
  if (zona == "cant") {
    Nep$FU <- NA_integer_
    
    # FU25 North Galicia
    # Se incluye el rectángulo ICES 17E1 (lat 44-44.5) aunque queda al norte
    # del límite ICES oficial (44ºN), por ser misma plataforma con capturas de Nep.
    filtro_FU25 <- Nep$lat > 43 & Nep$long < -8
    Nep_25 <- Nep[filtro_FU25, ]
    Nep$FU[filtro_FU25] <- 25
    
    # FU31 Cantabrian Sea
    filtro_FU31 <- Nep$long > -8 & Nep$long < -2
    Nep_31 <- Nep[filtro_FU31, ]
    Nep$FU[filtro_FU31] <- 31
    
    # FU26 West Galicia
    filtro_FU26 <- Nep$lat > 42 & Nep$lat < 43 & Nep$long < -8.5
    Nep_26 <- Nep[filtro_FU26, ]
    Nep$FU[filtro_FU26] <- 26  
  if (plot) {
    MapIberia64(xlims=c(-10,-1.8),ylims=c(42,44.5),ICESrect=ICESrect,places=places,bw=bw)
    if (ti) {title(main=paste0("Demersales ",ifelse(length(camp)==1,camptoyear(camp),
                                                    paste0(range(camptoyear(camp))[1],"-",range(camptoyear(camp))[2]))),line = 1.5)}
  if (plotnep) {points(lat~long,Nep,subset=numero>0,pch=21,bg="grey",cex=1.5)}
  points(lat~long,Nep_26,pch=21,bg="red")
  points(lat~long,Nep_25,pch=21,bg="green")
  points(lat~long,Nep_31,pch=21,bg="blue")
  if (plotnep){
  legend("bottomright",legend=c("Nep+","FU26","FU25","FU31"),pch=21,
         pt.bg=c("grey","red","green","blue"),inset=c(.02,.02),bg="white",
         ncol = 4)}
  else legend("bottomright",legend=c("FU26","FU25","FU31"),pch=21,
              pt.bg=c("grey","red","green","blue"),inset=c(.02,.02),bg="white",
              ncol =3)
  }
  }
  if (zona=="arsa") {
  if (plot) {
    MapIberia64(xlims=c(-7.7,-6),ylims=c(36,37.3),ICESrect=ICESrect,places=places,bw=bw)
    if (ti) {title(main=paste0("ARSA",ifelse(substr(camp[1],1,1)==1,"Q1 ","Q4 "),
                  ifelse(length(camp)==1,camptoyear(camp),
                  paste(range(camptoyear(camp))[1],range(camptoyear(camp))[2],sep="-"))),line=1.3)}
    if (plotnep) {points(lat~long,Nep,subset=numero>0,pch=21,bg="grey",cex=1.8)
    legend("bottomleft",legend=c("Nep+","FU30"),pch=21,
           pt.bg=c("grey","red"),pt.cex=c(1.8,1.1),inset=c(.03,.05),bg="white",ncol = 2)}
    points(lat~long,Nep,pch=21,cex=1.1,bg="red")
  }
  }
  if (out.dat) {
    if (zona=="cant") {
      FU31.B<-mean((Nep_31$peso.gr+0)/1000)
      FU31.Bsd<-sd(Nep_31$peso.gr/1000)
      FU31.BS<-sum(Nep_31$peso.gr/1000)
      FU31.N<-mean(Nep_31$numero)
      FU31.Nsd<-sd(Nep_31$numero)
      FU31.NS<-sum(Nep_31$numero)
      FU31.L<-length(Nep_31$lan)
      FU26.B<-mean(Nep_26$peso.gr/1000)
      FU26.Bsd<-sd(Nep_26$peso.gr/1000)
      FU26.BS<-sum(Nep_26$peso.gr/1000)
      FU26.N<-mean(Nep_26$numero)
      FU26.Nsd<-sd(Nep_26$numero)
      FU26.NS<-sum(Nep_26$numero)
      FU26.L<-length(Nep_26$lan)
      FU25.B<-mean(Nep_25$peso.gr/1000)
      FU25.Bsd<-sd(Nep_25$peso.gr/1000)
      FU25.BS<-sum(Nep_25$peso.gr/1000)
      FU25.N<-mean(Nep_25$numero)
      FU25.Nsd<-sd(Nep_25$numero)
      FU25.NS<-sum(Nep_25$numero)
      FU25.L<-length(Nep_25$lan)
      datFus<-data.frame(FU31=c(B=FU31.B,Bsd=FU31.Bsd,BS=FU31.BS,N=FU31.N,Nsd=FU31.Nsd,NS=FU31.NS,L=FU31.L),
               FU25=c(B=FU25.B,Bsd=FU25.Bsd,BS=FU25.BS,N=FU25.N,Nsd=FU25.Nsd,NS=FU25.NS,L=FU25.L),
               FU26=c(B=FU26.B,Bsd=FU26.Bsd,BS=FU26.BS,N=FU26.N,Nsd=FU26.Nsd,NS=FU26.NS,L=FU26.L))
    }
    if (zona=="arsa") {
      # Golfo de Cádiz es una única FU (FU30): un resumen por campaña, como en NepFU30.camp64
      datFus<-data.frame(camp=paste(camp,collapse=","),Wgh=sum(Nep$peso.gr/1000),Nb=sum(Nep$numero),
                          MeanWg=mean(Nep$peso.gr/1000),SDwg=sd(Nep$peso.gr/1000),
                          MeanNb=mean(Nep$numero),sdNb=sd(Nep$numero),Nlans=nrow(Nep))
    }
    return(list(lansNep=Nep,datFus=datFus))
  }
  invisible(Nep)
  }

