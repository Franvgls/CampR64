#' Información anual sobre cigala para la FU 25 Galicia norte
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
#' @param camp Campaña de la que se extraen los datos: año comcreto (XX): Demersales "NXX"
#' @param zona Sólo disponible para el Cantábrico "cant", combinados con dns "serv" busca los datos en el servidor de Santander si se han creado las odbcs
#' @param dns de dónde se toman los datos, ordenadro ("local") o del servidor ("serv")
#' @param year si T incluye una columna con el año al final de los datos
#' @param plot Saca el gráfico (T) o lo omite para dejar sólo los datos (F)
#' @param es si T letreros en español, si F en inglés (F por defecto)
#' @param plotnep si T presenta todos los lances en los que ha habido cigala en la campaña, además de los lances en cada FU, con o sín captura. Si F sólo saca los lances en cada FU sin marcar si ha habido cigala o no.
#' @param ICESrect Si T saca los rectangulos ices de 1 grado de latitud por medio de longitud
#' @param icesrectcol Color para los rectángulos ICES
#' @param places Si T saca etiquetas de principales ciudades en el mapa, si F se omiten los letreros
#' @param ICESlab Si T incluye las etiquetas de los rectángulos ICES
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param FU pinta los límites de la FU25 en color rojo
#' @param ColFU color de relleno de la FU, blanco por defecto, con dens=0 se omite el relleno.
#' @param dens valores de 20 hace que el color permita ver el fondo con los ICES rects y la batimetría, más lo deja en color sólido, 0 quita el relleno del todo.
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos de abundancia de cigala en cada FU con datos de biomasa, numero, desviaciones estándar y número de lances en cada FU.
#' @return Produce un gráfico con los lances en los que ha habido cigala en el lance en la FU FU25
#' @family mapas, NEP
#' @examples
#'   NepFU25.camp("N21")
#' @export
NepFU25.camp64<-function(camp="NNNNNNNNN23",zona="cant",dns=c("local","serv"),plot=TRUE,es=FALSE,ti=TRUE,
                         ICESlab=FALSE,ceros=T,ICESrectcol=1,ICESrect=TRUE,                       FU=25,places=TRUE,out.dat=TRUE,ColFU="white",dens=20,leg=T,escmult=.25,cexleg=1,cex0=1) {
  xlims <- c(-8.5, -7.9)
  ylims <- c(43, 44.5)
  asp   <- diff(ylims) / (diff(xlims) * cos(mean(ylims) * pi / 180))
  
  # Cerrar cualquier device existente y abrir uno nuevo bien dimensionado.
  # Esto evita el error "plot region too large" que ocurre cuando se intenta
  # reutilizar un device con dimensiones incompatibles con los margenes actuales.
  if (dev.cur() != 1) graphics.off()
  dev.new(width=15, height=15*asp, noRStudioGD=TRUE)
  Nep<-maphist64(2,19,camp,"cant",dns="local",plot=F,out.dat=T)
  Nep_25<-subset(Nep,c(long<c(-7.9) &lat>c(43) & lat<(44.5)))
  leyenda<-signif(max(Nep$numero)*.9,1)
  leyenda<-signif(c(1,.5,.25)*leyenda,1)
  escala<-signif(max(Nep$numero),1)*escmult
  lans_FU25<-subset(datlan.camp64(camp,zona,dns = dns,redux=T,incl2=T,incl0 = F),c(long<c(-8) & lat>c(43) & lat<(44.5)))
  lans_FU25<-lans_FU25[,c("camp","lance","lat","long","prof","sector","StatRec","zona")]
  #lans_FU25.All<-rbind(lans_FU25[lans_FU25$long>c(-10) & lans_FU25$lat<43.5,],lans_FU25[lans_FU25$long>c(-9) & lans_FU25$lat>44.5,])
  MapNort64(ICESrect = ICESrect,ylims=c(42.5,44.5),xlims=c(-10,-7.5),ICESlab = ICESlab,ICESrectcol =ICESrectcol,FU="FU25",ColFU=ColFU,dens=dens,places=places)
  title(main=camptoyear(camp),line=1.5,sub=paste("FU 25 Nep Catch (n)= ",
                                                   sum(Nep_25[Nep_25$camp==camp,"numero"])),cex.sub=1.2,cex.main=1.5)
  points(lat~long,Nep,subset=c(peso.gr>0 & camp==camp),cex=sqrt(Nep$numero/5),pch=21,col=2,bg=2)
  if (leg & max(Nep$numero,na.rm=TRUE)>0) {
    points(rep(-8.5,3),c(43.1,43,42.9),cex=sqrt(leyenda/escala),pch=21,bg="tomato",col="tomato")
    text(rep(-8.5,3),c(43.1,43,42.9),labels=paste(leyenda,"ind."),pos=4,offset=1,cex=cexleg)
  }
  #  legend(x=-8.5,y=c(43,42.5),legend=c(paste(ceiling(max(Nep$numero)),"inds"),adj=c(-.5,0),bty="n",pch=21,pt.bg="grey20",pt.cex=sqrt(ceiling(max(Nep$numero)/5)),text.font=2,cex=.9)
  if (ceros) {points(lat~long,Nep,subset=c(peso.gr==0 & camp==camp),cex=1*cex0,pch=21,col=1,bg=1)
    legend("bottomright",legend=c("0 catch hauls"),pch=21,pt.bg=1,pt.cex=1*cex0,inset=.01,bty="n")}
  if (out.dat) {list(abs=data.frame(camp=camp,Wgh=sum(Nep_25$peso.gr/1000),Nb=sum(Nep_25$numero),MeanWg=mean(Nep_25$peso.gr/1000),
                           SDwg=sd(Nep_25$peso.gr/1000),MeanNb=mean(Nep_25$numero),sdNb=sd(Nep_25$numero),Nlans=nrow(Nep_25)),
                     lans=lans_FU25)
                     }
  }


