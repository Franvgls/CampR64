#' Mapa de índices ecológicos para un grupo en una campaña
#' 
#' Utiliza los datos del Camp representar la variación geográfica de los índices ecológicos
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados. 6 Desechos y otros no se incluye en esta función
#' @param esp ha de ser 999 cuando se quiere incluir todas las especies del grupo, o elegir todas las especies deseadas con los codigos de las especies
#' @param camp Campaña de la que se extraen los datos: un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Zona: `"cant"`, `"porc"`, `"arsa"`, `"medi"`.
#' @param dns Origen de datos: `"local"` o `"serv"`.
#' @param ind Elige el valor (n)úmero o (p)eso sobre el que se calculan los índices de diversidad, dominancia....
#' @param indec Elige el índice ecológico a representar: opciones disponibles: Shannon-Wiener: 'div', Número de especies: 'nesp' y Diversidad Simpson: 'simp'.
#' @param plot Si T saca un gráfico en pantalla
#' @param ti Añade el nombre de la especie en latín sin T, si F no añade titulo
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param es Si T saca los titulos y rotulos en español, si F en inglés
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @return Saca el mapa de diversidad en la campaña seleccionada.
#' @examples
#' dumbecol<-MapEcol.camp(1,999,Nsh[25:30],"Cant",ind="n",bw=TRUE,indec="simp",out.dat=TRUE,layout=c(2,3))
#' dumbecol$estrato<-cut(dumbecol$prof,c(0,70,120,200,500,900),c("A1","A","B","C","D"))
#' lattice::bwplot(numbesp~estrato|camp,dumbecol,horizontal=FALSE,main="Número de especies",xlab="Estrato")
#' lattice::bwplot(div~estrato|camp,dumbecol,horizontal=FALSE,main="Shannon Wiener",xlab="Estrato")
#' @family mapas
#' @family ecologia
#' @export
MapEcol.camp64<-function(gr,esp="999",camp,zona="cant",dns=c("local","serv"),ind="n",indec="div",plot=TRUE,bw=FALSE,
                       ti=TRUE,idi="l",es=TRUE,out.dat=FALSE,layout=NA,cexleg=1,years=TRUE) {
  if (!(indec %in% c("simp","div","nesp"))) {
    stop(paste("el índice",indec,"no está implementado, índices disponibles: 'div', 'nesp' y 'simp'"))
  }
  lattice::trellis.par.set(lattice::col.whitebg())
  ndat<-length(camp)
  dumb<-NULL
  for (i in 1:ndat) {
    dumb<-rbind(dumb,cbind(ecolgr.camp64(gr,esp,camp[i],zona,dns,ind=ind),camp=camp[i]))
  }
  if (years) {
    dumbcamp<-dumb
    dumb$camp<-camptoyear(dumb$camp) 
  }
  dumb$camp<-factor(dumb$camp)
  if (indec=="div") {
    leyenda<-c(0,1.5,2,2.5,5)
    dumb$divF<-cut(dumb$div,leyenda,c("<1.5","1.5-2","2-2.5",">2.5"),right=F)
    dumb$divC<-cut(dumb$div,leyenda,c("yellow","green","lightsalmon","red"),right=F)
    leyenda<-levels(dumb$divF)
#    escala<-signif(max(dumb$div),1)*30/70}
  if (!es) sub<-paste("Shannon Wiener",ifelse(ind=="p","- Biomass","- Number"))
  else sub<-paste("Shannon Wiener",ifelse(ind=="p","- Biomasa","- Número"))
       }
  if (indec=="nesp") {
    leyenda<-c(0,15,20,25,50)
    dumb$nespF<-cut(dumb$numbesp,leyenda,c("<15 spp","15-20 spp","20-25 spp",">25 spp"),right=F)
    dumb$nespC<-cut(dumb$numbesp,leyenda,c("yellow","green","lightsalmon","red"),right=F)
    leyenda<-levels(dumb$nespF)
#    leyenda<-signif(max(dumb$numbesp)*.9,1)
#    escala<-signif(max(dumb$numbesp),1)*30/70
    sub<-ifelse(es,"Número de especies","Species number")
  }
  if (indec=="simp") {
    leyenda<-c(0,0.25,.5,.75,1)
    dumb$simpF<-cut(dumb$simp,leyenda,c("<0.25","0.25-0.50","0.50-0.75",">0.75"),right=T)
    dumb$simpC<-cut(dumb$simp,leyenda,c("yellow","green","lightsalmon","red"),right=T)
    leyenda<-levels(dumb$simpF)
#    leyenda<-signif(max(dumb$simp)*.9,1)
#    escala<-signif(max(dumb$simp),1)*30/70
    if (!es) sub<-paste("Simpson's Diversity",ifelse(ind=="p","- Biomass","- Number"))
    else sub<-paste("Diversidad de Simpson",ifelse(ind=="p","- Biomasa","- Número"))
  }
#  if (indec=="domsimp") {
#    leyenda<-signif(max(dumb$domsimp)*.9,1)
#    escala<-signif(max(dumb$domsimp),1)*1/2
#    if (!es) sub<-paste("Simpson's dominance",ifelse(ind=="p","- Biomass","- Number"))
#    else sub<-paste("Dominancia Simpson",ifelse(ind=="p","- Biomasa","- Número"))
  if (ti) titulo<-list(label=buscaesp64(gr,esp,zona,dns,id=idi),font=ifelse((idi=="l" & gr!="9" & esp!="999"),4,2))
  else titulo<-NULL
  if (bw) {
    lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
    colo=gray(.1)
  }
  else {
    lattice::trellis.par.set(lattice::col.whitebg())
    colo=4
  }
  if (any(is.na(layout))) {
    if (ndat!=4) layout=c(1,ndat)
    if (ndat==4) layout=c(2,2)
  }
  if (zona=="porc") {
    asp<-diff(c(50.5,54.5))/(diff(c(-15.5,-10.5))*cos(mean(c(50.5,54.5))*pi/180))
#    leyenda<-signif(c(1,.5)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-15.5,-10.5),main=titulo,xlab=NULL,ylab=NULL,
                    ylim=c(50.5,54.5),aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
                                                                                                x=list(at=c(-15:-11),labels=as.character(abs(-15:11))),y=list(at=(51:54),rot=90)),as.table=TRUE,sub=sub,
                    panel=function(x,y,subscripts) {
                      lattice::panel.xyplot(Porc.map$x,Porc.map$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Porc.map,"narr",plot=FALSE)[[1]],maps::map(Porc.map,"narr",plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=gray(.7)))
                      lattice::panel.xyplot(c(-12.5,-12.5,-12.5,-12.5),c(51.5,51.3,51.1,50.9),cex=1,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-12.5,4),c(51.5,51.3,51.1,50.9),labels=leyenda,pos=4,offset=1,cex=.8)
                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$divC))}
                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$simpC))}
                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$nespC))}
#                      if (indec=="domsimp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$domsimp[subscripts]>0,sqrt((dumb$domsimp[subscripts])/escala),.35),
#                                                          pch=ifelse(dumb$domsimp[subscripts]>0,16,20),col=colo)}
                    }) }
  if (zona=="cant") {
    asp<-diff(c(41.82,44.3))/(diff(c(-10.25,-1.4))*cos(mean(c(41.82,44.3))*pi/180))
#    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=c(-10.25,-1.4),ylim=c(41.82,44.3),main=titulo,xlab=NULL,ylab=NULL,
                    aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
                                                                                                 x=list(at=c(-10:-2),labels=as.character(abs(-10:-2))),y=list(at=(42:44),rot=90)),as.table=TRUE,sub=sub,
                    panel=function(x,y,subscripts) {
                      lattice::panel.xyplot(Nort.str$x,Nort.str$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Nort.map,"Costa",plot=FALSE)[[1]],maps::map(Nort.map,"Costa",plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=ifelse(bw,gray(.7),"wheat")))
                      lattice::panel.xyplot(rep(-7,4),c(43.,42.80,42.60,42.4),cex=1,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-7,4),c(43.,42.80,42.60,42.4),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$divC))}
                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$simpC))}
                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$nespC))}
#                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$div[subscripts]>0,sqrt((dumb$div[subscripts])/escala),.35),
#                                                      pch=ifelse(dumb$div[subscripts]>0,16,20),col=colo)}
#                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$simp[subscripts]>0,sqrt((dumb$simp[subscripts])/escala),.35),
#                                                       pch=ifelse(dumb$simp[subscripts]>0,16,20),col=colo)}
#                      if (indec=="domsimp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$domsimp[subscripts]>0,sqrt((dumb$domsimp[subscripts])/escala),.35),
#                                                          pch=ifelse(dumb$domsimp[subscripts]>0,16,20),col=colo)}
#                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numbesp[subscripts]>0,sqrt((dumb$numbesp[subscripts])/escala),.35),
#                                                       pch=ifelse(dumb$numbesp[subscripts]>0,16,20),col=colo)}
                    })}
  if (zona=="arsa") {
    asp<-diff(c(35.95,37.30))/(diff(c(-8.1,-5.5))*cos(mean(c(35.95,37.30))*pi/180))
#    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Arsa.map$range[c(1,2)],ylim=Arsa.map$range[c(3,4)],main=titulo,xlab=NULL,ylab=NULL,
                    aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,x=list(at=c(-7:-5),
                    labels=as.character(abs(-7:-5))),y=list(at=(36:37),rot=90)),as.table=TRUE,sub=sub,
                    panel=function(x,y,subscripts) {
                      lattice::panel.xyplot(Arsa.str$x,Arsa.str$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[1]],maps::map(Arsa.map,c("Portugal","Costa"),plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=gray(.7)))
                      lattice::panel.xyplot(rep(-5.9,4),c(36.4,36.5,36.6,36.7),cex=1,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-5.9,4),c(36.4,36.5,36.6,36.7),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$divC))}
                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$simpC))}
                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$nespC))}
#                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$div[subscripts]>0,sqrt((dumb$div[subscripts])/escala),.35),
#                                                      pch=ifelse(dumb$div[subscripts]>0,16,20),col=colo)}
#                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$simp[subscripts]>0,sqrt((dumb$simp[subscripts])/escala),.35),
#                                                       pch=ifelse(dumb$simp[subscripts]>0,16,20),col=colo)}
##                      if (indec=="domsimp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$domsimp[subscripts]>0,sqrt((dumb$domsimp[subscripts])/escala),.35),
##                                                          pch=ifelse(dumb$domsimp[subscripts]>0,16,20),col=colo)}
#                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numbesp[subscripts]>0,sqrt((dumb$numbesp[subscripts])/escala),.35),
#                                                       pch=ifelse(dumb$numbesp[subscripts]>0,16,20),col=colo)}
                    })}
  if (zona=="medit") {
    asp<-diff(c(35,43))/(diff(c(-5.7,5))*cos(mean(c(35,43))*pi/180))
#    leyenda<-signif(c(1,.5,.25)*leyenda,1)
    mapdist<-lattice::xyplot(lat~long|camp,dumb,layout=layout,xlim=Medits.tot$range[c(1,2)],ylim=Medits.tot$range[c(3,4)],main=titulo,xlab=NULL,
                    ylab=NULL,aspect=asp,par.strip.text=list(cex=.9,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.7,
                    x=list(at=c(-5:4),labels=c(paste(as.character(abs(-5:-1)),"W",sep=""),0,paste(1:4,"E",sep=""))),y=list(at=(36:42),rot=90)),as.table=TRUE,sub=sub,
                    panel=function(x,y,subscripts) {
                      lattice::panel.xyplot(Medits.tot$x,Medits.tot$y,type="l",lty=3,col=gray(.2))
                      grid::grid.polygon(maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[1]],maps::map(Medits.tot,Medits.tot$names[],plot=FALSE)[[2]],
                                   default.units = "native",gp=grid::gpar(fill=gray(.8)))
                      lattice::panel.xyplot(rep(-4,4),c(39.1,39.4,39.7,40.0),cex=1,pch=21,col=1,fill=c("yellow","green","lightsalmon","red"))
                      lattice::ltext(rep(-4,4),c(39.1,39.4,39.7,40.0),labels=leyenda,pos=4,offset=1.1,cex=.7)
                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$divC))}
                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$simpC))}
                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=1,pch=21,col=1,fill=as.character(dumb$nespC))}
#                      if (indec=="div") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$div[subscripts]>0,sqrt((dumb$div[subscripts])/escala),.35),
#                                                      pch=ifelse(dumb$div[subscripts]>0,16,20),col=colo)}
#                      if (indec=="simp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$simp[subscripts]>0,sqrt((dumb$simp[subscripts])/escala),.35),
#                                                       pch=ifelse(dumb$simp[subscripts]>0,16,20),col=colo)}
#                      if (indec=="domsimp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$domsimp[subscripts]>0,sqrt((dumb$domsimp[subscripts])/escala),.35),
#                                                          pch=ifelse(dumb$domsimp[subscripts]>0,16,20),col=colo)}
#                      if (indec=="nesp") {lattice::panel.xyplot(x,y,cex=ifelse(dumb$numbesp[subscripts]>0,sqrt((dumb$numbesp[subscripts])/escala),.35),
#                                                       pch=ifelse(dumb$numbesp[subscripts]>0,16,20),col=colo)}
                    })}
  if (plot) {print(mapdist)}
  if (out.dat) dumb
  else mapdist
}
