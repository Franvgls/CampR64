#' Crea un histograma distribución de tallas estratificada especie y campañas corrigiendo el que haya unas campañas a .5 y otras a 1 cm
#'
#'  Histograma de la distribución de tallas media estratificada por sexos a partir de ficheros del camp. Similar a \code{dtall.camp64} pero con la opción \code{cor.5}
#'  para homogeneizar series en las que unas campañas se midieron a 1 cm y otras a .5 cm (tallas en mm)
#' @param gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados
#' @param esp Código de la especie numérico o carácter. 999 para todas las especies del grupo. Si se saca más de una especie comprueba que todas estén medidas en las mismas unidades, y saca títulos y datos como si todas tuvieran las propiedades de la primera.
#' @param camp Campaña a representar en el mapa de un año comcreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"
#' @param zona Elige el origen de las bases de datos: Porcupine "porc", Cantábrico "cant", Golfo de Cádiz "arsa"
#' @param dns Elige origen datos ordenador "local" o del servidor "serv"
#' @param cor.time Si T corrige las abundancias en función de la duración del lance
#' @param cor.5 Si T pasa las distribuciones de tallas a .5 cm (en mm) a 1 cm. Se aplica campaña a campaña: sólo se convierten las campañas cuyo rango de tallas supera 150 (i.e. medidas en mm), de modo que distingue si una campaña es a .5 y otra a 1 cm
#' @param ti Si T añade título al gráfico, el nombre de la especie en latín
#' @param sub Si caracter lo añade como subtítulo en el gráfico
#' @param leg Si T añade leyenda
#' @param cexleg Varía el tamaño de letra de los ejes y del número de la leyenda
#' @param bw Gráfico en blanco en negro si T o en color si F
#' @param es Si T gráfico en castellano, si F gráfico en inglés
#' @param sex Sacar la distribución de tallas por sexos si existen (T), o conjunta (F)
#' @param plot Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)
#' @param idi Nombre científico de la especie ("l") o nombre común ("e")
#' @param clms Número de columnas para ordenar la serie histórica
#' @param layout Organización de gráficos en filas ó columnas c(r,c)
#' @param excl.sect Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos
#' @param ymax Valor máximo del eje y
#' @param out.dat Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto
#' @param years Si T saca los años como nombre de campaña en los paneles lattice de campañas
#' @param verbose Si T saca avisos de consistencia en tallas, sino los omite
#' @return Si plot=T saca el gráfico, pero si out.dat=T puede exportar una matriz talla(filas)xCampañas(columnas)
#' @seealso \link{dtall.camp64} \link{dattal.camp64}
#' @examples
#' \dontrun{
#' # Capros aper: medido a 1 cm en 2013 (N13) y a .5 cm en 2025 (N25)
#' dtall05.camp64(1,18,Nsh[c(30,42)],"cant","local",cor.5=TRUE)
#' dtall05.camp64(1,18,Nsh[c(30,42)],"cant","local",cor.5=FALSE)
#' dtall05.camp64(1,18,Nsh[c(30,42)],"cant","local",cor.5=TRUE,plot=FALSE,out.dat=TRUE)
#' # Sardina pilchardus en ARSA otoño 2014: medida a .5 cm en la base de datos
#' dtall05.camp64(1,72,214,"arsa","local",cor.5=FALSE,sub="ARSA Q4-2014",ti=TRUE)
#' dtall05.camp64(1,72,214,"arsa","local",cor.5=TRUE,sub="ARSA Q4-2014",ti=TRUE)
#' }
#' @export
dtall05.camp64<- function(gr,esp,camp,zona,dns=c("local","serv"),cor.time=TRUE,cor.5=FALSE,ti=FALSE,sub=NA,leg=TRUE,cexleg=1,bw=TRUE,es=TRUE,sex=FALSE,plot=T,idi="l",clms=2,
                       layout=NA,excl.sect=NA,ymax=NA,out.dat=FALSE,years=TRUE,verbose=TRUE) {
  options(scipen=2)
  if (length(gr)>1) stop("No tiene sentido mezclar distribuciones de tallas de especies en distintos clases taxonómicas")
  esp<-as.character(esp)
  #### unidades de medida de la(s) especie(s)
  if (any(esp=="999")) {
    especies<-readCampDBF("especies",zona,camp=NULL,dns)
    unids<-unique(especies[especies$grupo==as.character(gr),c("med","increm")])
  }
  else {
    unids<-do.call(rbind,lapply(esp,function(x) unid64.camp(gr,x,zona,dns)))
  }
  if (nrow(unids)==0) stop("No se encuentran las unidades de medida de la(s) especie(s) en especies.dbf")
  medidas<-ifelse(unids$med==1,"cm",ifelse(unids$increm==5,"x5 mm","mm"))
  if (length(unique(unids$increm))>1 | length(unique(medidas))>1) {
    if (any(esp=="999")) {
      if (verbose) message("Las especies del grupo están medidas en distintas unidades (mm y cm o .5 cm), se usan las de la primera")
    }
    else stop("Seleccionadas especies medidas en distintas unidades (mm y cm o .5 cm) o a la aleta anal")
  }
  increm<-as.numeric(unids$increm[1])
  medida<-medidas[1]
  if (bw) {
    colbars<-c(gray(.5),gray(.8),"white")
  }
  else {
    colbars<-c("lightyellow","steelblue","yellow1")
  }
  if (es) {sxn<-c("Indet","Machos","Hembras")
  ax<-c(paste0("Talla (",medida,")"),expression("Ind"%*%"lan"^-1))}
  else {sxn<-c("Undet","Male","Female")
  ax<-c(paste0("Length (",medida,")"),expression("Ind"%*%"haul"^-1))}
  sexn<-c("2","3","1")
  sixn<-c("1","2","3")
  dtalln<-c("machos","hembras","indet")
  if (is.logical(ti)) {
    if (ti) {tit<-list(label=buscaesp64(gr,esp,zona,dns,id=idi),font=ifelse(idi=="l",4,2),cex=1*cexleg)}
    else {tit<-NULL}
  }
  else {
    if(is.list(ti)) tit<-ti
    else tit<-list(label=ti)
  }
  ndat<-length(camp)
  for (i in 1:ndat) {
    dtall<-dattal.camp64(gr,esp,camp[i],zona,dns,cor.time=cor.time,excl.sect=excl.sect,sex=sex,verbose=verbose)
    if (!sex) names(dtall)[names(dtall)=="numero"]<-"indet"
    #### sección para corregir datos de tallas a .5 cm (en mm) dejándolos a 1 cm
    if (cor.5 & diff(range(dtall$talla))>150) {
      dtall$talla<-trunc(dtall$talla/10)
      dtall<-aggregate(.~talla,dtall,sum)
      increm<-1
      medida<-"cm"
      if (es) {ax<-c(paste0("Talla (",medida,")"),expression("Ind"%*%"lan"^-1))}
      else {ax<-c(paste0("Length (",medida,")"),expression("Ind"%*%"haul"^-1))}
    }
    names(dtall)<-c("talla",sexn[which(!is.na(match(dtalln,names(dtall)[2:ncol(dtall)])))])
    sxs<- match(sixn,names(dtall)[2:length(names(dtall))])
    if (sex) {
      ard<-c(NULL,NULL,NULL,NULL)
      if (!is.na(sxs[2])) a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,match("2",names(dtall))],rep(2,nrow(dtall)))
      else a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),rep(0,nrow(dtall)),rep(2,nrow(dtall)))
      ard<-rbind(ard,a1)
      if (!is.na(sxs[3])) a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,match("3",names(dtall))],rep(3,nrow(dtall)))
      else a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),rep(0,nrow(dtall)),rep(3,nrow(dtall)))
      ard<-rbind(ard,a1)
      if (!is.na(sxs[1])) a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,match("1",names(dtall))],rep(1,nrow(dtall)))
      else a1<-cbind(dtall[,1],rep(camp[i],nrow(dtall)),rep(0,nrow(dtall)),rep(1,nrow(dtall)))
      ard<-rbind(ard,a1)
    }
    else {
      if (ncol(dtall)>2) {
        ard<-as.data.frame(cbind(dtall[,1],rep(camp[i],nrow(dtall)),rowSums(dtall[,c(2:ncol(dtall))]),rep(1,nrow(dtall))))
      }
      else ard<-as.data.frame(cbind(dtall[,1],rep(camp[i],nrow(dtall)),dtall[,2],rep(1,nrow(dtall))))
    }
    if (i==1) a<-ard
    else a<-rbind(a,ard)
  }
  a<-as.data.frame(a)
  names(a)<-c("talla","camp","n","sex")
  if (years) {
    acamp<-a
    acamps<-camp
    camp<-camptoyear(camp)
    a$camp<-camptoyear(a$camp)
  }
  a$camp<-factor(as.character(a$camp),levels=camp)
  a$talla<-as.numeric(as.character(a$talla))
  a$n<-as.numeric(as.character(a$n))
  if (sum(a$n)==0) {
    message(paste0(ifelse(es,"No hay capturas de la especie ","No catches of species "),buscaesp64(gr,esp,zona,dns),ifelse(es," en las campañas seleccionadas"," in surveys selected")))
    return(invisible(NULL))
  }
  a$sex<-factor(as.character(a$sex),levels=c(1:3))
  maxy<-tapply(a$n,a[,c(1,2)],sum)
  maxy[which(is.na(maxy))]<-0
  ylim<-c(0,ifelse(is.na(ymax),max(maxy)*1.05,ymax))
  haysex<-sum(tapply(a$n,a$sex,sum)[c(2,3)],na.rm=TRUE)
  if (sex & (haysex != 0)) {
    sxn<-sxn[(sxs)]
    colbars<-colbars[which(!is.na(sxs))]
    a$sex<-factor((a$sex),exclude=sixn[which(is.na(sxs))])
  }
  else {
    if (bw) colbars<-gray(.9)
    else colbars<-"steelblue"
    leg=F
  }
  if (leg & (haysex != 0)) {
    leg<-list(columns=3,space="top",rectangles=list(T,size=5),
              col=colbars[c(2,3,1)],text=list(labels=sxn[c(3,1,2)],col="black",
                                              cex=cexleg*ifelse(!plot,.7,.9)))}
  else {leg<-NULL}
  xlimi<-c(min(a$talla)*(.95-1),max(a$talla)*1.05)
  if (is.character(sub)) sub=list(label=sub,font=2,cex=cexleg*.9)
  if (length(camp)==1) {
    foo<-lattice::barchart(n~talla,a,groups=a$sex,subscripts=T,key=leg,box.ratio=1000,box.width=increm,ylim=ylim,xlim=xlimi,as.table=T,
                           scales=list(alternating=F,tck=c(1,1),
                                       x=list(at= ticks.tal(a$talla),
                                              rot=45)),
                           stack=T,h=F,main=tit,par.strip.text=list(cex=cexleg*.8,font=2),
                           xlab=list(label=ax[1],cex=cexleg*1.2),ylab=list(label=ax[2],cex=cexleg*1.2),sub=sub,strip=TRUE,
                           panel=function(x,y,...) {lattice::panel.fill(col="white")
                             lattice::panel.grid(-1,0,lty=3,col=gray(.2))
                             lattice::panel.barchart(x,y,col=colbars,...)
                           }
    )
  }
  else {
    orden=NULL
    if (any(is.na(layout))) {
      if (ndat>3) layout<-c(clms,ceiling(ndat/clms))
      else {layout<-c(1,ndat)}
    }
    foo<-lattice::barchart(n~talla|camp,a,groups=a$sex,subscripts=T,key=leg,box.ratio=1000,box.width=increm,ylim=ylim,xlim=xlimi,col=colbars,drop.unused.levels=FALSE,
                           scales=list(alternating=F,tck=c(1,0),cex=cexleg*.7,x=list(at=ticks.tal(a$talla),
                                                                                     rot=45)),stack=T,h=F,main=tit,xlab=list(label=ax[1],cex=cexleg*.9),
                           ylab=list(label=ax[2],cex=cexleg*.9),layout=layout,par.strip.text=list(cex=cexleg*.8,font=2),as.table=ifelse(!is.null(orden),F,T),sub=sub,
                           panel=function(x,y,...) {
                             lattice::panel.fill(col="white")
                             lattice::panel.grid(-1,0,lty=3,col="gray60")
                             lattice::panel.barchart(x,y,...)
                           },
                           strip = function(...) lattice::strip.default(style=1,...))
  }
  if (plot) {
    if (bw) {
      lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
    }
    else {
      lattice::trellis.par.set(lattice::col.whitebg())
    }
    print(foo)
  }
  if (out.dat) {
    if (years) {
      a<-acamp
      a$n<-as.numeric(as.character(a$n))
      a$talla<-as.numeric(as.character(a$talla))
      camp<-acamps
    }
    tapply0<-tapply(a$n,a[,1:2],sum,na.rm=T)
    tapply0[is.na(tapply0)]<-0
    print(tapply0)
  }
  else {
    if (!plot) foo
  }
}
