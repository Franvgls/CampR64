#' Descenso logarítmico de abundancia por cohorte
#'
#' Genera un panel lattice (un subgráfico por cohorte) con el descenso
#' de log(abundancia) a lo largo de la edad, permitiendo estimar la
#' mortalidad total (Z) por cohorte como la pendiente de la regresión.
#'
#' @details
#' Cada panel muestra una cohorte (año de nacimiento = año campaña − edad).
#' La línea roja discontinua es la regresión de toda la serie completa,
#' como referencia. El valor en la esquina superior derecha de cada panel
#' es la pendiente de la regresión de esa cohorte (estimador de −Z).
#'
#' La campaña `"N87"` se trata como `NA` por ausencia de ALK ese año.
#' El layout se calcula automáticamente si no se especifica.
#'
#' @param gr Grupo taxonómico (solo peces — requiere ALK).
#' @param esp Código numérico de especie. Solo admite una especie.
#' @param camps Vector de campañas de la serie histórica (p.ej. `Nsh`).
#' @param zona Zona: `"cant"`, `"porc"`, `"arsa"`, `"medi"`.
#' @param dns Origen de datos: `"local"` o `"serv"`.
#' @param plus Edad plus — edades superiores acumuladas. Por defecto `8`.
#' @param cor.time Si `TRUE` corrige por duración del lance.
#' @param clms Número de columnas del panel lattice.
#' @param layout Vector `c(filas, columnas)` del panel lattice.
#'   Si `NA` (por defecto) se calcula automáticamente.
#'
#' @returns Invisiblemente, imprime la matriz edad × campaña. El gráfico
#'   lattice se produce como efecto lateral.
#'
#' @seealso [bubbage.camp64()] para burbujas de abundancia,
#'   [edadstr.camp64()] para abundancias a la edad por estrato
#'
#' @family edades
#'
#' @examples
#' \dontrun{
#' # Jurel (esp=90) — Cantábrico
#' logabage.camp64(gr=1, esp=90, camps=Nsh, zona="cant", dns="local")
#'
#' # Caballa (esp=74) — Cantábrico
#' logabage.camp64(gr=1, esp=74, camps=Nsh, zona="cant", dns="local")
#'
#' # Gallo whiff (esp=43) — Porcupine
#' logabage.camp64(gr=1, esp=43, camps=Psh, zona="porc", dns="local")
#' }#' @export
#'  
logabage.camp64<-function(gr,esp,camps,zona="cant",dns=c("local","serv"),plus=8,cor.time=TRUE,clms=2,layout=NA) {
  if (length(esp)>1) {
    stop("Sólo se puede incluir una especie en esta función")
  }
  ndat<-length(camps)
  cohts<-data.frame(age=NULL,year=NULL,abund=NULL)
  for (i in 1:length(camps)) {
    if (camps[i]=="N87") cohts<-rbind(cohts,data.frame(age=0:plus,year=1987,abund=rep(NA,plus+1)))
    else {			
      anyo<-ifelse(as.numeric(substr(camps[i],2,3))>50,1900,2000)+as.numeric(substr(camps[i],2,3))
      cohts<-rbind(cohts,data.frame(age=0:plus,year=anyo,abund=edadstr.camp64(gr,esp,camps[i],zona,dns,plus,cor.time=cor.time)$total))
    }
  }
  cohts$cohort<-cohts$year-cohts$age
  lattice::trellis.par.set(lattice::col.whitebg())
  lattice::trellis.par.set("strip.background",list(col=c(gray(.80))))
  ndat<-length(levels(factor(cohts$cohort)))
  orden<-NULL
  ylims<-ceiling(max(abs(log(cohts$abund[which(cohts$abund>0)]))))
  if (any(is.na(layout))) {	
    ndats<-ndat+plus+1
    floorndats<-floor(sqrt(ndats))
    if (ndats-floorndats^2>floorndats) layout<-c(ceiling(sqrt(ndats)),ceiling(sqrt(ndats)))
    else layout<-c(floorndats,floorndats)
  }
  print(lattice::xyplot(log(abund)~age|factor(cohort),cohts,subset=(cohts$abund!=0),
               as.table=FALSE,layout=layout,ylim=c(-ylims,ylims),main=list(label="Abundance along age by cohort",cex=.8),
               xlab=list(label="Age",cex=.7),ylab=list(label="Log(Abundance)",cex=.7),
               par.strip.text=list(cex=.7,font=2),scales=list(alternating=FALSE,tck=c(1,0),cex=.6,
                                                              x=list(at=c(0,seq(1,plus,by=1))),y=list(at=seq(-ylims+1,ylims-1,by=2))),
               panel=function(x,y,...) {
                 #if (length(x)>1) lattice::panel.abline(lm(y~x,na.action=na.omit),col=4,lty=1)
                 lattice::panel.xyplot(x,y,pch=20,col=gray(.3),type="o")
                 lattice::ltext(plus-1,ylims-1,label=round(coef(lm(y~x,na.action=na.omit))[2],2),cex=.7)
                 lattice::panel.abline(lm(log(abund)~age,cohts,subset=(cohts$abund!=0))$coef,col=2,lty=2)
               }))
  print(tapply(cohts$abund,cohts[c(1,2)],sum))	
  #print(tapply(cohts$abund,cohts[c(1,4)],sum))
}
