#' Serie histórica de abundancias estratificadas por edad y sector
#'
#' Recorre una serie de campañas (p.ej. `Nsh`) llamando a [edadsect.camp64()]
#' en cada una y junta los resultados en una única tabla. Equivale al bucle
#' `cbind(edadsect.camp64(1,51,"N83","cant","local")[,6],camp="N83")` para
#' todas las campañas de la serie.
#'
#' @details
#' Si una campaña falla (p.ej. no hay ALK, como en `"N87"`, o hay tallas que no
#' están en la clave) no se detiene el bucle: se avisa con un `warning` y esa
#' campaña sale con `NA` (o se omite si `incl.na=FALSE`).
#'
#' Si se da `file`, el resultado se va guardando en un csv (separador `;` y
#' decimal `,`) tras cada campaña, de forma que si se interrumpe el proceso no
#' se pierde lo ya calculado.
#'
#' @param gr Grupo de la especie: 1 peces (sólo hay claves talla-edad para peces y cigala).
#' @param esp Código de la especie numérico. Sólo admite una especie.
#' @param camps Vector de campañas de la serie histórica (p.ej. `Nsh`, `Psh`).
#' @param zona Origen de las bases de datos: Cantábrico `"cant"`, Porcupine `"porc"`, Golfo de Cádiz `"arsa"`.
#' @param dns Elige de dónde toma los datos, del ordenador `"local"` o del servidor `"serv"`.
#' @param plus Edad plus: todas las edades mayores se suman como edad +.
#' @param sect Columnas de [edadsect.camp64()] que se recuperan: `"total"` (por defecto,
#'   la columna 6 en el Cantábrico), uno o varios sectores (`c("1","2")`), o `NULL` para
#'   todos los sectores y el total.
#' @param excl.sect Sectores excluidos, se pasa a [edadsect.camp64()].
#' @param cor.time Si `TRUE` corrige las abundancias en función de la duración del lance.
#' @param wide Si `TRUE` y sólo se pide una columna en `sect`, devuelve una matriz
#'   edad x campaña en lugar de la tabla larga.
#' @param incl.na Si `TRUE` las campañas que fallan aparecen con `NA`; si `FALSE` se omiten.
#' @param file Ruta de un fichero csv donde ir salvando los resultados campaña a campaña. `NA` no guarda nada.
#'
#' @returns Por defecto un data.frame con columnas `camp`, `year`, `edad` y una columna por
#'   cada elemento de `sect` (abundancia media estratificada). Con `wide=TRUE` una matriz
#'   edad x campaña.
#'
#' @seealso [edadsect.camp64()] para una sola campaña, [edadstr.camp64()] por estrato.
#'
#' @examples
#' \dontrun{
#' # Total de la serie histórica de merluza (esp=50) en el Cantábrico
#' edadsect.camps64(1,50,Nsh,"cant","local")
#' # Bacaladilla (esp=51), todos los sectores, guardando según avanza
#' edadsect.camps64(1,51,Nsh,"cant","local",sect=NULL,file="c:/datos/edadsect_51.csv")
#' # Matriz edad x campaña del total
#' edadsect.camps64(1,51,Nsh[20:40],"cant","local",wide=TRUE)
#' }
#'
#' @family edades
#' @export
edadsect.camps64<-function(gr,esp,camps,zona="cant",dns=c("local","serv"),plus=8,sect="total",
                           excl.sect=NA,cor.time=TRUE,wide=FALSE,incl.na=TRUE,file=NA) {
  if (length(esp)>1) stop("Sólo se puede incluir una especie en esta función")
  dns<-match.arg(dns)
  res<-vector("list",length(camps))
  for (i in seq_along(camps)) {
    message("Procesando ",camps[i]," (",i,"/",length(camps),")")
    dat<-tryCatch(edadsect.camp64(gr,esp,camps[i],zona,dns,plus=plus,excl.sect=excl.sect,cor.time=cor.time),
                  error=function(e) {
                    warning(camps[i],": ",conditionMessage(e),call.=FALSE)
                    NULL
                  })
    if (is.null(dat) || !is.matrix(dat)) {
      if (!is.null(dat)) warning(camps[i],": edadsect.camp64 no devolvió resultados",call.=FALSE)
      if (!incl.na) next
      cols<-if (is.null(sect)) "total" else sect
      dat<-matrix(NA_real_,nrow=plus+1,ncol=length(cols),dimnames=list(0:plus,cols))
    }
    cols<-if (is.null(sect)) colnames(dat) else sect
    falta<-setdiff(cols,colnames(dat))
    if (length(falta)>0) {
      warning(camps[i],": no existen las columnas ",paste(falta,collapse=", "),call.=FALSE)
      dat<-cbind(dat,matrix(NA_real_,nrow(dat),length(falta),dimnames=list(NULL,falta)))
    }
    res[[i]]<-data.frame(camp=camps[i],year=camptoyear(camps[i]),edad=rownames(dat),
                         dat[,cols,drop=FALSE],row.names=NULL,check.names=FALSE)
    if (!is.na(file)) {
      out<-as.data.frame(data.table::rbindlist(res,fill=TRUE))
      utils::write.csv2(out,file,row.names=FALSE)
    }
  }
  out<-as.data.frame(data.table::rbindlist(res,fill=TRUE))
  if (wide) {
    if (length(setdiff(names(out),c("camp","year","edad")))!=1) {
      warning("wide=TRUE sólo funciona con una columna en sect, se devuelve la tabla larga")
      return(out)
    }
    vcol<-setdiff(names(out),c("camp","year","edad"))
    out<-tapply(out[[vcol]],list(edad=factor(out$edad,levels=unique(out$edad)),
                                 camp=factor(out$camp,levels=unique(out$camp))),sum)
  }
  out
}
