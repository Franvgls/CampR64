#' Comparación de parámetros del arte entre CAMP IEO y DATRAS
#'
#' @description
#' Crea gráficos comparando los datos del comportamiento del arte en los lances de una campaña
#' con la profundidad, entre los datos del CAMP IEO y los datos de DATRAS.
#' Sustituye a ArteParCompC, ArteParCompD y ArteParCompV.
#'
#' @param camp Campaña: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX", Arsa otoño "2XX"
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param var Variable a comparar: "calones" (wing spread), "puertas" (door spread) o "vertical" (net opening)
#' @param incl2 Si TRUE incluye lances especiales (default TRUE)
#' @param bw Si TRUE gráfico en blanco y negro, si FALSE en color (default TRUE)
#' @param ti Si TRUE añade título a cada panel (default TRUE)
#' @param sub Si TRUE añade subtítulo con la campaña y trimestre (default TRUE)
#' @param out.dat Si TRUE devuelve el merge de datos CAMP vs DATRAS (default FALSE)
#' @param es Si TRUE rótulos en español, si FALSE en inglés (default TRUE)
#' @param profrange Rango de profundidad c(min,max) para filtrar; NA no filtra (default NA)
#' @param Nlans Si TRUE muestra números de lance en vez de puntos (default TRUE)
#' @param lan.cex Tamaño de las etiquetas de número de lance (default 0.8)
#' @param cex.leg Tamaño de letra de los ejes (default 1.1)
#' @param graf Si FALSE el gráfico va a pantalla; si es un string guarda como PNG con ese nombre
#' @param xpng Anchura del PNG en píxeles (default 1200)
#' @param ypng Altura del PNG en píxeles (default 600)
#' @param ppng Pointsize del PNG (default 15)
#' @return Si out.dat=TRUE devuelve un data.frame con los datos CAMP e IEO fusionados por lance
#' @examples
#' ArteParComp64("N23", zona="cant", var="calones", Nlans=FALSE)
#' ArteParComp64("N23", zona="cant", var="puertas", Nlans=TRUE, lan.cex=2)
#' ArteParComp64("P25", zona="porc", var="vertical")
#' @family gear
#' @export
ArteParComp64 <- function(camp, zona = "cant", dns = c("local","serv"),
                           var = c("calones","puertas","vertical"),
                           incl2 = TRUE, es = TRUE, bw = TRUE,
                           ti = TRUE, sub = TRUE, out.dat = FALSE,
                           profrange = NA, Nlans = TRUE, lan.cex = 0.8,
                           cex.leg = 1.1,
                           graf = FALSE, xpng = 1200, ypng = 600, ppng = 15) {
  options(scipen = 2)
  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)
  var  <- match.arg(var)

  # ── 1. Datos CAMP IEO ──────────────────────────────────────────────────────
  lan <- datlan.camp64(camp = camp, zona = zona, dns = dns,
                       incl2 = incl2, redux = TRUE)

  # ── 2. Datos DATRAS ────────────────────────────────────────────────────────
  survey  <- switch(zona,
    cant = "SP-NORTH",
    porc = "SP-PORC",
    arsa = "SP-ARSA",
    stop("zona debe ser 'cant', 'porc' o 'arsa'.")
  )
  quarter <- switch(zona,
    cant = 4L,
    porc = 3L,
    arsa = ifelse(substr(camp, 1, 1) == "1", 1L, 4L)
  )
  landatr <- icesDatras::getHHdata(survey, camptoyear(camp), quarter)
  landatr <- dplyr::filter(landatr, HaulVal != "I")

  # ── 3. Filtro profundidad ──────────────────────────────────────────────────
  if (any(!is.na(profrange))) {
    lan     <- dplyr::filter(lan,     prof  > min(profrange) & prof  < max(profrange))
    landatr <- dplyr::filter(landatr, Depth > min(profrange) & Depth < max(profrange))
  }

  # ── 4. Variables según parámetro var ──────────────────────────────────────
  cfg <- switch(var,
    calones  = list(
      camp_var  = "abert_h",
      datr_var  = "WingSpread",
      datr_filt = quote(Netopening > 0),
      label_es  = "Abertura calones",
      label_en  = "Wing spread",
      titulo_es = "Abertura calones con profundidad",
      titulo_en = "Wing spread vs. depth"
    ),
    puertas  = list(
      camp_var  = "dista_p",
      datr_var  = "DoorSpread",
      datr_filt = quote(DoorSpread > 0),
      label_es  = "Distancia puertas",
      label_en  = "Door spread",
      titulo_es = "Distancia puertas con profundidad",
      titulo_en = "Door spread vs. depth"
    ),
    vertical = list(
      camp_var  = "abert_v",
      datr_var  = "Netopening",
      datr_filt = quote(Netopening > 0),
      label_es  = "Abertura vertical",
      label_en  = "Net opening",
      titulo_es = "Abertura vertical con profundidad",
      titulo_en = "Net opening vs. depth"
    )
  )

  ylab_txt  <- paste(ifelse(es, cfg$label_es,  cfg$label_en),  "(m)")
  xlab_txt  <- paste(ifelse(es, "Prof.", "Depth"), "(m)")
  title_txt <- ifelse(es, cfg$titulo_es, cfg$titulo_en)

  # ── 5. Apertura dispositivo gráfico ───────────────────────────────────────
  if (!is.logical(graf)) {
    png(filename = paste0(graf, ".png"),
        width = xpng, height = ypng, pointsize = ppng)
    on.exit(dev.off(), add = TRUE)
  }

  par(mfcol = c(1, 2), mar = c(4, 4, 3, 1) + 0.1)

  # ── 6. Panel izquierdo: CAMP IEO ──────────────────────────────────────────
  cv <- lan[[cfg$camp_var]]
  pv <- lan$prof
  plot(cv ~ pv,
       pch  = ifelse(Nlans, NA, 21),
       ylim = c(0, max(cv, na.rm = TRUE) * 1.1),
       xlim = c(min(pv, na.rm = TRUE), max(pv, na.rm = TRUE) * 1.1),
       xlab = xlab_txt, ylab = ylab_txt,
       cex.axis = cex.leg, cex.lab = cex.leg)
  if (Nlans)
    text(cv ~ pv, labels = lan$lance, cex = cex.leg, font = 2, pos = 4)
  if (ti)
    title(paste(title_txt, "CAMP IEO"))
  if (sub)
    mtext(paste0(survey, " Q", lan$quarter[1]), side = 3, adj = 1,
          font = 1, cex = 0.8)

  # ── 7. Panel derecho: DATRAS ──────────────────────────────────────────────
  landatr_f <- subset(landatr, eval(cfg$datr_filt))
  dv <- landatr_f[[cfg$datr_var]]
  dp <- landatr_f$Depth
  plot(dv ~ dp,
       pch  = ifelse(Nlans, NA, 21),
       ylim = c(0, max(dv, na.rm = TRUE) * 1.1),
       xlim = c(min(dp, na.rm = TRUE), max(dp, na.rm = TRUE) * 1.1),
       xlab = xlab_txt, ylab = ylab_txt,
       cex.axis = cex.leg, cex.lab = cex.leg)
  if (Nlans)
    text(dv ~ dp, labels = landatr_f$HaulNo, cex = cex.leg, font = 2, pos = 4)
  if (ti)
    title(paste(title_txt, "DATRAS"))
  if (sub)
    mtext(paste0(landatr$Survey[1], " Q", landatr$Quarter[1]),
          side = 3, adj = 1, font = 1, cex = 0.8)

  # ── 8. Datos de salida ────────────────────────────────────────────────────
  if (out.dat) {
    result <- merge(
      lan[, c("lance", cfg$camp_var)],
      landatr[, c("HaulNo", cfg$datr_var)],
      by.x = "lance", by.y = "HaulNo"
    )
    return(invisible(result))
  }

  invisible(NULL)
}
