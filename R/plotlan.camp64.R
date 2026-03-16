#' Gráfico diagnóstico de tallas por lance (CampR64) con paginación
#'
#' @param grupo,especie,camp,zona,dns Igual que en dtallan.camp64
#' @param sexo  TRUE → apila por sexo (M/H/I); FALSE → totales
#' @param lances vector opcional de lances a mostrar (p.ej. c(11,12,17))
#' @param top_n mostrar solo los 'top_n' lances por N total (ignora 'lances' si se usa)
#' @param ncol,nrow número de columnas/filas de paneles por página (layout)
#' @param page número de página (1, 2, 3, …)
#' @param free_scales TRUE → cada panel con su propio eje Y (mejor visibilidad)
#' @param cex.strip tamaño de strip (título de panel)
#' @param cex.axis  tamaño de ejes
#' @param cex.lab   tamaño de etiquetas x/y
#' @export
plotlan.camp64 <- function(grupo, especie, camp, zona,
                           dns = "local",
                           sexo = FALSE,
                           lances = NULL,
                           top_n = NULL,
                           ncol = 3, nrow = 3, page = 1,
                           free_scales = TRUE,
                           cex.strip = 0.8, cex.axis = 0.8, cex.lab = 0.9) {
  
  if (!requireNamespace("lattice", quietly = TRUE))
    stop("Necesitas 'lattice' (install.packages('lattice')).")
  
  # 1) Base por lance (correcciones ya aplicadas en dtallan.camp64)
  dt <- dtallan.camp64(grupo, especie, camp, zona, dns = dns, sexo = sexo, verbose = FALSE)
  if (!nrow(dt)) stop("No hay tallas para dibujar.")
  
  # 2) Asegurar tipos
  dt$talla <- suppressWarnings(as.numeric(dt$talla))
  dt$n     <- suppressWarnings(as.numeric(dt$n))
  dt <- dt[is.finite(dt$talla) & is.finite(dt$n), , drop = FALSE]
  
  # 3) Elegir lances a mostrar
  if (!is.null(top_n)) {
    tot <- aggregate(n ~ lance, dt, sum, na.rm = TRUE)
    tot <- tot[order(-tot$n, tot$lance), ]
    keep <- head(tot$lance, top_n)
    dt <- dt[dt$lance %in% keep, , drop = FALSE]
  } else if (!is.null(lances)) {
    dt <- dt[dt$lance %in% lances, , drop = FALSE]
  }
  
  if (!nrow(dt)) stop("No hay tallas para dibujar tras filtrar.")
  
  # 4) Orden de paneles (por N total descendente para ver 'lo gordo' primero)
  ord <- aggregate(n ~ lance, dt, sum, na.rm = TRUE)
  ord <- ord[order(-ord$n, ord$lance), ]
  dt$lance <- factor(dt$lance, levels = ord$lance)
  
  # 5) Paginación
  per_page <- max(1L, as.integer(ncol) * as.integer(nrow))
  total_panels <- nlevels(dt$lance)
  total_pages  <- ceiling(total_panels / per_page)
  page <- min(max(1L, as.integer(page)), total_pages)
  
  levs <- levels(dt$lance)
  idx_start <- (page - 1) * per_page + 1
  idx_end   <- min(page * per_page, total_panels)
  keep_levs <- levs[idx_start:idx_end]
  dt <- dt[dt$lance %in% keep_levs, , drop = FALSE]
  dt$lance <- droplevels(dt$lance)
  
  # 6) Estética por sexo (si aplica)
  if (isTRUE(sexo)) {
    if (!"sexo" %in% names(dt)) stop("No hay columna 'sexo' en el detalle.")
    dt$sexo <- suppressWarnings(as.integer(dt$sexo))
    dt$sexo_f <- factor(dt$sexo, levels = c(1L, 2L, 3L), labels = c("M","H","I"))
    cols_sexo <- c("dodgerblue3", "red3", "gray50")
  }
  
  # 7) Escalas
  scales <- list(
    x = list(relation = "same", cex = cex.axis),
    y = list(relation = if (free_scales) "free" else "same", cex = cex.axis)
  )
  
  # 8) Par settings (hacer strips más compactos)
  lattice::trellis.par.set(strip.background = list(col = "grey90"),
                           strip.shingle    = list(col = "grey80"))
  
  # 9) Título general (opcional: añadir nombre científico)
  tit <- tryCatch(
    buscaesp64(grupo = if (is.null(grupo)) 1L else grupo,
               especie = especie, zona = zona, dns = dns,
               name_field = "scientific"),
    error = function(e) sprintf("%03d", as.integer(especie))
  )
  main_t <- paste0("Tallas por lance: ", camp, " – ", tit, "  (pág. ", page, "/", total_pages, ")")
  
  lay <- c(ncol, nrow)
  
  # 10) Llamada lattice
  if (isTRUE(sexo)) {
    p <- lattice::barchart(n ~ talla | lance,
                           groups = ~ sexo_f,
                           data   = dt,
                           stack  = TRUE,
                           col    = cols_sexo,
                           layout = lay,
                           scales = scales,
                           xlab   = list("Talla (cm)", cex = cex.lab),
                           ylab   = list("N corregido", cex = cex.lab),
                           main   = list(label = main_t, cex = 1),
                           par.strip.text = list(cex = cex.strip),
                           auto.key = list(columns = 3, space = "top",
                                           rectangles = TRUE, points = FALSE,
                                           text = list(c("M","H","I"))),
                           panel = function(x, y, ..., groups, subscripts) {
                             lattice::panel.grid(h = -1, v = 0, col = "grey85", lty = 3)
                             lattice::panel.barchart(x, y, ..., groups = groups, subscripts = subscripts, origin = 0)
                           })
  } else {
    p <- lattice::barchart(n ~ talla | lance,
                           data   = dt,
                           col    = "gray40",
                           layout = lay,
                           scales = scales,
                           xlab   = list("Talla (cm)", cex = cex.lab),
                           ylab   = list("N corregido", cex = cex.lab),
                           main   = list(label = main_t, cex = 1),
                           par.strip.text = list(cex = cex.strip),
                           panel = function(x, y, ...) {
                             lattice::panel.grid(h = -1, v = 0, col = "grey85", lty = 3)
                             lattice::panel.barchart(x, y, ..., origin = 0)
                           })
  }
  
  print(p)
  invisible(list(plot = p,
                 page = page, total_pages = total_pages,
                 shown_lances = levels(dt$lance)))
}