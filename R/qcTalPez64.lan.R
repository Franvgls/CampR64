#' Comprueba concordancia entre capturas y tallas en un lance
#'
#' Comprueba coherencia entre faunaXXX.dbf y NtallXXX.dbf para un lance concreto:
#' especies capturadas sin medir, especies medidas sin captura, y discordancias
#' entre pesos y números del fauna vs los calculados desde las tallas.
#' @param camp Campaña en formato Camp Xyy
#' @param zona Zona: "cant", "porc", "arsa"
#' @param dns Origen de datos: "local" o "serv"
#' @param lan Lance a comprobar
#' @param gr Grupo: 1 peces, 2 crustáceos, 3 cefalópodos (default 1)
#' @param verbose Si TRUE muestra también los lances sin errores (default FALSE)
#' @return Imprime en consola las discordancias encontradas
#' @examples
#' qcTalPez64.lan("N25", zona="cant", dns="local", lan=23, gr=1)
#' @seealso \code{\link{qcTalPez.camp64}}
#' @family Control de calidad
#' @export
qcTalPez64.lan <- function(camp, zona = "cant", dns = c("local","serv"),
                            lan, gr = 1, verbose = FALSE) {
  if (length(camp) > 1) stop("Solo se puede comprobar una campaña a la vez.")
  if (length(lan)  > 1) stop("Solo se puede comprobar un lance a la vez.")
  if (!gr %in% 1:3)     stop("Solo se miden grupos 1 (peces), 2 (crustáceos) y 3 (moluscos).")

  dns  <- tolower(match.arg(dns))
  zona <- tolower(zona)

  w_lance <- ifelse(zona == "cant", 3, 2)
  lan_f   <- formatC(as.numeric(lan), flag = 0, width = w_lance)

  # ── Leer fauna ──────────────────────────────────────────────────────────────
  fauna <- readCampDBF("fauna", zona = zona, camp = camp, dns = dns)
  names(fauna) <- tolower(names(fauna))
  fauna$lance <- formatC(as.numeric(fauna$lance), flag = 0, width = w_lance)
  listsps <- fauna[trimws(fauna$lance) == lan_f & fauna$grupo == gr, ]

  if (nrow(listsps) == 0) {
    warning(paste("Lance", lan_f, "sin fauna registrada para grupo", gr, "— se omite."))
    return(invisible(NULL))
  }

  # ── Leer ntall ──────────────────────────────────────────────────────────────
  ntall <- readCampDBF("ntall", zona = zona, camp = camp, dns = dns)
  names(ntall) <- tolower(names(ntall))
  ntall$lance <- formatC(as.numeric(ntall$lance), flag = 0, width = w_lance)
  tals <- ntall[trimws(ntall$lance) == lan_f & ntall$grupo == gr, ]
  tals$numpond <- tals$numer * tals$peso_gr / tals$peso_m

  # ── Comparación especies ────────────────────────────────────────────────────
  fau     <- unique(trimws(listsps$esp))
  talsdat <- unique(trimws(tals$esp))

  nomed   <- fau[!fau %in% talsdat]      # en fauna pero sin tallas
  sincapt <- talsdat[!talsdat %in% fau]  # en tallas pero sin fauna

  hay_errores <- length(nomed) > 0 || length(sincapt) > 0
  if (hay_errores || verbose) cat(paste0("Lance ", lan_f, ":\n"))

  if (length(nomed) > 0) {
    cat("Especies con captura y sin medir:\n")
    for (i in nomed)
      cat(paste0("  ", i, " - ",
                 buscaesp64(gr, trimws(i), zona = zona, dns = dns), "\n"))
  }
  if (length(sincapt) > 0) {
    cat("Especies medidas sin capturas en fauna:\n")
    for (i in sincapt)
      cat(paste0("  ", i, " - ",
                 buscaesp64(gr, trimws(i), zona = zona, dns = dns), "\n"))
  }
  if (length(nomed) == 0 && length(sincapt) == 0 && verbose)
    cat(paste0("Lance ", lan_f, ": OK\n"))

  # ── Comparación pesos y números ─────────────────────────────────────────────
  talspes <- rowSums(tapply(tals$peso_gr, tals[, c("esp","cate")], mean,  na.rm = TRUE), na.rm = TRUE)
  talsnum <- rowSums(tapply(tals$numpond, tals[, c("esp","cate")], sum,   na.rm = TRUE), na.rm = TRUE)

  # Solo especies que están en ambos ficheros
  listsps_med <- listsps[trimws(listsps$esp) %in% talsdat, ]
  esp_match   <- trimws(listsps_med$esp)

  errpesos <- listsps_med$peso_gr - talspes[esp_match]
  errnumer <- listsps_med$numero  - talsnum[esp_match]
  names(errpesos) <- names(errnumer) <- esp_match

  if (any(!is.na(errpesos) & abs(errpesos) > 1)) {
    bad <- names(which(abs(errpesos) > 1 & !is.na(errpesos)))
    for (b in bad) {
      cat(paste0("Discordancia pesos para ",
                 buscaesp64(gr, trimws(b), zona = zona, dns = dns), ":\n"))
      cat(paste0("  Tallas: ", round(talspes[b], 1),
                 "  Fauna: ",
                 listsps$peso_gr[trimws(listsps$esp) == b], "\n"))
    }
  }
  if (any(!is.na(errnumer) & abs(errnumer) > 1)) {
    bad <- names(which(abs(errnumer) > 1 & !is.na(errnumer)))
    for (b in bad) {
      cat(paste0("Discordancia números para ",
                 buscaesp64(gr, trimws(b), zona = zona, dns = dns), ":\n"))
      cat(paste0("  Tallas: ", round(talsnum[b], 1),
                 "  Fauna: ",
                 listsps$numero[trimws(listsps$esp) == b], "\n"))
    }
  }

  invisible(NULL)
}
