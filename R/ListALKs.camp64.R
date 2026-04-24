#' Listar ALKs (EDAD<camp>.dbf) y especies por campaña, con nº de otolitos
#'
#' @param zona  "cant","porc","arsa","medi".
#' @param dns   "local" (por defecto) o "serv".
#' @param camp  String opcional; filtra campañas ("307","P22","N25"...).
#' @param with_names  TRUE (default): resuelve nombre con buscaesp64().
#' @param name_field  "scientific" (default), "spanish", "english".
#' @param output "long" (camp, ESP, n_ots, especie) o "wide" (camp × especie = n_ots).
#' @param quiet  TRUE para no imprimir resumen.
#' @export
ListALKs.camp64 <- function(zona = "cant",
                            dns         = c("local","serv"),
                            camp        = NULL,
                            with_names  = TRUE,
                            name_field  = c("scientific","spanish","english"),
                            output      = c("long","wide"),
                            quiet       = FALSE)
{
  dns        <- tolower(match.arg(dns))
  zona       <- tolower(zona)
  name_field <- match.arg(name_field)
  output     <- match.arg(output)
  
  if (!exists("CampR64_paths"))
    stop("CampR64_paths no está definido. ¿Cargaste configRoots_user.R?")
  
  base <- CampR64_paths[[dns]][[zona]]
  if (is.null(base))
    stop("No existe ruta definida para dns='", dns, "', zona='", zona, "'")
  
  # 1) Localizar EDAD*.dbf (filtrando por 'camp' si viene)
  pat <- if (is.null(camp)) "^edad.+\\.dbf$" else paste0("^edad", camp, "\\.dbf$")
  files <- list.files(base, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) {
    if (!quiet) message(if (is.null(camp)) paste0("Sin EDAD<camp>.dbf en ", base)
                        else paste0("No existe EDAD", camp, ".dbf en ", base))
    return(invisible(if (output == "long") {
      data.frame(camp=character(0), ESP=integer(0), n_ots=integer(0), especie=character(0))
    } else {
      matrix(nrow = 0, ncol = 0)
    }))
  }
  
  ord   <- order(tolower(basename(files)))
  files <- files[ord]
  
  # Para imprimir: extrae <camp> de "EDAD<camp>.dbf"
  get_camp <- function(p) toupper(sub("^edad", "", tools::file_path_sans_ext(basename(p)), ignore.case = TRUE))
  camps <- vapply(files, get_camp, character(1))
  if (!quiet) { message("ALKs en zona='", zona, "' (dns='", dns, "'):"); print(paste0("EDAD", camps)) }
  
  # 2) Construir 'out' sumando NL (o suma de E's) por ESP para cada campaña
  out_list <- vector("list", length(files))
  for (i in seq_along(files)) {
    ci <- camps[i]
    
    # leer EDAD<camp>.dbf con lector unificado
    df <- readCampDBF("edad", zona = zona, camp = ci, dns = dns)
    if (!is.data.frame(df) || nrow(df) == 0) next
    
    names(df) <- toupper(trimws(names(df)))
    if (!"ESP" %in% names(df)) next
    
    # cod especie numérico
    ESP_NUM <- suppressWarnings(as.integer(df$ESP))
    keep    <- !is.na(ESP_NUM)
    if (!any(keep)) next
    
    # nº de otolitos: prioriza NL; si no hay, suma por fila de E0..E*
    if ("NL" %in% names(df)) {
      NL <- suppressWarnings(as.integer(df$NL)); NL[is.na(NL)] <- 0L
      n_por_esp <- tapply(NL[keep], INDEX = ESP_NUM[keep], FUN = sum, na.rm = TRUE)
    } else {
      age_cols <- grep("^E[0-9]+$", names(df), value = TRUE)
      if (length(age_cols) == 0) next
      fila_sumE <- rowSums(df[, age_cols, drop = FALSE], na.rm = TRUE)
      n_por_esp <- tapply(fila_sumE[keep], INDEX = ESP_NUM[keep], FUN = sum, na.rm = TRUE)
    }
    
    if (length(n_por_esp) > 0) {
      out_list[[i]] <- data.frame(
        camp   = rep(ci, length(n_por_esp)),
        ESP    = as.integer(names(n_por_esp)),
        n_ots  = as.integer(unname(n_por_esp)),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combinar (esto puede quedar NULL si ningún fichero aportó filas)
  out <- do.call(rbind, out_list)
  
  # 3) Si no hay especies con otolitos, cortar aquí ANTES de mapear nombres
  if (is.null(out) || nrow(out) == 0) {
    if (!quiet) message("No hay especies con otolitos en las campañas seleccionadas.")
    return(invisible(if (output == "long") {
      data.frame(camp=character(0), ESP=integer(0), n_ots=integer(0), especie=character(0))
    } else {
      matrix(nrow = 0, ncol = 0)
    }))
  }
  
  # 4) Añadir nombre con buscaesp64 (pez = grupo 1 por defecto de tu flujo)
  if (isTRUE(with_names)) {
    # Traducir el name_field semántico al `id` que entiende buscaesp64
    name_field <- match.arg(name_field)
    id_map <- c(scientific = "l", spanish = "nombree", english = "nombrei")
    id_val <- id_map[[name_field]]
    
    out$especie <- vapply(
      out$ESP,
      function(code) {
        nm <- tryCatch(
          buscaesp64(gr = 1, esp = code, zona = zona, dns = dns, id = id_val),
          error = function(e) NA_character_
        )
        if (is.na(nm) || !nzchar(nm)) sprintf("%03d", code) else nm
      },
      character(1)
    )
  } else {
    out$especie <- sprintf("%03d", out$ESP)
  }
  
  # 5) Entregar en el formato pedido
  if (output == "long") {
    out <- out[order(out$camp, out$ESP), ]
    return(out)
  } else {
    camps_u <- sort(unique(out$camp))
    esp_u   <- unique(out$especie); esp_u <- esp_u[order(esp_u)]
    mat <- matrix(0L, nrow = length(camps_u), ncol = length(esp_u),
                  dimnames = list(camps_u, esp_u))
    for (i in seq_len(nrow(out))) {
      mat[out$camp[i], out$especie[i]] <- mat[out$camp[i], out$especie[i]] + out$n_ots[i]
    }
    return(mat)
  }
}
