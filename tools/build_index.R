#' Genera el catalogo de funciones de CampR64
#'
#' Recorre todos los ficheros .R de R/, extrae los bloques roxygen2
#' (titulo, descripcion, @family, @export, @param) sin depender de
#' funciones internas de roxygen2, y produce:
#'   - inst/extdata/function_index.rds  (data.frame para busqueda programatica)
#'   - FUNCTIONS.md                     (indice legible, agrupado por categoria)
#'
#' Ejecutar desde la raiz del paquete:
#'   source("tools/build_index.R")
#'
#' No requiere devtools::document() previo: lee el codigo fuente directamente.

# ---- 1. Configuracion ------------------------------------------------------

pkg_root <- getwd()  # ejecutar con el proyecto CampR64 abierto en RStudio
r_dir    <- file.path(pkg_root, "R")
out_rds  <- file.path(pkg_root, "inst", "extdata", "function_index.rds")
out_md   <- file.path(pkg_root, "FUNCTIONS.md")

if (!dir.exists(r_dir)) {
  stop("No se encuentra el directorio R/ en: ", r_dir,
       "\nEjecuta este script con el proyecto CampR64 como working directory.")
}

# Categorias por prefijo/keyword de nombre de fichero, usadas solo si
# la funcion no tiene @family explicito en su roxygen.
categorias_kw <- list(
  "Cartografia"        = c("map", "armap", "grafmarks"),
  "Datos de campana"   = c("^dat", "camp_readers", "campsdns", "camptoyear"),
  "Tallas y pesos"     = c("^dtall", "dattal", "denstal", "talbox", "talpes"),
  "Estadistica/CV"     = c("^cv", "strmean", "meanmaxl", "p95tal"),
  "Fauna y especies"   = c("fauna", "buscaesp", "buscacod", "buscaaphia", "abrvesp"),
  "Nephrops"           = c("nepfu", "vedacigala"),
  "Cetaceos"           = c("porc(nw|shelf|sw)sac", "newporccanyon"),
  "ALK edad-talla"     = c("^alk", "getalk", "ecolgr", "ecolmatrix", "edadsect", "edadstr"),
  "Graficos"           = c("^graf", "histbox", "bubbage", "logabage"),
  "Artes de pesca"     = c("^arte", "pasa\\.(arte|lan)"),
  "QC"                 = c("^qc", "map\\.check", "mark\\.gaps", "matchhidro"),
  "Formatos DATRAS"    = c("^camptohh", "^camptohl", "geticesarea", "mapicesstatrec"),
  "Cuadriculas/sorteo" = c("sacagrid", "sorteo", "captdia"),
  "Shapefiles"         = c("shape"),
  "Utilidades"         = c("dpthprfl", "cephdatabyh")
)

clasifica <- function(nombre_fichero) {
  nombre <- tolower(nombre_fichero)
  for (cat in names(categorias_kw)) {
    for (kw in categorias_kw[[cat]]) {
      if (grepl(kw, nombre)) return(cat)
    }
  }
  "Sin clasificar"
}

# ---- 2. Parseo de un fichero .R --------------------------------------------

#' Extrae bloques roxygen2 asociados a definiciones de función en un fichero
#'
#' @param path Ruta al fichero .R
#' @return data.frame con una fila por función documentada encontrada
parse_roxygen_file <- function(path) {

  lineas <- readLines(path, warn = FALSE, encoding = "UTF-8")
  n <- length(lineas)

  es_roxygen <- grepl("^\\s*#'", lineas)
  # regex de definicion: nombre <- function( ... o nombre = function(
  es_def <- grepl("^\\s*[a-zA-Z0-9_.]+\\s*(<-|=)\\s*function\\s*\\(", lineas)

  resultados <- list()

  i <- 1
  while (i <= n) {
    if (es_roxygen[i]) {
      inicio_bloque <- i
      while (i <= n && es_roxygen[i]) i <- i + 1
      fin_bloque <- i - 1

      # el bloque solo cuenta si la siguiente linea no vacia es una definicion
      j <- i
      while (j <= n && grepl("^\\s*$", lineas[j])) j <- j + 1

      if (j <= n && es_def[j]) {
        nombre_fn <- sub("^\\s*([a-zA-Z0-9_.]+)\\s*(<-|=).*$", "\\1", lineas[j])

        bloque <- sub("^\\s*#'\\s?", "", lineas[inicio_bloque:fin_bloque])

        tags <- list()
        titulo <- character(0)
        descripcion <- character(0)
        tag_actual <- NULL

        for (linea in bloque) {
          m <- regmatches(linea, regexec("^@(\\w+)\\s*(.*)$", linea))[[1]]
          if (length(m) == 3) {
            tag_actual <- m[2]
            tags[[tag_actual]] <- c(tags[[tag_actual]], m[3])
          } else if (is.null(tag_actual)) {
            if (length(titulo) == 0 && !grepl("^\\s*$", linea)) {
              titulo <- linea
            } else if (!grepl("^\\s*$", linea)) {
              descripcion <- c(descripcion, linea)
            }
          }
        }

        family <- if (!is.null(tags[["family"]])) tags[["family"]][1] else clasifica(basename(path))
        export <- !is.null(tags[["export"]])
        n_param <- length(tags[["param"]])
        tiene_ejemplos <- !is.null(tags[["examples"]])

        resultados[[length(resultados) + 1]] <- data.frame(
          fichero      = basename(path),
          funcion      = nombre_fn,
          titulo       = if (length(titulo)) titulo else NA_character_,
          descripcion  = if (length(descripcion)) paste(descripcion, collapse = " ") else NA_character_,
          categoria    = family,
          exportada    = export,
          n_parametros = n_param,
          tiene_ejemplos = tiene_ejemplos,
          stringsAsFactors = FALSE
        )
      }
    } else {
      i <- i + 1
    }
  }

  if (length(resultados) == 0) return(NULL)
  do.call(rbind, resultados)
}

# ---- 3. Recorrer el paquete -------------------------------------------------

ficheros_r <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
cat("Analizando", length(ficheros_r), "ficheros en", r_dir, "...\n")
cat("(script ejecutado desde tools/build_index.R)\n")

indice <- do.call(rbind, lapply(ficheros_r, parse_roxygen_file))

if (is.null(indice) || nrow(indice) == 0) {
  stop("No se encontraron funciones documentadas con roxygen2 en R/. ",
       "Revisa que los bloques #' precedan directamente a 'nombre <- function(...)'.")
}

# ---- 3b. Normalizacion de categorias --------------------------------------
# canon_map: corrige variantes de TEXTO de la misma categoria (typos, acentos,
#            mayusculas, nombres provisionales) sin tocar los ficheros fuente.
# override_family: corrige por NOMBRE DE FUNCION los casos donde el @family
#            del roxygen esta simplemente mal puesto (error de clasificacion,
#            no de texto) - tiene prioridad sobre canon_map.
# Revisar y ampliar esta tabla cada vez que aparezcan categorias nuevas
# fragmentadas en FUNCTIONS.md / buscaFuncion64(categorias = TRUE).

canon_map <- c(
  "mapas"                    = "Cartografía",
  "mapas base"               = "Cartografía",
  "mapas, NEP"               = "Cartografía",
  "Cartografia"              = "Cartografía",
  "Conversion unidades"      = "Conversión de unidades",
  "datos_especies#'"         = "Datos especies",
  "datos_especies"           = "Datos especies",
  "Tallas y pesos"           = "Distribuciones de tallas",
  "gear"                     = "Artes de pesca",
  "ecologia"                 = "Ecología",
  "ListadosFauna"            = "Faunísticas"
)

override_family <- c(
  "ListFauna.lan64"   = "Faunísticas",
  "qcDupFauna64.camp" = "Control de calidad"
)

indice$categoria <- trimws(indice$categoria)

hay_canon <- indice$categoria %in% names(canon_map)
indice$categoria[hay_canon] <- canon_map[indice$categoria[hay_canon]]

hay_override <- indice$funcion %in% names(override_family)
indice$categoria[hay_override] <- override_family[indice$funcion[hay_override]]

indice <- indice[order(indice$categoria, indice$funcion), ]
rownames(indice) <- NULL

cat("Funciones indexadas:", nrow(indice), "\n")
cat("Sin @title/roxygen minimo:", sum(is.na(indice$titulo)), "\n")

# ---- 4. Guardar indice programatico ----------------------------------------

dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)
saveRDS(indice, out_rds)
cat("Indice guardado en:", out_rds, "\n")

# ---- 5. Generar FUNCTIONS.md agrupado por categoria ------------------------

con <- file(out_md, open = "w", encoding = "UTF-8")
cat("# Catalogo de funciones — CampR64\n\n", file = con)
cat("Generado automaticamente por `tools/build_index.R`. No editar a mano.\n\n",
    file = con)
cat(sprintf("Total funciones documentadas: **%d**\n\n", nrow(indice)), file = con)

for (cat_nombre in sort(unique(indice$categoria))) {
  sub <- indice[indice$categoria == cat_nombre, ]
  cat(sprintf("## %s (%d)\n\n", cat_nombre, nrow(sub)), file = con)
  cat("| Función | Fichero | Título | Export | @examples |\n", file = con)
  cat("|---|---|---|---|---|\n", file = con)
  for (k in seq_len(nrow(sub))) {
    fila <- sub[k, ]
    titulo_md <- ifelse(is.na(fila$titulo), "*(sin @title)*", fila$titulo)
    cat(sprintf(
      "| `%s()` | %s | %s | %s | %s |\n",
      fila$funcion, fila$fichero, titulo_md,
      ifelse(fila$exportada, "si", "no"),
      ifelse(fila$tiene_ejemplos, "si", "no")
    ), file = con)
  }
  cat("\n", file = con)
}

close(con)
cat("Indice legible generado en:", out_md, "\n")

cat("\nListo. Para buscar desde la consola usa buscaFuncion64('palabra clave').\n")
