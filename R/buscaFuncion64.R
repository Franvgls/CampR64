#' Busca funciones de CampR64 por palabra clave
#'
#' Consulta el indice generado por \code{data-raw/build_index.R}
#' (\code{inst/extdata/function_index.rds}) y devuelve las funciones cuyo
#' nombre, titulo, descripcion o categoria coinciden con la palabra clave.
#' Pensada para uso interactivo: cuando el paquete crece y no se recuerda
#' el nombre exacto de una funcion.
#'
#' @param patron Palabra o palabras clave a buscar (case-insensitive).
#'   Se trata como expresion regular, asi que caracteres especiales de
#'   regex deben escaparse si se buscan literalmente.
#' @param campos Vector de campos donde buscar. Por defecto busca en
#'   nombre de funcion, titulo, descripcion y categoria.
#' @param categoria Si se indica, filtra ademas por categoria exacta
#'   (ver categorias disponibles con \code{buscaFuncion64(categorias = TRUE)}).
#' @param categorias Si \code{TRUE}, ignora \code{patron} y solo lista
#'   las categorias disponibles con su numero de funciones.
#'
#' @return Invisible: data.frame con las coincidencias. Se imprime un
#'   resumen legible en consola.
#'
#' @examples
#' \dontrun{
#' buscaFuncion64("duplicados")
#' buscaFuncion64("sefos")
#' buscaFuncion64(categoria = "QC")
#' buscaFuncion64(categorias = TRUE)
#' }
#'
#' @export
buscaFuncion64 <- function(patron = NULL,
                            campos = c("funcion", "titulo", "descripcion", "categoria"),
                            categoria = NULL,
                            categorias = FALSE) {

  idx_path <- system.file("extdata", "function_index.rds", package = "CampR64")

  if (!nzchar(idx_path) || !file.exists(idx_path)) {
    # fallback para desarrollo local, cuando el paquete aun no esta instalado
    idx_path_local <- file.path("inst", "extdata", "function_index.rds")
    if (file.exists(idx_path_local)) {
      idx_path <- idx_path_local
    } else {
      stop("No se encuentra function_index.rds. Ejecuta primero ",
           "source('data-raw/build_index.R') desde la raiz del paquete.")
    }
  }

  indice <- readRDS(idx_path)

  if (isTRUE(categorias)) {
    tabla <- sort(table(indice$categoria), decreasing = TRUE)
    cat("Categorias disponibles:\n\n")
    for (nom in names(tabla)) {
      cat(sprintf("  %-22s %d\n", nom, tabla[[nom]]))
    }
    return(invisible(tabla))
  }

  resultado <- indice

  if (!is.null(categoria)) {
    resultado <- resultado[tolower(resultado$categoria) == tolower(categoria), ]
  }

  if (!is.null(patron)) {
    coincide <- rep(FALSE, nrow(resultado))
    for (campo in campos) {
      valores <- resultado[[campo]]
      valores[is.na(valores)] <- ""
      coincide <- coincide | grepl(patron, valores, ignore.case = TRUE)
    }
    resultado <- resultado[coincide, ]
  }

  resultado <- resultado[order(resultado$categoria, resultado$funcion), ]

  if (nrow(resultado) == 0) {
    cat("Sin coincidencias para:", patron, "\n")
    return(invisible(resultado))
  }

  cat(sprintf("%d coincidencia(s):\n\n", nrow(resultado)))
  for (k in seq_len(nrow(resultado))) {
    fila <- resultado[k, ]
    cat(sprintf("  %s()  [%s]\n", fila$funcion, fila$categoria))
    if (!is.na(fila$titulo)) {
      cat(sprintf("    %s\n", fila$titulo))
    }
    cat(sprintf("    -> %s\n\n", fila$fichero))
  }

  invisible(resultado)
}
