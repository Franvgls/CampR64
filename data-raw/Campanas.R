# Actualiza las series de campañas (Nsh, Psh, As2...) añadiendo las nuevas.
# Ejecutar desde la raíz del paquete. Sólo añade las que no están ya.

add_camp <- function(nombre, nuevas) {
  e <- new.env()
  load(file.path("data", paste0(nombre, ".rda")), envir = e)
  x <- get(nombre, envir = e)
  x <- c(x, setdiff(nuevas, x))
  assign(nombre, x, envir = e)
  save(list = nombre, envir = e, file = file.path("data", paste0(nombre, ".rda")), compress = "bzip2")
  invisible(x)
}

# 2026: Porcupine terminada, Demersales en curso y Arsa otoño prevista
add_camp("Nsh", "N26")
add_camp("Psh", "P26")
add_camp("As2", "226")
