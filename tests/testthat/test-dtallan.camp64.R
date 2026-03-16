# helpers ----
skip_if_no_dbf <- function(tipo, zona, camp = NULL, dns = "local") {
  # Intento rápido: si readCampDBF falla por falta de fichero, salto el test
  ok <- TRUE
  res <- try({
    if (is.null(camp)) {
      readCampDBF(tipo, zona = zona, dns = dns)
    } else {
      readCampDBF(tipo, zona = zona, camp = camp, dns = dns)
    }
  }, silent = TRUE)
  if (inherits(res, "try-error") || !is.data.frame(res) || nrow(res) == 0) ok <- FALSE
  if (!ok) testthat::skip(paste("No hay", tipo, "para", zona, camp, "en", dns, "→ se salta test."))
}

# tests ----

test_that("dtallan.camp64 (sexo=FALSE) devuelve estructura y orden por lance (cant/local)", {
  skip_if_no_dbf("lance",  "cant", "N23", "local")
  skip_if_no_dbf("ntall",  "cant", "N23", "local")
  skip_if_no_dbf("fauna",  "cant", "N23", "local")
  skip_if_no_dbf("camp",   "cant", "N23", "local")

  x <- dtallan.camp64(grupo = 1, especie = 42, camp = "N23", zona = "cant",
                      dns = "local", sexo = FALSE, verbose = FALSE)
  expect_true(is.data.frame(x))
  expect_equal(names(x), c("lance","sector","talla","n"))
  # orden numérico por lance (1,2,3, ... 10, 11, 100 ...)
  expect_true(all(diff(x$lance) >= 0))
  # no negativos ni NA en 'n'
  expect_true(all(is.finite(x$n)))
  expect_true(all(x$n >= 0))
})

test_that("dtallan.camp64 (sexo=TRUE) añade columna 'sexo' y respeta estructura (cant/local)", {
  skip_if_no_dbf("lance",  "cant", "N23", "local")
  skip_if_no_dbf("ntall",  "cant", "N23", "local")
  skip_if_no_dbf("fauna",  "cant", "N23", "local")
  skip_if_no_dbf("camp",   "cant", "N23", "local")

  x <- dtallan.camp64(grupo = 1, especie = 42, camp = "N23", zona = "cant",
                      dns = "local", sexo = TRUE, verbose = FALSE)
  expect_true(all(c("lance","sector","talla","sexo","n") %in% names(x)))
  # sexo codificado 1,2,3 o subset de ello
  expect_true(all(unique(x$sexo) %in% c(1L,2L,3L)))
})

test_that("dtallan.camp64 aplica cor.time (los totales cambian al activar/desactivar)", {
  skip_if_no_dbf("lance",  "cant", "N23", "local")
  skip_if_no_dbf("ntall",  "cant", "N23", "local")
  skip_if_no_dbf("fauna",  "cant", "N23", "local")
  skip_if_no_dbf("camp",   "cant", "N23", "local")

  x_no <- dtallan.camp64(1, 42, "N23", "cant", dns="local",
                         sexo=FALSE, cor.time=FALSE, verbose=FALSE)
  x_si <- dtallan.camp64(1, 42, "N23", "cant", dns="local",
                         sexo=FALSE, cor.time=TRUE,  verbose=FALSE)

  # Ambas salidas válidas y con mismas claves (lance/sector/talla) — sólo cambian los valores
  key <- c("lance","sector","talla")
  y_no <- x_no[order(x_no$lance, x_no$sector, x_no$talla), ]
  y_si <- x_si[order(x_si$lance, x_si$sector, x_si$talla), ]
  k_no <- y_no[, key, drop=FALSE]
  k_si <- y_si[, key, drop=FALSE]
  expect_equal(k_no, k_si)

  # La corrección temporal debe modificar algún total
  expect_true(!isTRUE(all.equal(sum(y_no$n), sum(y_si$n))))
})

test_that("dtallan.camp64 respeta excl.sect (cant/local)", {
  skip_if_no_dbf("lance",  "cant", "N23", "local")
  skip_if_no_dbf("ntall",  "cant", "N23", "local")
  skip_if_no_dbf("fauna",  "cant", "N23", "local")
  skip_if_no_dbf("camp",   "cant", "N23", "local")

  x_all <- dtallan.camp64(1, 42, "N23", "cant", dns="local", sexo=FALSE, verbose=FALSE)
  expect_true(nrow(x_all) > 0)

  sect_pres <- sort(unique(x_all$sector))
  # elige un sector presente (si hay) para excluir
  s_ex <- sect_pres[1]
  x_ex <- dtallan.camp64(1, 42, "N23", "cant", dns="local", sexo=FALSE,
                         excl.sect = s_ex, verbose=FALSE)
  expect_true(all(x_ex$sector != s_ex))
  # y obviamente algunos totales cambian
  expect_true(!isTRUE(all.equal(sum(x_all$n), sum(x_ex$n))))
})