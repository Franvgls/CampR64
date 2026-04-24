# Dependencias de CampR64 (leídas del DESCRIPTION)
deps_campr64 <- c(
  "foreign", "dplyr", "maps", "data.table", "sf", "boot", "geosphere",
  "icesDatras", "lattice", "lubridate", "stringr", "tidyr", "vegan",
  "worrms", "sp", "vioplot", "mapdata",
  # Suggests (para vignettes y tests)
  "knitr", "rmarkdown", "testthat", "devtools", "remotes"
)

missing <- setdiff(deps_campr64, rownames(installed.packages()))
if (length(missing)) {
  install.packages(missing, dependencies = TRUE)
} else {
  message("Todas las dependencias ya instaladas.")
}