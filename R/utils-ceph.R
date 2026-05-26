# ---- Helpers internos (no exportados) ----
#' @noRd
.ices_map_cant <- function(code) {
  switch(code,
         "9.aN" = list(div = "27.9.a", strata = "9aN"),
         "8.c"  = list(div = "27.8.c", strata = "8c"),
         stop(sprintf("Estrato '%s' sin mapeo ICES definido en .ices_map_cant()", code))
  )
}

.ceph_strata_row <- function(survey, yy, qrt, mth, div, strata, asfis,
                             abund, se_abund, cpue, se_cpue) {
  data.frame(
    RecordType   = "SA",
    Survey       = survey,
    Year         = yy,
    Quarter      = qrt,
    Month        = mth,
    ICESDivision = div,
    StrataID     = strata,
    SpecASFIS    = asfis,
    Abundance    = abund    * 2,
    SE_Abundance = se_abund * 2,
    UnitAb       = "numbers per hour",
    CPUE         = cpue     * 2,
    SE_CPUE      = se_cpue  * 2,
    CPUEUnits    = "kg per hour",
    stringsAsFactors = FALSE)
}