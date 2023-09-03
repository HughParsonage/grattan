#' Seniors and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param Spouse_income Spouse income whose unutilized SAPTO may be added to the current taxpayer. Must match \code{family_status}; i.e. can only be nonzero when \code{family_status != "single"}.
#' @param family_status Family status of the individual. 
#' @param on_sapto_cd SAPTO claim code type (for non-veterans). A 
#' letter A-E. A = single, B = lived apart due to illness and spouse was eligible,
#'  C = lived apart but spouse ineligible, D = lived together, both eligible for sapto, 
#'  E = lived together, spouse ineligible. Only \code{"A"} and \code{"D"} are supported.
#' @param .check Run checks for consistency of values. For example, ensuring no 
#' single individuals have positive \code{Spouse_income}.
#' @export

sapto <- function(rebate_income,
                  fy.year,
                  fill = 0,
                  sapto.eligible = TRUE,
                  Spouse_income = 0,
                  family_status = "single",
                  on_sapto_cd = "A",
                  .check = TRUE) {
  N <- length(rebate_income)
  if (!is.integer(rebate_income)) {
    rebate_income <- as.integer(rebate_income)
  }
  
  # 
  .Call("Csapto",
        rebate_income,
        fy::fy2yr(fy.year),
        fill, 
        sapto.eligible,
        do_rN(Spouse_income, integer(N), 1L),
        family_status,
        as_raw_sapto_cd(on_sapto_cd),
        PACKAGE = packageName())

}

as_raw_sapto_cd <- function(x, nThread = getOption("grattan.nThread", 1L)) {
  .Call("C_asraw_OnSaptoCd", x, nThread, PACKAGE = packageName())
}

# sample file 2 on_sapto_cd
sf2osc <- function(.dots.ATO) {
  stopifnot(is.data.table(.dots.ATO))
  age <- age_from_file(.dots.ATO)
  is_married <- 
    if (hasName(.dots.ATO, "Marital_status")) {
      .subset2(.dots.ATO, "Marital_status")
    } else if (hasName(.dots.ATO, "Partner_status")) {
      .subset2(.dots.ATO, "Partner_status")
    } else {
      integer(length(age))
    }
  .Call("C_sf2osc", rep_len(as.integer(age), length(is_married)), as.integer(is_married), PACKAGE = packageName())
}


