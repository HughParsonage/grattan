#' Seniors and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param Spouse_income Spouse income whose unutilized SAPTO may be added to the current taxpayer. Must match \code{family_status}; i.e. can only be nonzero when \code{family_status != "single"}.
#' @param taxable_income,spouse_taxable_income Taxable incomes used to calculate the unused
#' rebate available for transfer, including any exempt pensions specified by
#' regulation 12(5). Each defaults to the corresponding rebate income.
#' @param family_status Family status of the individual. 
#' @param on_sapto_cd SAPTO claim code type (for non-veterans). A 
#' letter A-E. A = single, B = lived apart due to illness and spouse was eligible,
#'  C = lived apart but spouse ineligible, D = lived together, both eligible for sapto, 
#'  E = lived together, spouse ineligible. A supplied code denotes actual
#' eligibility; the standalone function does not infer eligibility from age.
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
                  .check = TRUE,
                  taxable_income = rebate_income, spouse_taxable_income = Spouse_income) {
  N <- length(rebate_income)
  if (!is.integer(rebate_income)) {
    rebate_income <- as.integer(rebate_income)
  }
  if (missing(on_sapto_cd)) {
    on_sapto_cd <- as.raw(c(1L, 2L, 0L, 4L))[fmatch(family_status,
      c("single", "separated_due_illness", NA_character_), nomatch = 4L)]
  }
  if (!is.logical(sapto.eligible) ||
      !(length(sapto.eligible) %in% c(1L, N)) || anyNA(sapto.eligible)) {
    stop("`sapto.eligible` must be non-missing logical, of length one or length(rebate_income).")
  }
  
  # The native calculation accepts scalar spouse amounts. Keep those small,
  # and share the normalized amount when the transfer uses the same income.
  amount <- function(x) {
    if (length(x) == 1L) return(do_rN(x, 0L, 1L))
    if (length(x) != N) return(0L)
    if (is.integer(x) && !anyNA(x)) return(x)
    do_rN(x, integer(N), 1L)
  }
  Spouse_income <- amount(Spouse_income)
  .Call("Csapto",
        rebate_income,
        fy::fy2yr(fy.year),
        fill, 
        sapto.eligible,
        Spouse_income,
        amount(taxable_income),
        if (missing(spouse_taxable_income)) Spouse_income else amount(spouse_taxable_income),
        if (is.raw(on_sapto_cd)) on_sapto_cd else as_raw_sapto_cd(on_sapto_cd),
        PACKAGE = packageName())

}

as_raw_sapto_cd <- function(x, nThread = getOption("grattan.nThread", 1L), legacy = FALSE) {
  if (is.factor(x)) return(as_raw_sapto_cd(levels(x), nThread, legacy)[as.integer(x)])
  if (legacy && is.character(x)) x <- chartr("RTUK", "ABDE", x)
  if (is.double(x)) x <- as.integer(x)
  .Call("C_asraw_OnSaptoCd", x, nThread, PACKAGE = packageName())
}

# sample file 2 on_sapto_cd
sf2osc <- function(.dots.ATO, yr = NULL, System = NULL) {
  stopifnot(is.data.frame(.dots.ATO))
  N <- nrow(.dots.ATO)
  yr <- yr %||% fy::fy2yr(fy::date2fy(Sys.Date()))
  married <- return_field(.dots.ATO, "sp_flag", "Partner_status", "Marital_status", default = 0L)
  married <- as.integer(married)
  if (length(married) != N) married <- rep_len(married, N)
  return_claim_codes(.dots.ATO, age_from_file(.dots.ATO), married, yr, System)
}

return_claim_codes <- function(data, age, married, yr, System, eligible = NULL) {
  N <- length(married)
  if (!N) return(raw())
  if (!is.null(eligible) && (!is.logical(eligible) || !(length(eligible) %in% c(1L, N)) ||
                             anyNA(eligible))) {
    stop("sapto_eligible must be a non-missing logical per taxpayer or a scalar.")
  }
  years <- if (length(yr) == 1L) c(yr, yr) else hutilscpp::minmax(yr)
  first_year <- years[1L]
  claim <- data$on_sapto_cd
  legacy <- data$on_sato_cd
  modern_years <- is.null(legacy) || isTRUE(first_year >= 2013L)
  complete_modern <- !is.null(claim) && modern_years && !anyNA(claim)
  complete_legacy <- !is.null(legacy) && isTRUE(years[2L] <= 2012L) && !anyNA(legacy)
  complete <- complete_modern || complete_legacy
  forced_off <- length(eligible) == 1L && !eligible

  infer <- function(age, female = FALSE, pension_age = NULL, spouse_age = NULL) {
    .Call("C_sf2osc", age, as.integer(married), female, as.integer(yr), System,
          pension_age, spouse_age, PACKAGE = "grattan")
  }
  if (forced_off || complete) {
    # Validate the annual schedules even when no age inference is needed.
    infer(NULL)
    if (forced_off) return(raw(N))
    inferred <- if (complete_modern) as_raw_sapto_cd(claim) else as_raw_sapto_cd(legacy, legacy = TRUE)
    if (length(inferred) != N) inferred <- rep_len(inferred, N)
    if (is.null(eligible)) return(inferred)
  }

  # The native age pass can consume integer and logical flags directly.
  pension_flag <- function(x) {
    if (is.null(x) || is.logical(x) || (is.integer(x) && !is.factor(x))) return(x)
    if (is.double(x)) return(fmatch(x, c(0L, 1L)) == 2L)
    fmatch(x, c("0", "FALSE", "N", "1", "TRUE", "Y")) > 3L
  }
  spouse_age <- pension_flag(data$sp_pension_age_eligible)
  if (!complete) {
    # The two dictionaries use opposite sex codes. From 2013-14 the
    # pension-age schedule is identical for both sexes.
    female <- FALSE
    if (isTRUE(first_year <= 2013L)) {
      if (!is.null(data$c_gender)) {
        female <- data$c_gender == 0L
        if (anyNA(female) && !is.null(data$Gender)) female <- fcoalesce(female, data$Gender == 1L)
      } else if (!is.null(data$Gender)) female <- data$Gender == 1L
    }
    pension_age <- data$c_pension_age_eligible
    required_age <- System$Sapto$pension_age %||% System$sapto_pension_age
    # The statutory male pension age increases monotonically across years.
    if (!is.null(pension_age) && !is.null(required_age) &&
        !all(age_pension_age(fy::yr2fy(unique(years)), "male") == required_age)) {
      pension_age <- NULL
    }
    if (is.object(age) || (!is.integer(age) && !is.double(age))) age <- as.double(age)
    inferred <- infer(age, female, pension_flag(pension_age), spouse_age)
  }

  senior_status <- if (!complete && first_year <= 2012L) data$on_sato_eligibility
  if (!is.null(senior_status) || !is.null(eligible)) {
    family_code <- fifelse(married != 0L, 4L, 1L, na = 0L)
    if (!is.null(spouse_age)) family_code[married != 0L & spouse_age == 0L] <- 5L
  }
  if (!complete) {
    if (first_year <= 2000L) {
      # The predecessor excluded recipients of TaxPack items 5 and 6.
      payments <- return_field(data, "Aust_govt_pnsn_allw_amt", default =
        as.double(return_field(data, "i_gov_payments", default = 0)) +
          return_field(data, "i_gov_pension", default = 0))
      if (length(payments) != 1L || payments > 0) inferred[yr <= 2000L & payments > 0] <- as.raw(0)
    }
    if (!is.null(senior_status)) {
      status <- fmatch(senior_status, c("N", "Y"), nomatch = 0L)
      granted <- yr <= 2012L & status == 2L
      inferred[granted] <- as.raw(family_code[granted])
      inferred[yr <= 2012L & status == 1L] <- as.raw(0)
    }
    if (!is.null(legacy) && first_year <= 2012L) {
      rows <- yr <= 2012L & !is.na(legacy)
      codes <- if (length(legacy) == 1L) legacy else legacy[rows]
      inferred[rows] <- as_raw_sapto_cd(codes, legacy = TRUE)
    }
    if (!is.null(claim)) {
      rows <- !is.na(claim)
      if (!modern_years) rows <- rows & yr >= 2013L
      codes <- if (length(claim) == 1L) claim else claim[rows]
      inferred[rows] <- as_raw_sapto_cd(codes)
    }
  }
  if (!is.null(eligible)) {
    add <- eligible & inferred == as.raw(0)
    inferred[add] <- as.raw(family_code[add])
    if (length(eligible) > 1L) inferred[!eligible] <- as.raw(0)
  }
  inferred
}
