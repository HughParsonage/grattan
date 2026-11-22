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
    on_sapto_cd <- ifelse(family_status == "single", "A",
                          ifelse(family_status == "separated_due_illness", "B", "D"))
  }
  if (!is.logical(sapto.eligible) ||
      !(length(sapto.eligible) %in% c(1L, N)) || anyNA(sapto.eligible)) {
    stop("`sapto.eligible` must be non-missing logical, of length one or length(rebate_income).")
  }
  
  # 
  .Call("Csapto",
        rebate_income,
        fy::fy2yr(fy.year),
        fill, 
        sapto.eligible,
        do_rN(Spouse_income, integer(N), 1L),
        do_rN(taxable_income, integer(N), 1L),
        do_rN(spouse_taxable_income, integer(N), 1L),
        as_raw_sapto_cd(on_sapto_cd),
        PACKAGE = packageName())

}

as_raw_sapto_cd <- function(x, nThread = getOption("grattan.nThread", 1L)) {
  if (is.factor(x)) x <- as.character(x)
  if (is.double(x)) x <- as.integer(x)
  .Call("C_asraw_OnSaptoCd", x, nThread, PACKAGE = packageName())
}

# sample file 2 on_sapto_cd
sf2osc <- function(.dots.ATO, yr = NULL, System = NULL) {
  stopifnot(is.data.frame(.dots.ATO))
  N <- nrow(.dots.ATO)
  yr <- yr %||% fy::fy2yr(fy::date2fy(Sys.Date()))
  married <- return_field(.dots.ATO, "sp_flag", "Partner_status", "Marital_status", default = 0L)
  return_claim_codes(.dots.ATO, rep_len(age_from_file(.dots.ATO), N),
                     rep_len(as.integer(married), N), yr, System)
}

return_claim_codes <- function(data, age, married, yr, System, eligible = NULL) {
  N <- length(age)
  # The two source dictionaries use opposite numeric sex codes.
  female <- data$c_gender == 0L
  sample_female <- data$Gender == 1L
  if (!length(female)) female <- sample_female
  else if (length(sample_female)) female <- ifelse(is.na(female), sample_female, female)
  if (!length(female)) female <- FALSE
  inferred <- .Call("C_sf2osc", as.double(age), as.integer(married),
                    rep_len(as.logical(female), N), as.integer(yr), System,
                    PACKAGE = "grattan")
  family_code <- ifelse(married != 0L, 4L, 1L)
  spouse_age <- data$sp_pension_age_eligible
  if (!is.null(spouse_age)) family_code[married != 0L & spouse_age %in% c(0, FALSE, "N")] <- 5L
  inferred[inferred != as.raw(0)] <- as.raw(family_code[inferred != as.raw(0)])
  pension_age <- data$c_pension_age_eligible
  if (!is.null(pension_age)) {
    required_age <- System$Sapto$pension_age %||% System$sapto_pension_age
    statutory <- is.null(required_age) || all(age_pension_age(fy::yr2fy(yr), "male") == required_age)
    known <- statutory & pension_age %in% c(0, 1, TRUE, FALSE, "Y", "N")
    attained <- pension_age %in% c(1, TRUE, "Y")
    inferred[known] <- as.raw(ifelse(attained[known], family_code[known], 0L))
  }
  if (any(yr <= 2000L)) {
    # The original low income aged persons rebate excluded recipients of
    # the government allowances and pensions at items 5 and 6 of TaxPack.
    payments <- return_field(data, "Aust_govt_pnsn_allw_amt", default =
      as.double(return_field(data, "i_gov_payments", default = 0)) +
        return_field(data, "i_gov_pension", default = 0))
    inferred[rep_len(yr <= 2000L & payments > 0, N)] <- as.raw(0)
  }
  senior_status <- data$on_sato_eligibility
  if (!is.null(senior_status)) {
    rows <- rep_len(yr <= 2012L, N) & senior_status %in% c("Y", "N")
    inferred[rows] <- as.raw(ifelse(senior_status[rows] == "Y", family_code[rows], 0L))
  }
  claim <- data$on_sapto_cd
  legacy <- data$on_sato_cd
  if (!is.null(legacy)) {
    if (is.character(legacy) || is.factor(legacy)) legacy <- chartr("RTUK", "ABDE", as.character(legacy))
    rows <- rep_len(yr <= 2012L, N) & !is.na(legacy)
    inferred[rows] <- rep_len(as_raw_sapto_cd(legacy), N)[rows]
  }
  if (!is.null(claim)) {
    rows <- if (is.null(legacy)) rep(TRUE, N) else rep_len(yr >= 2013L, N)
    rows <- rows & !is.na(claim)
    inferred[rows] <- rep_len(as_raw_sapto_cd(claim), N)[rows]
  }
  if (!is.null(eligible)) {
    if (!is.logical(eligible) || !length(eligible) || !(length(eligible) %in% c(1L, N)) ||
        anyNA(eligible)) stop("sapto_eligible must be a non-missing logical per taxpayer or a scalar.")
    eligible <- rep_len(eligible, N)
    add <- eligible & inferred == as.raw(0)
    inferred[add] <- as.raw(family_code[add])
    inferred[!eligible] <- as.raw(0)
  }
  inferred
}

