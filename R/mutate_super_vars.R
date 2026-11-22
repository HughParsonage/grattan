#' @title Superannuation caps and Division 293 calculations
#'
#' @description Mutate a sample file to reflect particular caps on concessional contributions and applications of Division 293 tax.
#' @author Hugh Parsonage, William Young
#' @param .sample.file A data.table with taxable income and employer contributions.
#' Accepts 2\% sample MCS or Spr columns and aLife return and super fields
#' matched to one row per person and year. Previous assessed excess in
#' \code{i_excess_super} (or \code{sc_excess_concess_cont}) is replaced when
#' modelling a different cap. If absent, taxable income is assumed to precede
#' any modelled excess inclusion. See
#' \code{system.file("tax-return-fields.txt", package = "grattan")}.
#' @param colname_concessional The name for concessional contributions.
#' @param colname_div293_tax The name of the column containing the values of Division 293 tax payable for that taxpayer.
#' @param colname_new_Taxable_Income The name of the column containing the new Taxable Income.
#' @param div293_threshold The Division 293 threshold.
#' @param cap Concessional cap in dollars. With \code{age_based_cap = FALSE},
#' supply a scalar or an effective cap per person, including available
#' carry-forward. A reported aLife \code{sn_concess_cont_cap} takes precedence
#' over the historical defaults when no cap or age threshold is specified.
#' @param cap2 The cap on concessional contributions for those above the age threshold. No effect if \code{age_based_cap} is FALSE.
#' @param age_based_cap Is the cap on concessional contributions age-based? 
#' @param cap2_age With an exact age, \code{cap2} applies above this age.
#' With a sample age band, a threshold from 20 to below 65 is rounded up to
#' a multiple of five, and the band starting at that age qualifies. Thresholds
#' below 20 include all bands. Thresholds of 65 or more include the 70+ band.
#' @param ecc (logical) Should an excess concessional contributions charge be calculated? (Not implemented.)
#' @param use_other_contr Treat all other contributions as concessional before
#' applying the cap. The default includes only separately identified other
#' concessional contributions (aLife \code{sc_other_concess_cont}).
#' @param scale_contr_match_ato (logical) Should concessional contributions be inflated to match aggregates in 2013-14? That is, should concessional contributions by multiplied by \code{grattan:::super_contribution_inflator_1314}, which was defined to be: \deqn{\frac{\textrm{Total assessable contributions in SMSF and funds}}{\textrm{Total contributions in 2013-14 sample file}}}{Total assessable contributions in SMSF and funds / Total contributions in 2013-14 sample file.}. 
#' @param .lambda Scalar weight applied to \code{concessional contributions}. \eqn{\lambda = 0} means no (extra) weight. \eqn{\lambda = 1} means contributions are inflated by the ratio of aggregates to the sample file's total. For \eqn{R = \textrm{actual} / \textrm{apparent}} then the contributions are scaled by \eqn{1 + \lambda(R - 1)}.
#' @param reweight_late_lodgers (logical) Should WEIGHT be inflated to account for late lodgers?
#' @param .mu Scalar weight for WEIGHT. (\eqn{w' = \mu w}) No effect if \code{reweight_late_lodgers} is \code{FALSE}.
#' @param impute_zero_concess_contr Should zero concessional contributions be imputed using salary?
#' @param .min.Sw.for.SG The minimum salary required for super guarantee to be imputed.
#' @param .SG_rate The super guarantee rate for imputation.
#' @param warn_if_colnames_overwritten (logical) Issue a warning if the construction of helper columns will overwrite existing column names in \code{.sample.file}.
#' @param drop_helpers (logical) Should columns used in the calculation be dropped before the sample file is returned?
#' @param copyDT (logical) Should the data table be \code{copy()}d? If the action of this data table is being compared, possibly useful.
#' @return A data table comprising the original sample file (\code{.sample.file}) with extra superannuation policy-relevant variables for the policy specified by the function.
#' 
#' 
#' @export 

apply_super_caps_and_div293 <- function(.sample.file, colname_concessional = "concessional_contributions", colname_div293_tax = "div293_tax", colname_new_Taxable_Income = "Taxable_income_for_ECT", div293_threshold = 300e3, cap = 30e3, cap2 = 35e3, age_based_cap = TRUE, cap2_age = 59, ecc = FALSE, use_other_contr = FALSE, scale_contr_match_ato = FALSE, .lambda = 0, reweight_late_lodgers = FALSE, .mu = 1.05, impute_zero_concess_contr = FALSE, .min.Sw.for.SG = 450 * 12, .SG_rate = 0.0925, warn_if_colnames_overwritten = TRUE, drop_helpers = FALSE, copyDT = TRUE) {
  if (!identical(ecc, FALSE)) stop("ECC not implemented.")
  if (!is.data.table(.sample.file)) stop(".sample.file must be a data.table")
  d <- if (copyDT) copy(.sample.file) else .sample.file
  field <- function(..., default = 0) return_field(d, ..., default = default)
  n <- nrow(d)
  # Clamp only the freshly computed difference in place. The public amount
  # columns are double, including when both source fields are integer.
  positive_difference <- function(x, y) pmax0(as.double(x - y), in_place = TRUE)

  employer <- field("sc_empl_cont", "Spr_Emplr_Contr", "MCS_Emplr_Contr", default = NULL)
  taxable <- field("ic_taxable_income_loss", "Taxable_Income", default = NULL)
  if (is.null(employer) || is.null(taxable)) {
    stop("The sample file does not have the variables needed: employer contributions and taxable income.")
  }
  personal <- field("sc_pers_cont", "Spr_Prsnl_Contr", "MCS_Prsnl_Contr")
  deductible <- field("ds_pers_super_cont", "Non_emp_spr_amt", "sc_pers_cont_deductible")
  reportable <- field("it_rept_empl_super_cont", "Rptbl_Empr_spr_cont_amt", default = NULL)
  classified_other <- field("sc_other_concess_cont", default = NULL)
  other <- if (use_other_contr) {
    field("sc_other_cont", "Spr_Othr_Contr", "MCS_Othr_Contr", default = classified_other %||% 0)
  } else classified_other

  outputs <- c(colname_concessional, colname_div293_tax, colname_new_Taxable_Income)
  helpers <- c("concessional_cap", "excess_concessional_contributions", "rental_losses",
               "surchargeable_income_div293", "low_tax_contributions_div293", "div293_income")
  dropped <- if (drop_helpers) setdiff(helpers[-1L], outputs) else character()
  put <- function(name, value) {
    if (name %chin% dropped) return(invisible(NULL))
    # set() recycles scalars for new columns. For an existing column use a
    # full-length replacement to preserve the result type on overwrite.
    if (length(value) != n && (length(value) != 1L || hasName(d, name))) {
      value <- rep_len(value, n)
    }
    set(d, j = name, value = value)
  }
  collisions <- intersect(c(outputs, helpers), names(d))
  if (colname_new_Taxable_Income == "Taxable_Income" && "Taxable_Income" %in% collisions) {
    warning("Dropping Taxable Income. Replacing it with modelled taxable income.")
    collisions <- setdiff(collisions, "Taxable_Income")
  }
  if (warn_if_colnames_overwritten && length(collisions)) {
    warning("Overwriting ", paste(collisions, collapse = ", "), " in .sample.file")
  }
  if (!is.null(reportable)) {
    put("SG_contributions", positive_difference(employer, reportable))
    put("salary_sacrifice_contributions", reportable)
  }
  if (scale_contr_match_ato) {
    scale <- 1 + (super_contribution_inflator_1314 - 1) * .lambda
    employer <- employer * scale
    deductible <- deductible * scale
    for (name in intersect(c("sc_empl_cont", "Spr_Emplr_Contr", "MCS_Emplr_Contr",
                             "ds_pers_super_cont", "Non_emp_spr_amt", "sc_pers_cont_deductible"), names(d))) {
      set(d, j = name, value = d[[name]] * scale)
    }
  }

  put("personal_deductible_contributions", deductible)
  put("non_concessional_contributions", positive_difference(personal, deductible))
  gross <- as.double(employer) + deductible
  if (!is.null(other)) gross <- gross + other
  if (impute_zero_concess_contr) {
    wage <- field("i_salary_wage", "Sw_amt", default = NULL)
    if (is.null(wage) || is.null(reportable)) stop("Salary and reportable employer super are required to impute.")
    impute <- which(near(gross, 0) & wage > .min.Sw.for.SG)
    gross[impute] <- wage[impute] * .SG_rate + reportable[impute]
  }
  if (reweight_late_lodgers) {
    if (is.null(d$WEIGHT)) warning("No WEIGHT found; using WEIGHT=50 in reweighting.")
    put("WEIGHT", field("WEIGHT", default = 50) * .mu)
  }

  # Use the reported cap for an observed assessment unless the caller has
  # specified a policy cap. A single closing balance cannot establish carry-forward.
  policy_cap <- function() {
    if (!age_based_cap) return(cap)
    age <- d$c_age_30_june
    above <- if (!is.null(age)) age > cap2_age
    if (is.null(age) || anyNA(age)) {
      band <- field("Age_range", "age_range", "Birth_year", default = NULL)
      lower <- if (!is.null(band)) positive_difference(70, 5 * band)
      if (!is.null(d$age_range_description) && (is.null(lower) || anyNA(lower))) {
        parse_lower <- function(x) as.numeric(sub(" .*", "", sub("^under .*", "0", as.character(x))))
        labels <- accel_repetitive_input(d$age_range_description, parse_lower)
        lower <- if (is.null(lower)) labels else fcoalesce(lower, labels)
      }
      if (is.null(age) && is.null(lower)) stop("An age or age band is required for an age-based cap.")
      band_eligible <- FALSE
      if (!is.null(lower)) {
        # Use the sample file's historical band convention, including its
        # open-ended groups. An exact age still uses the strict cutoff above.
        first_band <- fifelse(cap2_age < 20, 0,
                              fifelse(cap2_age >= 65, 70, 5 * ceiling(cap2_age / 5)))
        band_eligible <- lower >= first_band
        if (anyNA(lower)) band_eligible[is.na(lower)] <- FALSE
      }
      above <- if (is.null(age)) band_eligible else fifelse(is.na(age), band_eligible, above)
    }
    if (length(above) && typeof(cap2) == typeof(cap) && length(cap) %in% c(1L, length(above)) &&
        length(cap2) %in% c(1L, length(above))) {
      fifelse(above, cap2, cap)
    } else {
      ifelse(above, cap2, cap)
    }
  }
  annual_cap <- if (missing(cap) && missing(cap2) && missing(cap2_age)) {
    field("sn_concess_cont_cap", default = policy_cap())
  } else policy_cap()
  if (!(length(annual_cap) %in% c(1L, n)) || (length(annual_cap) && !isTRUE(min(annual_cap) >= 0))) {
    stop("Supply one nonnegative cap or a cap per taxpayer.")
  }
  put("concessional_cap", annual_cap)

  excess <- positive_difference(gross, annual_cap)
  low_tax <- positive_difference(gross, excess)
  previous_excess <- field("i_excess_super", "sc_excess_concess_cont")
  new_taxable <- as.double(taxable) - previous_excess + excess
  rent_loss <- field("it_property_loss", default = positive_difference(0L, field("is_net_rent", "Net_rent_amt")))
  fringe <- field("it_rept_fringe_benefit", "Rep_frng_ben_amt",
                  default = field("it_rept_fringe_benefit_exe") + field("it_rept_fringe_benefit_nexe"))
  surchargeable <- pmax0(new_taxable) + fringe + rent_loss +
    field("it_invest_loss", "Net_fincl_invstmt_lss_amt", "Net_fincl_invstmnt_lss_amt")
  if (hasName(d, "as_family_trust_inc_txd")) {
    surchargeable <- surchargeable + field("as_family_trust_inc_txd")
  }
  for (deduction in intersect(c("zero_rate_super_lump_sum", "is_other_cat3_fhss"), names(d))) {
    surchargeable <- surchargeable - field(deduction)
  }
  total <- surchargeable + low_tax
  put("excess_concessional_contributions", excess)
  put("rental_losses", rent_loss)
  put("surchargeable_income_div293", surchargeable)
  put("low_tax_contributions_div293", low_tax)
  put("div293_income", total)
  put(colname_concessional, low_tax)
  above_threshold <- positive_difference(total, div293_threshold)
  # A missing low-tax contribution also makes the total missing. pminV
  # requires equal lengths and canonicalizes NaN to NA on missing records.
  tax_base <- if (length(low_tax) == length(above_threshold) && !anyNA(above_threshold)) {
    pminV(low_tax, above_threshold)
  } else pmin(low_tax, above_threshold)
  put(colname_div293_tax, 0.15 * tax_base)
  put(colname_new_Taxable_Income, new_taxable)
  for (name in intersect(dropped, names(d))) set(d, j = name, value = NULL)
  d
}
