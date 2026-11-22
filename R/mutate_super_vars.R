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
#' @param cap2_age The age above which \code{cap2} applies.
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

apply_super_caps_and_div293 <- function(.sample.file, 
                                        colname_concessional = "concessional_contributions",
                                        colname_div293_tax = "div293_tax", 
                                        colname_new_Taxable_Income = "Taxable_income_for_ECT",
                                        div293_threshold = 300e3, 
                                        cap = 30e3, 
                                        cap2 = 35e3, 
                                        age_based_cap = TRUE, 
                                        cap2_age = 59, 
                                        ecc = FALSE,
                                        use_other_contr = FALSE,
                                        scale_contr_match_ato = FALSE, 
                                        .lambda = 0, 
                                        reweight_late_lodgers = FALSE,
                                        .mu = 1.05,
                                        impute_zero_concess_contr = FALSE,
                                        .min.Sw.for.SG = 450 * 12,
                                        .SG_rate = 0.0925,
                                        warn_if_colnames_overwritten = TRUE, 
                                        drop_helpers = FALSE, 
                                        copyDT = TRUE) {
  if (!identical(ecc, FALSE)) stop("ECC not implemented.")
  if (!is.data.table(.sample.file)) stop(".sample.file must be a data.table")
  d <- if (copyDT) copy(.sample.file) else .sample.file
  field <- function(..., default = 0) return_field(d, ..., default = default)
  n <- nrow(d)
  put <- function(name, value) set(d, j = name, value = rep_len(value, n))

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
  } else classified_other %||% 0

  outputs <- c(colname_concessional, colname_div293_tax, colname_new_Taxable_Income)
  helpers <- c("concessional_cap", "excess_concessional_contributions", "rental_losses",
               "surchargeable_income_div293", "low_tax_contributions_div293", "div293_income")
  collisions <- intersect(c(outputs, helpers), names(d))
  if (colname_new_Taxable_Income == "Taxable_Income" && "Taxable_Income" %in% collisions) {
    warning("Dropping Taxable Income. Replacing it with modelled taxable income.")
    collisions <- setdiff(collisions, "Taxable_Income")
  }
  if (warn_if_colnames_overwritten && length(collisions)) {
    warning("Overwriting ", paste(collisions, collapse = ", "), " in .sample.file")
  }
  if (!is.null(reportable)) {
    put("SG_contributions", pmax(0, employer - reportable))
    put("salary_sacrifice_contributions", reportable)
  }
  put("personal_deductible_contributions", deductible)
  if (scale_contr_match_ato) {
    scale <- 1 + (super_contribution_inflator_1314 - 1) * .lambda
    employer <- employer * scale
    deductible <- deductible * scale
    for (name in intersect(c("sc_empl_cont", "Spr_Emplr_Contr", "MCS_Emplr_Contr",
                             "ds_pers_super_cont", "Non_emp_spr_amt", "sc_pers_cont_deductible"), names(d))) {
      set(d, j = name, value = d[[name]] * scale)
    }
  }
  put("non_concessional_contributions", pmax(0, personal - deductible))
  gross <- as.double(employer) + deductible + other
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
  reported_cap <- if (missing(cap) && missing(cap2) && missing(cap2_age)) d$sn_concess_cont_cap
  annual_cap <- cap
  if (age_based_cap && (is.null(reported_cap) || anyNA(reported_cap))) {
    band <- field("Age_range", "age_range", "Birth_year", default = NULL)
    age <- if (!is.null(band)) pmax(0, 70 - 5 * band)
    if (!is.null(d$age_range_description)) {
      description <- as.character(d$age_range_description)
      lower <- as.numeric(sub(" .*", "", sub("^under .*", "0", description)))
      age <- if (is.null(age)) lower else ifelse(is.na(age), lower, age)
    }
    age <- field("c_age_30_june", default = age)
    if (is.null(age)) stop("An age or age band is required for an age-based cap.")
    annual_cap <- ifelse(age > cap2_age & !is.na(age), cap2, cap)
  }
  if (!is.null(reported_cap)) annual_cap <- ifelse(is.na(reported_cap), annual_cap, reported_cap)
  if (!(length(annual_cap) %in% c(1L, n)) || anyNA(annual_cap) || any(annual_cap < 0)) {
    stop("Supply one nonnegative cap or a cap per taxpayer.")
  }
  put("concessional_cap", annual_cap)

  excess <- pmax(0, gross - annual_cap)
  low_tax <- pmax(0, gross - excess)
  previous_excess <- field("i_excess_super", "sc_excess_concess_cont")
  new_taxable <- as.double(taxable) - previous_excess + excess
  rent_loss <- field("it_property_loss", default = pmax(0, -field("is_net_rent", "Net_rent_amt")))
  fringe <- field("it_rept_fringe_benefit", "Rep_frng_ben_amt",
                  default = field("it_rept_fringe_benefit_exe") + field("it_rept_fringe_benefit_nexe"))
  surchargeable <- pmax(0, new_taxable) + fringe + rent_loss +
    field("it_invest_loss", "Net_fincl_invstmt_lss_amt", "Net_fincl_invstmnt_lss_amt") +
    field("as_family_trust_inc_txd") - field("zero_rate_super_lump_sum") - field("is_other_cat3_fhss")
  total <- surchargeable + low_tax
  put("excess_concessional_contributions", excess)
  put("rental_losses", rent_loss)
  put("surchargeable_income_div293", surchargeable)
  put("low_tax_contributions_div293", low_tax)
  put("div293_income", total)
  put(colname_concessional, low_tax)
  put(colname_div293_tax, 0.15 * pmin(low_tax, pmax(0, total - div293_threshold)))
  put(colname_new_Taxable_Income, new_taxable)
  if (drop_helpers) {
    for (name in setdiff(helpers[-1L], outputs)) set(d, j = name, value = NULL)
  }
  d
}
