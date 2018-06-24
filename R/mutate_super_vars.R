#' @title Superannuation caps and Division 293 calculations
#'
#' @description Mutate a sample file to reflect particular caps on concessional contributions and applications of Division 293 tax.
#' @author Hugh Parsonage, William Young
#' @param .sample.file A data.table containing at least the variables \code{sample_file_1314} from the taxstats package.
#' @param colname_concessional The name for concessional contributions.
#' @param colname_div293_tax The name of the column containing the values of Division 293 tax payable for that taxpayer.
#' @param colname_new_Taxable_Income The name of the column containing the new Taxable Income.
#' @param div293_threshold The Division 293 threshold.
#' @param cap The cap on concessional contributions for all taxpayers if \code{age_based_cap} is FALSE, or for those below the age threshold otherwise.
#' @param cap2 The cap on concessional contributions for those above the age threshold. No effect if \code{age_based_cap} is FALSE.
#' @param age_based_cap Is the cap on concessional contributions age-based? 
#' @param cap2_age The age above which \code{cap2} applies.
#' @param ecc (logical) Should an excess concessional contributions charge be calculated? (Not implemented.)
#' @param use_other_contr Make a (poor) assumption that all 'Other contributions' (\code{MCS_Othr_Contr}) are concessional contributions. This may be a useful upper bound should such contributions be considered important.
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
#' @export 

apply_super_caps_and_div293 <- function(.sample.file, 
                                        colname_concessional = "concessional_contributions",
                                        colname_div293_tax = "div293_tax", 
                                        colname_new_Taxable_Income = "Taxable_income_for_ECT",
                                        div293_threshold = 300e3, 
                                        cap = 25e3, 
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
                                        copyDT = TRUE){
  # Todo/wontfix
  if (!identical(ecc, FALSE)) {
    stop("ECC not implemented.")
  }
  
  # CRAN NOTE avoidance
  age_range_description <- concessional_cap <- div293_income <- div293_tax <-
    low_tax_contributions_div293 <- pre_tax_contributions <- rental_losses <-
    super_income_in_excess_of_cap <- surchargeable_income_div293 <-
    concessional_contributions <- new_Taxable_Income <- .new_Taxable_Income <-
    excess_concessional_contributions <- SG_contributions <- 
    salary_sacrifice_contributions <- personal_deductible_contributions <- 
    non_concessional_contributions <- Taxable_income_for_ECT <- NULL
  
  if (copyDT){
    .sample.file <- copy(.sample.file)
  }
  
  if (!is.data.table(.sample.file)){
    stop(".sample.file must be a data.table")
  }
  
  # Ensure the user is appropriately warned if we are about to overwrite columns:
  new_colnames <- c("concessional_cap", "div293_income", "div293_tax", 
                    "low_tax_contributions_div293", "pre_tax_contributions", "rental_losses", 
                    "excess_concessional_contributions", "surchargeable_income_div293", 
                    "concessional_contributions", 
                    "SG_contributions",
                    "salary_sacrifice_contributions",
                    "personal_deductible_contributions",
                    ".new_Taxable_Income")
  
  common.indices <- new_colnames %in% names(.sample.file)
  
  if (warn_if_colnames_overwritten && any(common.indices)){
    warning("Overwriting", names(.sample.file)[common.indices], "in .sample.file")
  }
  
  if (colname_new_Taxable_Income == "Taxable_Income"){
    warning("Dropping Taxable Income.")
    orig_colname_new_Taxable_Income <- colname_new_Taxable_Income
    colname_new_Taxable_Income <- ".Taxable_Income"
  }
  
  if (any(c(colname_div293_tax, colname_concessional, colname_new_Taxable_Income) %in% names(.sample.file))){
    warning("Dropping requested column names.")
    .sample.file[ , which(names(.sample.file) %in% c(colname_div293_tax, colname_concessional, colname_new_Taxable_Income)) := NULL]
  }
  
  if (!all(c("MCS_Emplr_Contr", 
             "MCS_Prsnl_Contr", 
             "MCS_Othr_Contr", 
             "MCS_Ttl_Acnt_Bal") %in% names(.sample.file))){
    stop("The sample file you requested does not have the variables needed for this function.")
  }
  
  if ("Rptbl_Empr_spr_cont_amt" %in% names(.sample.file)){
    .sample.file[ , SG_contributions := pmaxC(MCS_Emplr_Contr - Rptbl_Empr_spr_cont_amt, 0)]
    .sample.file[ , salary_sacrifice_contributions := Rptbl_Empr_spr_cont_amt]
  }
  
  if ("Non_emp_spr_amt" %in% names(.sample.file))
    .sample.file[ , personal_deductible_contributions := Non_emp_spr_amt]
  
  # Concessional contributions
  if (scale_contr_match_ato) {
    .sample.file[ , MCS_Emplr_Contr := MCS_Emplr_Contr * (1 + (super_contribution_inflator_1314 - 1) * .lambda) ]
    .sample.file[ , Non_emp_spr_amt := Non_emp_spr_amt * (1 + (super_contribution_inflator_1314 - 1) * .lambda) ]
  }
  
  .sample.file[ , concessional_contributions := MCS_Emplr_Contr + Non_emp_spr_amt]
  .sample.file[ , non_concessional_contributions := pmaxC(MCS_Prsnl_Contr - Non_emp_spr_amt, 0)]
  
  if (impute_zero_concess_contr){
    # CRAN note avoidance.
    Sw_amt <- Rptbl_Empr_spr_cont_amt <- NULL
    
    if (!all(c("Rptbl_Empr_spr_cont_amt", "Sw_amt") %in% names(.sample.file))){
      stop("'Rptbl_Empr_spr_cont_amt' and 'Sw_amt' required to impute.")
    }
    # Imputation method: if the concessional contributions are absent, yet salary is high, 
    # replace the concessional contributions by super due SG and Rptbl_Empr_spr_cont_amt
    .sample.file[ , concessional_contributions := if_else(near(concessional_contributions, 0) & Sw_amt > .min.Sw.for.SG, 
                                                          as.double(Sw_amt * .SG_rate + Rptbl_Empr_spr_cont_amt), 
                                                          as.double(concessional_contributions))]
  }
  
  if (reweight_late_lodgers){
    WEIGHT <- NULL
    if (!any("WEIGHT" == names(.sample.file))){
      warning("No WEIGHT found; using WEIGHT=50 in reweighting.")
      .sample.file[ , WEIGHT := 50]
    } 
    .sample.file[ , WEIGHT := WEIGHT * .mu]
  }

  if (age_based_cap){
    if (!any("age_range_description" == names(.sample.file))){
      
      # Following step to avoid having to require taxstats being attached (installed)
      # to enjoy decoding of age variable.  A bit strange (as it is very likely that
      # the user, who must have a sample file, would be using taxstats as well).
      
      age_range_decoder2 <- 
        data.table(age_range = 0:11,
                   age_range_description = c("70 and over",
                                             paste0(seq(65, 20, by = -5), 
                                                    " to ", 
                                                    seq(69, 24, by = -5)), 
                                             "under 20"),
                   key = "age_range") %>%
        .[, age_range_description := factor(age_range_description, 
                                            levels = unique(rev(age_range_description)), 
                                            ordered = TRUE)]
      
      
      .sample.file <- merge(.sample.file, age_range_decoder2, by = "age_range")
    }
    
    # if an age falls between an age group, we assume ambiguously aged individuals are NOT entitled to cap2.
    if (cap2_age < 65 && cap2_age >= 20){
      cap2_age_group <- paste0(ceiling(cap2_age / 5) * 5, " to ", ceiling(cap2_age / 5) * 5 + 4)
    } else {
      if (cap2_age >= 65){
        cap2_age_group <- "70 and over"
      } else {
        cap2_age_group <- "under 20"
      }
    }
    
    .sample.file[ , concessional_cap := if_else(age_range_description >= cap2_age_group, cap2, cap)]
  } else {
    .sample.file[ , concessional_cap := cap]
  }
  
  # https://www.ato.gov.au/uploadedFiles/Content/TPALS/downloads/Division-293-scenario-table-v2.pdf
  .sample.file[ , excess_concessional_contributions := pmaxC(concessional_contributions - concessional_cap, 0)]
  .sample.file[ , rental_losses := -1 * pminC(Net_rent_amt, 0)]
  .sample.file[ , surchargeable_income_div293 := Taxable_Income + Net_fincl_invstmt_lss_amt + rental_losses + Rep_frng_ben_amt]
  if (use_other_contr){
    .sample.file[ , low_tax_contributions_div293 := pmaxC(concessional_contributions + MCS_Othr_Contr - excess_concessional_contributions, 0)]
  } else {
    .sample.file[ , low_tax_contributions_div293 := pmaxC(concessional_contributions - excess_concessional_contributions, 0)]
  }
  .sample.file[ , div293_income := surchargeable_income_div293 + low_tax_contributions_div293]
  .sample.file[ , div293_tax := if_else(div293_income > div293_threshold, 
                                        0.15 * pminV(low_tax_contributions_div293, 
                                                     pmaxC(div293_income - div293_threshold, 0)), 
                                        0)]
  
  # Modify taxable income to reflect exceeding cap:
  .sample.file[ , Taxable_income_for_ECT := Taxable_Income + excess_concessional_contributions]
  
  
  setnames(.sample.file, "div293_tax", colname_div293_tax)
  setnames(.sample.file, "concessional_contributions", colname_concessional)
  if (!exists("orig_colname_new_Taxable_Income")){
    setnames(.sample.file, "Taxable_income_for_ECT", colname_new_Taxable_Income)
  } else {
    .sample.file[ , Taxable_Income := NULL]
    setnames(.sample.file, "Taxable_income_for_ECT", orig_colname_new_Taxable_Income)
  }
  
  if (drop_helpers){
    .sample.file[ , c("excess_concessional_contributions",
                      "rental_losses", 
                      "surchargeable_income_div293", 
                      "low_tax_contributions_div293", 
                      "div293_income") := NULL]
  }
  
  return(.sample.file)
}
