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
#' @param inflate_contr_match_ato (logical) Should concessional contributions be inflated to match aggregates in 2013-14? That is, should concessional contributions by multipled by \code{grattan:::super_contribution_inflator_1314}, which was defined to be: \deqn{\frac{\textrm{Total assessable contributions in SMSF and funds}}{\textrm{Total contributions in 2013-14 sample file}}}{Total assessable contributions in SMSF and funds / Total contributions in 2013-14 sample file.}. 
#' @param .lambda Exponential weight applied to \code{concessional contributions}. 0 is equivalent to FALSE in \code{inflate_contr_match_ato}; 1 is equivalent to match.
#' @param reweight_contr_match_ato (logical) Should WEIGHT be inflated so as to match aggregates?
#' @param .mu Exponential weight for WEIGHT. Should be set so \eqn{\lambda + \mu = 1}. Failure to do so is a warning.
#' @param div293 (logical) Should Division 293 tax be calculated? If FALSE, \code{.sample.file} is returned immediately, with a warning (that you're using this function pointlessly!).
#' @param warn_if_colnames_overwritten (logical) Issue a warning if the construction of helper columns will overwrite existing column names in \code{.sample.file}.
#' @param drop_helpers (logical) Should columns used in the calculation be dropped before the sample file is returned?
#' @param copyDT (logical) Should the data table be \code{copy()}d? If the action of this data table is being compared, possibly useful.
#' @return A data frame comprising the original sample file (\code{.sample.file}) with an extra column equalling the Division 293 tax payable by that taxpayer.
#' @export 

apply_super_caps_and_div293 <- function(.sample.file, 
                                        colname_concessional = "concessional_contributions",
                                        colname_div293_tax = "div293_tax", 
                                        colname_new_Taxable_Income = "Taxable_income_for_ECT",
                                        div293_threshold = 300e3, 
                                        # for low income tax contributions amount
                                        cap = 30e3, cap2 = 35e3, age_based_cap = TRUE, cap2_age = 49, ecc = FALSE,
                                        use_other_contr = FALSE,
                                        inflate_contr_match_ato = FALSE, .lambda = 1, .mu = 1, reweight_contr_match_ato = FALSE,
                                        div293 = TRUE, warn_if_colnames_overwritten = TRUE, drop_helpers = FALSE, copyDT = TRUE){
  # Todo/wontfix
  if (!identical(ecc, FALSE)) stop("ECC not implemented.")
  
  # CRAN NOTE avoidance
  age_range_description <- concessional_cap <- div293_income <- div293_tax <-
    low_tax_contributions_div293 <- pre_tax_contributions <- rental_losses <-
    super_income_in_excess_of_cap <- surchargeable_income_div293 <-
    concessional_contributions <- new_Taxable_Income <- .new_Taxable_Income <-
    excess_concessional_contributions <- SG_contributions <- 
    salary_sacrifice_contributions <- personal_deductible_contributions <- 
    non_concessional_contributions <- Taxable_income_for_ECT <- NULL
  
  if (copyDT){
    .sample.file <- data.table::copy(.sample.file)
  }
  
  if (!data.table::is.data.table(.sample.file)){
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
  
  .sample.file[ , SG_contributions := pmaxC(MCS_Emplr_Contr - Rptbl_Empr_spr_cont_amt, 0)]
  .sample.file[ , salary_sacrifice_contributions := Rptbl_Empr_spr_cont_amt]
  .sample.file[ , personal_deductible_contributions := Non_emp_spr_amt]
  # Concessional contributions
  if (inflate_contr_match_ato && reweight_contr_match_ato){
    if (abs(.lambda + .mu - 1) > .Machine$double.eps ^ 0.5){
      warning(".lambda + .mu != 1\nWeighting may be wrong.")
    }
  }
  
  if (inflate_contr_match_ato){
    .sample.file[ , MCS_Emplr_Contr := MCS_Emplr_Contr * (super_contribution_inflator_1314 ^ .lambda) ]
    .sample.file[ , Non_emp_spr_amt := Non_emp_spr_amt * (super_contribution_inflator_1314 ^ .lambda) ]
  }
  
  .sample.file[ , concessional_contributions := MCS_Emplr_Contr + Non_emp_spr_amt]
  .sample.file[ , non_concessional_contributions := pmaxC(MCS_Prsnl_Contr - Non_emp_spr_amt, 0)]
  
  if (reweight_contr_match_ato){
    WEIGHT <- NULL
    if (!any("WEIGHT" == names(.sample.file))){
      warning("No WEIGHT found; using WEIGHT=50 in reweighting.")
      .sample.file[ , WEIGHT := 50]
    } 
    .sample.file[ , WEIGHT := WEIGHT * if_else(concessional_contributions > 0,
                                               (super_contribution_inflator_1314 ^ .mu), 
                                               1)]
  }

  if (age_based_cap){
    if (!any("age_range_description" == names(.sample.file))){
      
      # Following step to avoid having to require taxstats being attached (installed)
      # to enjoy decoding of age variable.  A bit strange (as it is very likely that
      # the user, who must have a sample file, would be using taxstats as well).
      if (!isTRUE(requireNamespace("taxstats")) && !exists("age_range_decoder")){
        age_range_decoder <- 
          structure(list(age_range = c(11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L, 0L), 
                         age_range_description = structure(1:12, 
                                                           .Label = c("under 20",  "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",  "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 and over"), 
                                                           class = c("ordered", "factor"))), 
                    .Names = c("age_range", "age_range_description"), 
                    class = c("data.frame", "data.table"), 
                    row.names = c(NA, -12L))
        
      }
      .sample.file <- merge(.sample.file, age_range_decoder, by = "age_range")
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
  
  
  data.table::setnames(.sample.file, "div293_tax", colname_div293_tax)
  data.table::setnames(.sample.file, "concessional_contributions", colname_concessional)
  if (!exists("orig_colname_new_Taxable_Income")){
    data.table::setnames(.sample.file, "Taxable_income_for_ECT", colname_new_Taxable_Income)
  } else {
    .sample.file[ , Taxable_Income := NULL]
    data.table::setnames(.sample.file, "Taxable_income_for_ECT", orig_colname_new_Taxable_Income)
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
