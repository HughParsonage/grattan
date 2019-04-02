#' Modelled Income Tax
#' @description The income tax payable if tax settings are changed. 
#' @param sample_file A sample file having at least as many variables as the 2012-13 sample file.
#' @param baseline_fy If a parameter is not selected, the parameter's value in this tax year is used.
#' 
#' Must be a valid tax year and one for which \code{income_tax} has been programmed. 
#' @param elasticity_of_taxable_income Either \code{NULL} (the default), or a numeric vector the same length of \code{sample_file} (or length-1) providing the elasticity of taxable income for each observation in \code{sample_file}; 
#' \deqn{\frac{\Delta z / z}{\Delta \tau / (1 - \tau)}} 
#' where \eqn{z} is taxable income and \eqn{\tau} is tax payable.
#' 
#' For example, if, for a given taxpayer,
#' the tax settings would otherwise result in a 2\% decrease of disposable income
#' under the tax settings to be modelled, and \code{elasticity_of_taxable_income} is set to 0.1,
#' the \code{Taxable_Income} is reduced by 0.2\% before the tax rates are applied.
#' 
#' If \code{NULL}, an elasticity of 0 is used. 
#' @param n_dependants The number of dependants for each entry in \code{sample_file}.
#' @param ordinary_tax_thresholds A numeric vector specifying the lower bounds of the brackets for "ordinary tax" as defined by the Regulations.
#' The first element should be zero if there is a tax-free threshold.
#' @param ordinary_tax_rates The marginal rates of ordinary tax. The first element should be zero if there is a tax-free threshold. 
#' Since the temporary budget repair levy was imposed on a discrete tax bracket when it applied, it is not included in this function.

#' @param medicare_levy_taper The taper that applies between the \code{_lower} and \code{_upper} thresholds.
#' @param medicare_levy_rate The ordinary rate of the Medicare levy for taxable incomes above \code{medicare_levy_upper_threshold}.

#' @param medicare_levy_lower_threshold Minimum taxable income at which the Medicare levy will be applied.
#' @param medicare_levy_upper_threshold Minimum taxable income at which the Medicare levy will be applied at the full Medicare levy rate (2\% in 2015-16). Between this threshold and the \code{medicare_levy_lower_threshold}, a tapered rate applies, starting from zero and climbing to \code{medicare_levy_rate}. 

#' @param medicare_levy_lower_family_threshold,medicare_levy_upper_family_threshold The equivalent values for families.
#' @param medicare_levy_lower_sapto_threshold,medicare_levy_upper_sapto_threshold The equivalent values for SAPTO-eligible individuals (not families).
#' @param medicare_levy_lower_family_sapto_threshold,medicare_levy_upper_family_sapto_threshold The equivalent values for SAPTO-eligible individuals in a family.
#' @param medicare_levy_lower_up_for_each_child The amount to add to the \code{_family_threshold}s for each dependant child.
#' @param lito_max_offset The maximum offset available for low incomes.
#' @param lito_taper The taper to apply beyond \code{lito_min_bracket}.
#' @param lito_min_bracket The taxable income at which the value of the offset starts to reduce (from \code{lito_max_offset}).
#' @param lito_multi A list of two components, named \code{x} and \code{y}, giving the value of a \emph{replacement} for \code{lito} at specified points, which will be linked by a piecewise linear curve between the points specified. For example, to mimic LITO in 2015-16 (when the offset was \$445 for incomes below \$37,000, and afterwards tapered off to \$66,667), one would use \code{lito_multi = list(x = c(-Inf, 37e3, 200e3/3, Inf), y = c(445, 445, 0, 0))}. The reason the argument ends with \code{multi} is that it is intended to extend the original parameters of LITO so that multiple kinks (including ones of positive and negative gradients) can be modelled. 
#' 
#' @param Budget2018_lamington The Low Middle Income Tax Offset proposed in the 2018 Budget.
#' @param Budget2018_lito_202223 The LITO proposed for 2022-23 proposed in the 2018 Budget.
#' @param Budget2018_watr The "Working Australian Tax Refund" proposed in the Opposition Leader's Budget Reply Speech 2018.
#' 
#' @param sapto_eligible Whether or not each taxpayer in \code{sample_file} is eligible for \code{SAPTO}. 
#' If \code{NULL}, the default, then eligibility is determined by \code{age_range} in \code{sample_file};
#' \emph{i.e.}, if \code{age_range <= 1} then the taxpayer is assumed to be eligible for SAPTO.
#' @param sapto_max_offset The maximum offset available through SAPTO. 
#' @param sapto_lower_threshold The threshold at which SAPTO begins to reduce (from \code{sapto_max_offset}).
#' @param sapto_taper The taper rate beyond \code{sapto_lower_threshold}.
#' 
#' @param sapto_max_offset_married,sapto_lower_threshold_married,sapto_taper_married As above,
#' but applied to members of a couple
#' @param sbto_discount The \code{tax_discount} in \code{\link{small_business_tax_offset}}.
#' @param cgt_discount_rate (numeric(1)) The capital gains tax discount rate, currently 50\%. 
#' 
#' @param calc_baseline_tax (logical, default: \code{TRUE}) Should the income tax in \code{baseline_fy} be included as a column in the result?
#' @param return. What should the function return? One of \code{tax}, \code{sample_file}, or \code{sample_file.int}. 
#' If \code{tax}, the tax payable under the settings; if \code{sample_file}, the \code{sample_file},
#' but with variables \code{tax} and possibly \code{new_taxable_income}; if \code{sample_file.int}, same as \code{sample_file} but \code{new_tax} is coerced to integer.
#' @param clear_tax_cols If \code{TRUE}, the default, then \code{return. = sample_file} implies any columns called \code{new_tax} or \code{baseline_tax} in \code{sample_file} are dropped silently.
#' @param warn_upper_thresholds If \code{TRUE}, the default, then any inconsistency between \code{baseline_fy} and the upper thresholds result in a warning. Set to \code{FALSE}, if the \code{lower_threshold}s may take priority. 
#' @param .debug Return a data.table of \code{new_tax}. Experimental so cannot be relied in future versions.
#' 
#' @examples
#' 
#' library(data.table)
#' library(hutils)
#' 
#' # With new tax-free threshold of $20,000:
#' if (requireNamespace("taxstats", quietly = TRUE)) {
#'   library(taxstats)
#'   library(magrittr)
#'      
#'   model_income_tax(sample_file_1314,
#'                    "2013-14",
#'                    ordinary_tax_thresholds = c(0, 20e3, 37e3, 80e3, 180e3)) %>%
#'     select_grep("tax", "Taxable_Income")
#' 
#' }
#' 
#' @export



model_income_tax <- function(sample_file,
                             baseline_fy,
                             n_dependants = 0L,
                             elasticity_of_taxable_income = NULL,
                             
                             ordinary_tax_thresholds = NULL,
                             ordinary_tax_rates = NULL,
                             
                             
                             medicare_levy_taper = NULL, 
                             medicare_levy_rate = NULL,
                             medicare_levy_lower_threshold = NULL,
                             medicare_levy_upper_threshold = NULL,
                             
                             medicare_levy_lower_sapto_threshold = NULL,
                             medicare_levy_upper_sapto_threshold = NULL,
                             
                             medicare_levy_lower_family_threshold = NULL,
                             medicare_levy_upper_family_threshold = NULL,
                             
                             medicare_levy_lower_family_sapto_threshold = NULL,
                             medicare_levy_upper_family_sapto_threshold = NULL,
                             medicare_levy_lower_up_for_each_child = NULL,
                             
                             
                             lito_max_offset = NULL,
                             lito_taper = NULL,
                             lito_min_bracket = NULL,
                             lito_multi = NULL,
                             
                             Budget2018_lamington = FALSE,
                             Budget2018_lito_202223 = FALSE,
                             Budget2018_watr = FALSE,
                             
                             sapto_eligible = NULL,
                             sapto_max_offset = NULL,
                             sapto_lower_threshold = NULL,
                             sapto_taper = NULL,
                             sapto_max_offset_married = NULL,
                             sapto_lower_threshold_married = NULL,
                             sapto_taper_married = NULL,
                             
                             sbto_discount = NULL,
                             
                             cgt_discount_rate = NULL,
                             
                             calc_baseline_tax = TRUE,
                             return. = c("sample_file", "tax", "sample_file.int"),
                             clear_tax_cols = TRUE,
                             warn_upper_thresholds = TRUE,
                             .debug = FALSE) {
  arguments <- ls()
  argument_vals <- as.list(environment())
  return. <- match.arg(return.)
  
  `%|||%` <- function(lhs, rhs) {
    if (is.null(lhs)) {
      rep_len(rhs, max.length)
    } else {
      rep_len(lhs, max.length)
    }
  }
  
  stopifnot(is.data.table(sample_file))
  sample_file <- copy(sample_file)
  max.length <- nrow(sample_file)
  .dots.ATO <- sample_file
  sample_file_noms <- copy(names(sample_file))
  
  if (clear_tax_cols) {
    if ("new_tax" %chin% sample_file_noms) {
      sample_file[, "new_tax" := NULL]
    }
    if ("baseline_tax" %chin% sample_file_noms) {
      sample_file[, "baseline_tax" := NULL]
    }
  }
  
  
  s1213_noms <-
    c(#"Gender",
      # "age_range",
      "Occ_code", "Partner_status", 
      "Region", "Lodgment_method", "PHI_Ind", "Sw_amt", "Alow_ben_amt", 
      "ETP_txbl_amt", "Grs_int_amt", "Aust_govt_pnsn_allw_amt", "Unfranked_Div_amt", 
      "Frk_Div_amt", "Dividends_franking_cr_amt", "Net_rent_amt", "Gross_rent_amt", 
      "Other_rent_ded_amt", "Rent_int_ded_amt", "Rent_cap_wks_amt", 
      "Net_farm_management_amt", "Net_PP_BI_amt", "Net_NPP_BI_amt", 
      "Total_PP_BI_amt", "Total_NPP_BI_amt", "Total_PP_BE_amt", "Total_NPP_BE_amt", 
      "Net_CG_amt", "Tot_CY_CG_amt", "Net_PT_PP_dsn", "Net_PT_NPP_dsn", 
      "Taxed_othr_pnsn_amt", "Untaxed_othr_pnsn_amt", "Other_foreign_inc_amt", 
      "Other_inc_amt", "Tot_inc_amt", "WRE_car_amt", "WRE_trvl_amt", 
      "WRE_uniform_amt", "WRE_self_amt", "WRE_other_amt", "Div_Ded_amt", 
      "Intrst_Ded_amt", "Gift_amt", "Non_emp_spr_amt", "Cost_tax_affairs_amt", 
      "Other_Ded_amt", "Tot_ded_amt", "PP_loss_claimed", "NPP_loss_claimed", 
      "Rep_frng_ben_amt", # "Med_Exp_TO_amt", "Asbl_forgn_source_incm_amt", 
      # "Spouse_adjusted_taxable_inc",
      "Net_fincl_invstmt_lss_amt", "Rptbl_Empr_spr_cont_amt", 
      "Cr_PAYG_ITI_amt", "TFN_amts_wheld_gr_intst_amt", "TFN_amts_wheld_divs_amt", 
      "Hrs_to_prepare_BPI_cnt", "Taxable_Income", "Help_debt")
  
  if (!all(s1213_noms %chin% sample_file_noms)) {
    absent_cols <- setdiff(s1213_noms, sample_file_noms)
    stop("`sample_file` lacked the following required columns:\n\t",
         paste0(absent_cols, collapse = "\n\t"), ".\n")
  }
  
  
  income <- sample_file[["Taxable_Income"]]
  max.length <- length(income)
  prohibit_vector_recycling(income, n_dependants, baseline_fy)
  
  old_tax <- income_tax(income,
                        fy.year = baseline_fy,
                        .dots.ATO = .dots.ATO,
                        n_dependants = n_dependants)
  if (calc_baseline_tax && return. != "tax") {
    set(sample_file,
        j = "baseline_tax",
        value = switch(return.,
                       "sample_file" = old_tax,
                       "sample_file.int" = as.integer(old_tax)))
  }
  
  # Recalculate the taxable income
  # CG adjustment
  if (!is.null(cgt_discount_rate)) {
    if (length(cgt_discount_rate) != 1L) {
      if (length(cgt_discount_rate) != max.length) {
        stop("`length(cgt_discount_rate) = ", length(cgt_discount_rate), "`, ", 
             "yet `nrow(sample_file) = ", max.length, "`. ",
             "Ensure `cgt_discount_rate` has length one.")
      }
    }
    
    if (!is.numeric(cgt_discount_rate)) {
      stop("`cgt_discount_rate` was type ", typeof(cgt_discount_rate), ", ",
           "but must be numeric. Ensure `cgt_discount_rate`, if used, is numeric.")
    }
    
    extra_Net_CG_amt <- function(DT,
                                 new_rate = cgt_discount_rate,
                                 old_rate = 0.5) {
      # Return the extra amount of capital gains
      # payable under `new_rate` compared to `old_rate`.
      # That which will be added to both Net_CG_amt and Taxable_Income
      # in the resultant sample file.
      
      
      # Use this function to avoid copying
      
      Net_CG_amt. <- .subset2(DT, "Net_CG_amt")
      Tot_CY_CG_amt. <- .subset2(DT, "Tot_CY_CG_amt")
      income <- .subset2(DT, "Taxable_Income")
      
      # Assume a CGT discount is applicable 
      
      has_discount <-
        if (min(Tot_CY_CG_amt.) == 0) {
          Tot_CY_CG_amt. > Net_CG_amt.
        } else {
          and(Tot_CY_CG_amt. > Net_CG_amt.,
              Tot_CY_CG_amt. > 0)
        }
      
      out <- integer(max.length)
      
      out[has_discount] <-
          as.integer(Net_CG_amt.[has_discount] * {{1 - new_rate} / {1 - old_rate} - 1})
      
      
      
      for (j in c("Net_CG_amt", "Tot_inc_amt", "Taxable_Income")) {
        v <- .subset2(DT, j)
        set(DT, 
            j = j,
            value = as.integer(v) + out)
      }
      
      # Taxable Income cannot be negative
      DT[, Taxable_Income := as.integer(pmax.int(Taxable_Income, 0L))]
      
      DT[]
    }
    extra_Net_CG_amt(sample_file)
    # Need to update since the Taxable Income is now different.
    income <- .subset2(sample_file, "Taxable_Income")
  }
  
  if (is.null(sapto_eligible)) {
    if ("age_range" %chin% names(sample_file)) {
      sapto_eligible <- .subset2(sample_file, "age_range") <= 1L
    } else {
      warning("Assuming everyone is ineligible for SAPTO.")
      sapto_eligible <- logical(max.length)
    }
  } else {
    if (!is.logical(sapto_eligible)) {
      stop("`sapto_eligible` was not a logical vector.")
    }
    if (length(sapto_eligible) != 1L && length(sapto_eligible) != max.length) {
      stop("`sapto_eligible` was length ", length(sapto_eligible), ". ",
           "Ensure `sapto_eligible` has length ",
           max.length, "(i.e. nrow(sample_file)) or length one.")
    }
  }
  
  

  
  ordering <- NULL
  
  if (identical(key(sample_file), "Taxable_Income")) {
    input <- 
      setDT(list(income = income, 
                 fy_year = rep(baseline_fy, times = max.length),
                 SaptoEligible = sapto_eligible))
    setattr(input, "sorted", c("fy_year", "income"))
  } else {
    input <-
      data.table(income = income,
                 fy_year = baseline_fy, 
                 SaptoEligible = sapto_eligible) %>%
      .[, "ordering" := .I] %>%
      setkeyv(c("fy_year", "income"))
  }
  
  # Check base tax
  if (!is.null(ordinary_tax_thresholds) || !is.null(ordinary_tax_rates)) {
    
    # accessing the baseline rates is expensive if not necessary
    if (is.null(ordinary_tax_thresholds) || is.null(ordinary_tax_rates)) {
      tax_table2_fy <- tax_table2[fy_year == baseline_fy]
      
      Thresholds <- ordinary_tax_thresholds %||% tax_table2_fy[["lower_bracket"]]
      Rates <-  ordinary_tax_rates %||% tax_table2_fy[["marginal_rate"]]
    } else {
      Thresholds <- ordinary_tax_thresholds
      Rates <- ordinary_tax_rates
    }
    
    if (length(Thresholds) != length(Rates)) {
      stop("`ordinary_tax_thresholds` and `ordinary_tax_rates` had different lengths. ",
           "Specify numeric vectors of equal length (or NULL).")
    }
    
    base_tax. <-
      IncomeTax(income,
                thresholds = Thresholds,
                rates = Rates)
    temp_budget_repair_levy. <- 0
  } else {
    tax_at <- lower_bracket <- marginal_rate <- NULL
    if ("ordering" %chin% names(input)) {
      base_tax. <- 
        tax_table2[input, roll = Inf] %>%
        .[, .(ordering, tax = tax_at + (income - lower_bracket) * marginal_rate)] %>%
        setorderv("ordering") %>%
        .subset2("tax")
    } else {
      base_tax. <- 
        tax_table2[input, roll = Inf] %>%
        .[, .(tax = tax_at + (income - lower_bracket) * marginal_rate)] %>%
        .subset2("tax") 
    }
    
    temp_budget_repair_levy. <-
      and(input[["fy_year"]] %chin% c("2014-15", "2015-16", "2016-17"),
          income > 180e3) * 
      (0.02 * (income - 180e3))
  }
  
  WEIGHTj <- which(names(.dots.ATO) == "WEIGHT")
  if (!length(WEIGHTj)) {
    WEIGHTj <- 0L
  }
  
  # If .dots.ATO  is NULL, for loops over zero-length vector
  # Use integers since that's what tax forms use.
  for (j in which(vapply(.dots.ATO, FUN = is.double, logical(1)))) {
    if (j != WEIGHTj) {
      set(.dots.ATO, j = j, value = as.integer(.dots.ATO[[j]]))
    }
  }
  
  if (is.null(.dots.ATO) ||
      "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)) {
    the_spouse_income <- integer(max.length)
  } else {
    the_spouse_income <- .dots.ATO[["Spouse_adjusted_taxable_inc"]]
    the_spouse_income[is.na(the_spouse_income)] <- 0L
  }
  
  
  # Check medicare levy
  medicare_args <- mget(grep("^medicare_levy", arguments, perl = TRUE, value = TRUE))
  
  if (all(vapply(medicare_args, is.null, FALSE))) {
    medicare_levy. <- 
      medicare_levy(income, 
                    Spouse_income = the_spouse_income,
                    fy.year = baseline_fy, 
                    sapto.eligible = sapto_eligible, 
                    family_status = {
                      FS <- rep_len("individual", max.length)
                      FS[the_spouse_income > 0] <- "family"
                      FS
                    }, 
                    n_dependants = n_dependants, 
                    .checks = FALSE)
    
  } else {
    medicare_tbl_fy <- medicare_tbl[input, on = c("fy_year", "sapto==SaptoEligible")]
    
    if ("ordering" %chin% names(input)) {
      setorderv(medicare_tbl_fy, "ordering")
    }
    
    # When a parameter is selected it must satisfy the 
    # conditions of the low-income area
    
    #    |                   .
    #    |              .
    # rb |         .
    #    |       /
    #    |      /
    #    |-----/
    #    0   a  b
    # 
    # t(b - a) = rb
    #
    
    # Here, we test whether or not the conditions are so satisfied; 
    # if not, we fix one of the parameters that is not specified to
    # satisfy the relation, with a warning and a prayer to change
    # the relevant the parameter.
    
    
    # Individuals
    ma <- medicare_levy_lower_threshold %|||% medicare_tbl_fy[["lower_threshold"]]
    mb <- medicare_levy_upper_threshold %|||% medicare_tbl_fy[["upper_threshold"]]
    mt <- medicare_levy_taper %|||% medicare_tbl_fy[["taper"]]
    mr <- medicare_levy_rate  %|||% medicare_tbl_fy[["rate"]]
    
    # Individuals - SAPTO
    # N.B. medicare_tbl_fy[["lower/upper_threshold"]] since the join above correctly identifies which ones
    msa <- medicare_levy_lower_sapto_threshold %|||% medicare_tbl_fy[["lower_threshold"]]
    msb <- medicare_levy_upper_sapto_threshold %|||% medicare_tbl_fy[["upper_threshold"]]
    
    ma <- as.integer(ma)
    msa <- as.integer(msa)
    mb <- as.integer(mb - 1)
    msb <- as.integer(msb - 1)
    
    
    # Families
    mfa <- medicare_levy_lower_family_threshold %|||% medicare_tbl_fy[["lower_family_threshold"]]
    mfb <- medicare_levy_upper_family_threshold %|||% medicare_tbl_fy[["upper_family_threshold"]]
    
    # Families - SAPTO
    mfsa <- medicare_levy_lower_family_sapto_threshold %|||% medicare_tbl_fy[["lower_family_threshold"]]
    mfsb <- medicare_levy_upper_family_sapto_threshold %|||% medicare_tbl_fy[["upper_family_threshold"]]
    
    mfa <- as.integer(mfa)
    mfb <- as.integer(mfb - 1)
    mfsa <- as.integer(mfsa)
    mfsb <- as.integer(mfsb - 1)
    
    
    medicare_parameter_roots <- 
      if_else(sapto_eligible,
              if_else(the_spouse_income > 0,
                      abs(mt * (mfsb - mfsa) - mr * mfsb),
                      abs(mt * (msb - msa) - mr * msb)),
              if_else(the_spouse_income > 0,
                      abs(mt * (mfb - mfa) - mr * mfb),
                      abs(mt * (mb - ma) - mr * mb)))
    
    if (any(medicare_parameter_roots > 1)) {
      # model is misspecified in respect of offsets etc
      
      warning_if_misspecified <- function(the_arg) {
        se <- sapto_eligible
        f. <- the_spouse_income > 0
        nor <- function(x, y) and(!x, !y)
        val <- switch(the_arg, 
                      "medicare_levy_upper_threshold" = mb[nor(se, f.)], 
                      "medicare_levy_lower_threshold" = ma[nor(se, f.)], 
                      "medicare_levy_upper_sapto_threshold" = msb[se & !f.], 
                      "medicare_levy_lower_sapto_threshold" = msa[se & !f.], 
                      "medicare_levy_upper_family_threshold" = mfb[!se & f.], 
                      "medicare_levy_lower_family_threshold" = mfa[!se & f.], 
                      "medicare_levy_upper_family_sapto_threshold" = mfsb[se & f.], 
                      "medicare_levy_lower_family_sapto_threshold" = mfsa[se & f.], 
                      "medicare_levy_taper" = mt,
                      "medicare_levy_rate" = mr)
        
        if (warn_upper_thresholds || !grepl("upper", the_arg)) {
          if (uniqueN(val) == 1L) {
            warning("`", the_arg, "` was not specified, ",
                    "but its default value would be inconsistent with the parameters that were specified.\n", 
                    "Its value has been set to:\n\t",
                    the_arg, " = ", round(val[1], digits = if (val[1] < 1) 2 else 0),
                    call. = FALSE)
          } else {
            warning("`", the_arg, "` was not specified, ",
                    "but its default values would be inconsistent with the parameters that were specified.\n",
                    "Its values have been set to: ",
                    "\n\t", paste0(utils::head(unique(round(val), 5)), 
                                   "...", 
                                   utils::tail(unique(round(val), 5)),
                                   collapse = "\n\t"),
                    "\n\t\t (First and last five shown.)",
                    call. = FALSE)
          }
        }
      }
      
      # Could be a problem with the individual parameter changes, or 
      # with the family ones. Do one at a time.
      if (any(medicare_parameter_roots[the_spouse_income == 0L] > 1)) {
        if (any(medicare_parameter_roots[and(the_spouse_income == 0L,
                                             !sapto_eligible)] > 1)) {
          # Individual thresholds
          if (is.null(medicare_levy_upper_threshold)) {
            mb <- mt * ma / (mt - mr)
            warning_if_misspecified("medicare_levy_upper_threshold")
            
          } else {
            
            if (is.null(medicare_levy_lower_threshold)) {
              ma <- mb * (mt - mr) / mt
              warning_if_misspecified("medicare_levy_lower_threshold")
              
            } else {
              
              if (is.null(medicare_levy_taper)) {
                mt <- mr * mb / (mb - ma)
                warning_if_misspecified("medicare_levy_taper")
                
              } else {
                
                if (is.null(medicare_levy_rate)) {
                  mr <- mt * (mb - ma) / mb
                  warning_if_misspecified("medicare_levy_rate")
                  # nocov start
                  # alternative not reachable
                } else stop("ERR # e59ed9845068f337d6653a7cc00401e1dbeeda7d. ",
                            "Please contact `grattan` package maintainer.") 
                # nocov end
              }
            }
          }
        }
        
        if (any(medicare_parameter_roots[and(the_spouse_income == 0L,
                                             sapto_eligible)] > 1)) {
          # SAPTO non-families
          if (is.null(medicare_levy_upper_sapto_threshold)) {
            msb <- mt * msa / (mt - mr)
            warning_if_misspecified("medicare_levy_upper_sapto_threshold")
            
          } else {
            
            if (is.null(medicare_levy_lower_sapto_threshold)) {
              msa <- msb * (mt - mr) / mt
              warning_if_misspecified("medicare_levy_lower_sapto_threshold")
              
            } else {
              medicare_levy_taper_stop <- 
                round(mr * msb / (msb - msa), 3)
              
              stop("Medicare levy parameter mismatch could not be safely resolved.\n\n",
                   "`medicare_levy_upper_sapto_threshold` and ",
                   "`medicare_levy_lower_sapto_threshold` were both supplied, ",
                   "but imply a Medicare taper rate of\n\t",
                   if (uniqueN(medicare_levy_taper_stop) == 1L) {
                     unique(medicare_levy_taper_stop)
                   } else {
                     paste0(paste0(utils::head(unique(medicare_levy_taper_stop)),
                                   collapse = "\n\t"),
                            " (first 6 shown)")
                   },
                   "\t (to 3 decimal places)\n",
                   "Either supply Medicare levy parameters compatible with this taper rate, ",
                   "or change `medicare_levy_taper` (which may force other parameters to", 
                   " change too).")
            }
          }
        }
      }
      
      if (any(medicare_parameter_roots[the_spouse_income > 0L] > 1)) {
        # Family thresholds only
        if (any(medicare_parameter_roots[and(the_spouse_income > 0L,
                                             !sapto_eligible)] > 1)) {
          if (is.null(medicare_levy_upper_family_threshold)) {
            mfb <- mt * mfa / (mt - mr)
            warning_if_misspecified("medicare_levy_upper_family_threshold")
            
          } else {
            
            if (is.null(medicare_levy_lower_family_threshold)) {
              mfa <- mfb * (mt - mr) / mt
              warning_if_misspecified("medicare_levy_lower_family_threshold")
              
            } else {
              medicare_levy_taper_stop <- 
                round(mr * mfb / (mfb - mfa), 3)
              stop("Medicare levy parameter mismatch could not be safely resolved.\n\n",
                   "`medicare_levy_upper_family_threshold` and ",
                   "`medicare_levy_lower_family_threshold` were both supplied, ",
                   "but imply a Medicare taper rate of\n\t",
                   if (uniqueN(medicare_levy_taper_stop) == 1L) {
                     unique(medicare_levy_taper_stop)
                   } else {
                     paste0(paste0(utils::head(unique(medicare_levy_taper_stop)),
                                   collapse = "\n\t"),
                            " (first 6 shown)")
                   }, "\t (to 3 decimal places)\n",
                   "Either supply Medicare levy parameters compatible with this taper rate, ",
                   "or change `medicare_levy_taper` (which may force other parameters to", 
                   " change too).")
            }
          }
        }
        
        # Family - SAPTO
        if (any(medicare_parameter_roots[and(the_spouse_income > 0L,
                                             sapto_eligible)] > 1)) {
          if (is.null(medicare_levy_upper_family_sapto_threshold)) {
            mfsb <- mt * mfsa / (mt - mr)
            warning_if_misspecified("medicare_levy_upper_family_sapto_threshold")
            
          } else {
            
            if (is.null(medicare_levy_lower_family_sapto_threshold)) {
              mfsa <- mfsb * (mt - mr) / mt
              warning_if_misspecified("medicare_levy_lower_family_sapto_threshold")
              
            } else {
              medicare_levy_taper_stop <- round(mr * mfsb / (mfsb - mfsa), 3)[sapto_eligible]
              
              stop("Medicare levy parameter mismatch could not be safely resolved.\n\n",
                   "`medicare_levy_upper_family_sapto_threshold` and ",
                   "`medicare_levy_lower_family_sapto_threshold` were both supplied, ",
                   "but imply a Medicare taper rate of\n\t",
                   if (uniqueN(medicare_levy_taper_stop) == 1L) {
                     unique(medicare_levy_taper_stop)
                   } else {
                     paste0(paste0(utils::head(unique(medicare_levy_taper_stop)),
                                   collapse = "\n\t"),
                            " (first 6 shown)")
                   }, "\t (to 3 decimal places)\n",
                   "Either supply Medicare levy parameters compatible with this taper rate, ",
                   "or change `medicare_levy_taper` (which may force other parameters to", 
                   " change too).")
            }
          }
          
        }
      }
    }
    
    if (any(sapto_eligible)) {
      ma[sapto_eligible] <- msa[sapto_eligible]
      mb[sapto_eligible] <- msb[sapto_eligible]
      mfa[sapto_eligible] <- mfsa[sapto_eligible]
      mfb[sapto_eligible] <- mfsb[sapto_eligible]
    }
    
    medicare_levy. <-
      MedicareLevy(income = income,
                   
                   lowerThreshold = ma,
                   upperThreshold = mb,
                   
                   SpouseIncome = the_spouse_income,
                   isFamily = the_spouse_income > 0,
                   NDependants = if (length(n_dependants) == 1) rep_len(n_dependants, max.length) else n_dependants,
                   
                   lowerFamilyThreshold = mfa,
                   upperFamilyThreshold = mfb,
                   lowerUpForEachChild  = medicare_levy_lower_up_for_each_child %|||% medicare_tbl_fy[["lower_up_for_each_child"]], 
                   
                   rate = mr,
                   taper = mt)
  }
  
  lito_args <- mget(grep("^lito_", arguments, perl = TRUE, value = TRUE))
  
  if (all(vapply(lito_args, is.null, FALSE))) {
    setkeyv(input, "fy_year")
    lito. <- .lito(input)
  } else {
    lito_fy <- lito_tbl[fy_year == baseline_fy]
    lito. <- lito(income,
                  max_lito = lito_max_offset %||% lito_fy[["max_lito"]],
                  lito_taper = lito_taper %||% lito_fy[["lito_taper"]],
                  min_bracket = lito_min_bracket %||% lito_fy[["min_bracket"]])
  }
  
  
  sapto. <- double(max.length)
  if (any(sapto_eligible)) {
    sapto_args <- mget(grep("^sapto_(?!eligible)", arguments, perl = TRUE, value = TRUE))
    
    if (all(vapply(sapto_args, is.null, FALSE))) {
      # 
      .dASE <- .dots.ATO[(sapto_eligible),
                         .SD, 
                         .SDcols = c("Rptbl_Empr_spr_cont_amt", 
                                     "Net_fincl_invstmt_lss_amt",
                                     "Net_rent_amt",
                                     "Rep_frng_ben_amt")]
      rebate_income_over_eligible <-
        rebate_income(Taxable_Income = income[sapto_eligible],
                      Rptbl_Empr_spr_cont_amt = .dASE[["Rptbl_Empr_spr_cont_amt"]],
                      Net_fincl_invstmt_lss_amt = .dASE[["Net_fincl_invstmt_lss_amt"]],
                      Net_rent_amt = .dASE[["Net_rent_amt"]],
                      Rep_frng_ben_amt = .dASE[["Rep_frng_ben_amt"]])
      
      sapto.[sapto_eligible] <-
        sapto(rebate_income = rebate_income_over_eligible, 
              fy.year = if (length(baseline_fy) > 1) baseline_fy[sapto_eligible] else baseline_fy,
              Spouse_income = the_spouse_income[sapto_eligible],
              family_status = {
                FS_sapto <- rep_len("single", max.length)
                FS_sapto[the_spouse_income > 0] <- "married"
                FS_sapto[sapto_eligible]
              },
              sapto.eligible = TRUE)
    } else {
      sapto_tbl_fy <- sapto_tbl[fy_year == baseline_fy]
      sapto_married_fy <- sapto_tbl_fy[family_status == "married"]
      sapto_single_fy <- sapto_tbl_fy[family_status == "single"]
      sapto_married <- sapto_eligible & the_spouse_income > 0L
      # Need to expand length-one arguments so that [] subsetting
      # does not improperly introduce NAs
      sapto. <-
        do_sapto_rcpp2(RebateIncome = rebate_income(Taxable_Income = income, 
                                                    Rptbl_Empr_spr_cont_amt = .dots.ATO[["Rptbl_Empr_spr_cont_amt"]],
                                                    Net_fincl_invstmt_lss_amt = .dots.ATO[["Net_fincl_invstmt_lss_amt"]],
                                                    Net_rent_amt = .dots.ATO[["Net_rent_amt"]],
                                                    Rep_frng_ben_amt = .dots.ATO[["Rep_frng_ben_amt"]]),
                       
                       maxOffsetSingle =                     sapto_max_offset %||% sapto_single_fy[["max_offset"]],
                       maxOffsetMarried =            sapto_max_offset_married %||% sapto_married_fy[["max_offset"]],
                       lowerThresholdSingle =           sapto_lower_threshold %||% sapto_single_fy[["lower_threshold"]],
                       lowerThresholdMarried = sapto_lower_threshold_married %||% sapto_married_fy[["lower_threshold"]],
                       taperRateSingle =                          sapto_taper %||% sapto_single_fy[["taper_rate"]],
                       taperRateMarried =                 sapto_taper_married %||% sapto_married_fy[["taper_rate"]],
                       SaptoEligible = sapto_eligible,
                       IsMarried = the_spouse_income > 0L,
                       SpouseIncome = the_spouse_income)
      
    }
  }
  
  new_tax <-
  {
    if (baseline_fy == "2011-12") {
      flood_levy. <- 0.005 * {pmaxC(income - 50e3, 0) + pmaxC(income - 100e3, 0)}
    } else {
      flood_levy. <- 0
    }
    
    lamington_offset. <-
      if (Budget2018_lamington) {
        lmito(income, fy.year = baseline_fy)
      } else {
        0
      }
    
    watr. <- 
      if (Budget2018_watr) {
        watr(income)
      } else {
        0
      }
    
    if (!is.null(lito_multi)) {
      if (!is.null(lito_max_offset)) {
        stop("`lito_multi` is not NULL, yet neither is `lito_max_offset`. ",
             "Either set `lito_max_offset` to NULL or `lito_multi`.")
      }
      if (!is.null(lito_taper)) {
        stop("`lito_multi` is not NULL, yet neither is `lito_taper`. ",
             "Either set `lito_taper` to NULL or `lito_multi`.")
      }
      if (!is.null(lito_min_bracket)) {
        stop("`lito_multi` is not NULL, yet neither is `lito_min_bracket`. ",
             "Either set `lito_min_bracket` to NULL or `lito_multi`.")
      }
      
      if (!is.list(lito_multi)) {
        stop("`lito_multi` had class ", class(lito_multi), ". Must be a list.")
      }
      if (!length(names(lito_multi))) {
        stop("`lito_multi` had no names. ", 
             "When `lito_multi` is set, it be a list with two elements named 'x' and 'y'.")
      }
      
      if (!identical(names(lito_multi), c("x", "y"))) {
        stop("`names(lito_multi) = ", paste0(names(lito_multi)[1:2], collapse = ","), "`. ", 
             "Set the names as 'x' and 'y'.")
      }
      
      lito_multi_x <- lito_multi[["x"]]
      lito_multi_y <- lito_multi[["y"]]
      
      # Infinity should be permitted, but won't work with approxfun
      lito_multi_x[which.min(lito_multi_x)] <- min(income)
      lito_multi_x[which.max(lito_multi_x)] <- max(income)
      
      lito. <-
        stats::approxfun(x = lito_multi_x, 
                         y = lito_multi_y)(income)
      
    } else if (Budget2018_lito_202223) {
      lito. <- 
        pmaxC(pmaxV(lito(income, max_lito = 645, lito_taper = 0.065, min_bracket = 37e3),
                    lito(income, max_lito = 385, lito_taper = 0.015, min_bracket = 41e3)),
              0)
    }
    
    # http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html
    S4.10_basic_income_tax_liability <-
      pmaxC(base_tax. - lito. - lamington_offset. - watr. - sapto., 0)
    
    # SBTO can only be calculated off .dots.ATO
    sbto. <-
      small_business_tax_offset(taxable_income = income,
                                basic_income_tax_liability = S4.10_basic_income_tax_liability,
                                .dots.ATO = .dots.ATO,
                                fy_year = if (is.null(sbto_discount)) baseline_fy,
                                tax_discount = sbto_discount)
    
    new_tax <- 
      pmaxC(S4.10_basic_income_tax_liability - sbto., 0) +
      temp_budget_repair_levy. + 
      medicare_levy. +
      flood_levy.
    
    if (.debug) {
      return(data.table(Ind = if ("Ind" %in% names(.dots.ATO)) .dots.ATO[["Ind"]] else -1L, 
                        income, 
                        old_tax,
                        new_tax,
                        base_tax.,
                        lito.,
                        lamington_offset.,
                        sapto.,
                        sbto.,
                        medicare_levy.))
    }
    
    new_tax  
  }
  # new_tax2 <<- new_tax
  
  # Elasticity of Taxable Income
  ## 
  if (!is.null(elasticity_of_taxable_income)) {
    D_tax <- new_tax - old_tax
    # Change in net income
    new_taxable_income <-
      income * (1 - elasticity_of_taxable_income * D_tax / (income - old_tax)) %>%
      coalesce(0)
    hutils::drop_col(sample_file, "new_taxable_income")
    sample_file[, "new_taxable_income" := new_taxable_income]
    
    # nocov start
    if (anyNA(new_taxable_income) || identical(as.double(new_taxable_income), as.double(income))) stop("NAs: ", sum(is.na(new_taxable_income)), call. = FALSE)
    # nocov end
    
    new_argument_vals <-
      argument_vals %>%
      .[names(argument_vals) %notin% c("arguments",
                                       "elasticity_of_taxable_income",
                                       "sample_file",
                                       "calc_baseline_tax",
                                       "return.")]
    
    new_sample_file <- copy(sample_file)
    new_sample_file[, "Taxable_Income" := as.double(new_taxable_income)]
    
    .model_income_tax <- function(...) {
      model_income_tax(sample_file = new_sample_file,
                       elasticity_of_taxable_income = NULL,
                       return. = "tax",
                       calc_baseline_tax = FALSE,
                       ...)
    }
    
    new_tax <- do.call(.model_income_tax, new_argument_vals)
  }
  
  switch(return.,
         "tax" = new_tax,
         "sample_file" = set(sample_file, j = "new_tax", value = new_tax),
         "sample_file.int" = set(sample_file, j = "new_tax", value = as.integer(new_tax)))
  
}



