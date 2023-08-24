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
#' @param lito_max_offset (deprecated) The maximum offset available for low incomes.
#' @param lito_taper (deprecated) The taper to apply beyond \code{lito_min_bracket}.
#' @param lito_min_bracket (deprecated) The taxable income at which the value of the offset starts to reduce (from \code{lito_max_offset}).
#' @param lito_multi No longer supported.
#' 
#' @param offsets A list of lists created by \code{\link{set_offsets}}. If
#' \code{NULL}, the default, the list is populated by the offsets
#' in \code{baseline_fy}.
#' 
#' @param Budget2018_lamington No longer supported
#' @param Budget2019_lamington No longer supported.
#' 
#' @param Budget2018_lito_202223 No longer supported.
#' @param Budget2018_watr No longer supported
#' @param Budget2019_watr No longer supported.
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
#' # if (requireNamespace("taxstats", quietly = TRUE) && FALSE) {
#' # library(taxstats)
#' # library(magrittr)
#' #    
#' # model_income_tax(sample_file_1314,
#' #                  "2013-14",
#' #                  ordinary_tax_thresholds = c(0, 20e3, 37e3, 80e3, 180e3)) %>%
#'  #   select_grep("tax", "Taxable_Income")
#' #
#' # }
#' 
#' @export



model_income_tax <- function(sample_file,
                             baseline_fy,
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
                             offsets = NULL,
                             Budget2018_lamington = FALSE,
                             Budget2019_lamington = NA,
                             Budget2018_lito_202223 = FALSE,
                             Budget2018_watr = FALSE,
                             Budget2019_watr = FALSE,
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
  yr <- fy::fy2yr(baseline_fy)
  if (!missing(Budget2018_lamington) || 
      !missing(Budget2019_lamington) || 
      !missing(Budget2018_lito_202223) ||
      !missing(Budget2018_watr) ||
      !missing(Budget2019_watr) ) {
    if (!is_testing()) {
      warning("Budget2018_lamington, Budget2019_lamington, Budget2018_lito_202223,",
              " Budget2018_watr, Budget2019_watr no longer supported and will",
              " be ignored.")
    }
    if (isTRUE(Budget2018_lamington) && is.null(offsets)) {
      offsets <- set_offsets(set_offset(offset_1st = 200L,
                                        thresholds = c(37000L, 
                                                       48000L,
                                                       90000L),
                                        tapers = c(-0.03, 0, 0.015)),
                             yr = yr)
    }
    if (isTRUE(Budget2018_watr) && is.null(offsets)) {
      offsets <- set_offsets(set_offset(offset_1st = 350L,
                                        thresholds = c(37000L, 
                                                       48000L,
                                                       90000L),
                                        tapers = c(-0.0525, 0, 0.02625)),
                             yr = yr)
    }
    if (isTRUE(Budget2019_watr) && is.null(offsets)) {
      offsets <- set_offsets(set_offset(offset_1st = 350L,
                                        thresholds = c(37000L, 
                                                       48000L,
                                                       90000L),
                                        tapers = c((1080-350)/11000, 0, -0.03)),
                             yr = yr)
    }
    if (isTRUE(Budget2018_lito_202223) && is.null(offsets)) {
      offsets <- set_offsets(set_offset(offset_1st = 645L, 
                                        tapers = c(0.065, 0.015), 
                                        thresholds = c(37000L, 41000L)),
                             yr = NULL)
    }
  }
  
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
  max.length <- nrow(sample_file)
  .dots.ATO <-  sample_file
  sample_file_noms <- copy(names(sample_file))
  
  .Offsets <- set_offsets(yr = yr, 
                          lito_max_offset = lito_max_offset, 
                          lito_taper = lito_taper,
                          lito_min_bracket = lito_min_bracket)
  if (!is.null(offsets)) {
    .Offsets <- offsets
  }
  
  
  .System <-
    System(ordinary_tax_thresholds = ordinary_tax_thresholds,
           ordinary_tax_rates = ordinary_tax_rates,
           yr = yr,
           medicare_levy_taper = medicare_levy_taper,
           medicare_levy_rate = medicare_levy_rate,
           medicare_levy_lower_threshold = medicare_levy_lower_threshold,
           medicare_levy_upper_threshold = medicare_levy_upper_threshold,
           medicare_levy_lower_sapto_threshold = medicare_levy_lower_sapto_threshold,
           medicare_levy_upper_sapto_threshold = medicare_levy_upper_sapto_threshold,
           medicare_levy_lower_family_threshold = medicare_levy_lower_family_threshold,
           medicare_levy_upper_family_threshold = medicare_levy_upper_family_threshold,
           medicare_levy_lower_family_sapto_threshold = medicare_levy_lower_family_sapto_threshold,
           medicare_levy_upper_family_sapto_threshold = medicare_levy_upper_family_sapto_threshold,
           medicare_levy_lower_up_for_each_child = medicare_levy_lower_up_for_each_child,
           Offsets = .Offsets,
           sapto_max_offset = sapto_max_offset,
           sapto_lower_threshold = sapto_lower_threshold,
           sapto_taper = sapto_taper,
           sapto_max_offset_married = sapto_max_offset_married,
           sapto_lower_threshold_married = sapto_lower_threshold_married,
           sapto_taper_married = sapto_taper_married, 
           fix = 1L)
  
  if (hasName(.dots.ATO, "Taxable_Income") && !hasName(.dots.ATO, "ic_taxable_income_loss")) {
    # for elasticity
    .dots.ATO[, "ic_taxable_income_loss" := Taxable_Income]
  }
  
  old_tax <- income_tax2(.dots.ATO = .dots.ATO,
                         fy.year = baseline_fy)
  
  set_cgt_rate(.dots.ATO, yr, cgt_discount_rate)
  if (!is.null(sapto_eligible)) {
    .dots.ATO[, "c_age_30_june" := fifelse(sapto_eligible, 67L, 42L)]
  }
  .apply_elasticity(.dots.ATO, old_tax, baseline_fy, .System, elasticity_of_taxable_income)
  
  new_tax <- income_tax2(.dots.ATO = .dots.ATO,
                         fy.year = baseline_fy,
                         System = .System)
  
  switch(return.,
         "tax" = NULL,
         "sample_file" = set(sample_file, j = "baseline_tax", value = old_tax),
         "sample_file.int" = set(sample_file, j = "baseline_tax", value = as.integer(old_tax)))
                         
  
  
  switch(return.,
         "tax" = new_tax,
         "sample_file" = set(sample_file, j = "new_tax", value = new_tax),
         "sample_file.int" = set(sample_file, j = "new_tax", value = as.integer(new_tax)))
  
}

.elast_income <- function(income, old_tax, new_tax, elasticity_of_taxable_income) {
  D_tax <- new_tax - old_tax
  # Change in net income
  coalesce0(income * (1 - elasticity_of_taxable_income * D_tax / (income - old_tax)))
  
}

.apply_elasticity <- function(.dots.ATO, old_tax, fy_year, System, elasticity_of_taxable_income) {
  if (!is.numeric(elasticity_of_taxable_income)) {
    return(invisible(.dots.ATO))
  }
  
  stopifnot(hasName(.dots.ATO, "ic_taxable_income_loss"))
  ic_taxable_income_loss <- .subset2(.dots.ATO, "ic_taxable_income_loss")
  .dots.ATO[, "new_taxable_income" := copy(ic_taxable_income_loss)]
  new_tax <- income_tax2(.dots.ATO = .dots.ATO, System = System, fy.year = fy_year)
  D_tax <- new_tax - old_tax
  i0 <- .subset2(.dots.ATO, "ic_taxable_income_loss")
  i1 <- .elast_income(i0, old_tax, new_tax, elasticity_of_taxable_income)
  
  # Change in net income, coalesce0 due to likely NaNs
  set(.dots.ATO, j = "ic_taxable_income_loss", value = i1)
}

set_cgt_rate <- function(.dots.ATO, yr, new_cgt_discount) {
  stopifnot(is.data.table(.dots.ATO))
  if (is.null(new_cgt_discount)) {
    return(invisible(.dots.ATO))
  }
  if (!is.numeric(new_cgt_discount)) {
    stop("`cgt_discount_rate` was type ", typeof(new_cgt_discount), ", ",
         "but must be numeric. Ensure `cgt_discount_rate`, if used, is numeric.")
  }
  if (length(new_cgt_discount) != 1 && length(new_cgt_discount) != nrow(.dots.ATO)) {
    stop("`length(cgt_discount_rate) = ", length(new_cgt_discount), "`, ", 
         "yet `nrow(sample_file) = ", nrow(.dots.ATO), "`. ",
         "Ensure `cgt_discount_rate` has length one.")
  }
  ic_taxable_income_loss <- 
    .subset2(.dots.ATO, "ic_taxable_income_loss") %||%
    .subset2(.dots.ATO, "Taxable_Income")
  is_cg_net <- 
    .subset2(.dots.ATO, "is_cg_net") %||%
    .subset2(.dots.ATO, "Net_CG_amt")
  if (is.null(is_cg_net)) {
    return(invisible(.dots.ATO))
  }
  # isn_tot_current <-
  #   .subset2(.dots.ATO, "isn_cg_tot_current") %||%
  #   .subset2(.dots.ATO, "Tot_CY_CG_amt") %||%
  #   is_cg_net
  
  # assume that every one with cg_event
  tot_cg <- is_cg_net / (1 - CGT_DISCOUNT(yr))
  new_cg_net <- ((1 - new_cgt_discount) * tot_cg)
  new_taxable_income <- ic_taxable_income_loss +  new_cg_net - is_cg_net
  set(.dots.ATO, j = "ic_taxable_income_loss", value = as.integer(new_taxable_income))
}



