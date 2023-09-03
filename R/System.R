#' A list describing a tax system
#' 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yr \code{integer(1)} A year.
#' @param Offsets A list as created by \code{set_offsets()}.
#' 
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
#' @param Offsets List of offsets created by \code{set_offsets}.
#' @param sapto_max_offset The maximum offset available through SAPTO. 
#' @param sapto_lower_threshold The threshold at which SAPTO begins to reduce (from \code{sapto_max_offset}).
#' @param sapto_taper The taper rate beyond \code{sapto_lower_threshold}.
#' 
#' @param sapto_max_offset_married,sapto_lower_threshold_married,sapto_taper_married,sapto_lower_threshold_illness,sapto_max_offset_illness As above,
#' but applied to members of a couple.
#' 
#' @param sapto_pension_age The age at and above which the SAPTO is to apply.
#' 
#' @param fix \code{integer(1)} If \code{0L}, the default, an error will be emitted if
#' parameters are inconsistent; if \code{1L}, inconsistencies will be fixed.
#' 
#' @export

System <- function(yr, 
                   ordinary_tax_thresholds = NULL,
                   ordinary_tax_rates = NULL,
                   medicare_levy_taper = NULL, 
                   medicare_levy_rate = NULL,
                   medicare_levy_lower_threshold = NULL,
                   medicare_levy_lower_sapto_threshold = NULL,
                   medicare_levy_lower_family_threshold = NULL,
                   medicare_levy_lower_family_sapto_threshold = NULL,
                   medicare_levy_lower_up_for_each_child = NULL,
                   medicare_levy_upper_sapto_threshold = NULL,
                   medicare_levy_upper_family_threshold = NULL,
                   medicare_levy_upper_family_sapto_threshold = NULL,
                   medicare_levy_upper_threshold = NULL,
                   Offsets = NULL,
                   sapto_max_offset = NULL,
                   sapto_lower_threshold = NULL,
                   sapto_taper = NULL,
                   sapto_max_offset_married = NULL,
                   sapto_lower_threshold_married = NULL,
                   sapto_taper_married = NULL, 
                   sapto_max_offset_illness = NULL,
                   sapto_lower_threshold_illness = NULL,
                   sapto_pension_age = NULL,
                   fix = 0L) {
  yr
  if (is.character(yr) || is.fy(yr)) {
    yr <- fy::fy2yr(yr)
  }
  RSystem <- mget(ls(sorted = FALSE))
  .validateSystem(Filter(length, RSystem), fix = fix)
}



.validateSystem <- function(RSystem, fix = 0L) {
  if (anyDuplicated(tolower(names(RSystem)))) {
    stop("names(System) has duplicated names (case-insensitively)")
  }
  # if (length(RSystem$))
  .Call("CvalidateSystem", RSystem, as.integer(fix), PACKAGE = packageName())
}

brack_by_year <- function(yr, b = 1:8) {
  .Call("Cbracks_by_year", yr, b, PACKAGE = packageName())
}

rate_by_year <- function(yr, b = 1:8) {
  .Call("Crates_by_yr", yr, b, PACKAGE = packageName())
}







