#' A list describing a tax system
#' 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param yr \code{integer(1)} A year.
#' @param Sapto A list created by \code{Sapto()}.
#' @param offsets A list as created by \code{Offsets()}.
#' @param Medicare A list created by \code{Medicare()}.
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
#' @param offsets PARAM_DESCRIPTION, Default: NULL
#' @param sapto_max_offset The maximum offset available through SAPTO. 
#' @param sapto_lower_threshold The threshold at which SAPTO begins to reduce (from \code{sapto_max_offset}).
#' @param sapto_taper The taper rate beyond \code{sapto_lower_threshold}.
#' 
#' @param sapto_max_offset_married,sapto_lower_threshold_married,sapto_taper_married As above,
#' but applied to members of a couple
#' @param ... Other parameters that may be supported in future versions. 
#' 
#' @export

System <- function(yr, 
                   ordinary_tax_thresholds = NULL,
                   ordinary_tax_rates = NULL,
                   Medicare = NULL,
                   offsets = NULL,
                   Sapto = NULL, 
                   ...) {
  RSystem <- mget(ls(sorted = FALSE))
  if (is.null(Medicare) && !missing(..1)) {
    Medicare <- Medicare(...)
  }
  if (is.null(Sapto) && !missing(..1)) {
    Sapto <- Sapto(...)
  }
  for (m in names(Medicare)) {
    RSystem[[m]] <- Medicare[[m]]
  }
  for (s in names(Sapto)) {
    RSystem[[s]] <- Medicare[[s]]
  }
  Filter(length, RSystem)
}

#' @rdname System
#' @export
Medicare <- function(yr,
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
                     ...) {
  RSystem <- mget(ls(sorted = FALSE))
  Filter(length, RSystem)
}

#' @rdname System
#' @export
Sapto <- function(yr,
                  sapto_max_offset = NULL,
                  sapto_lower_threshold = NULL,
                  sapto_taper = NULL,
                  sapto_max_offset_married = NULL,
                  sapto_lower_threshold_married = NULL,
                  sapto_taper_married = NULL,
                  ...) {
  RSystem <- mget(ls(sorted = FALSE))
  Filter(length, RSystem)
}

Offsets <- function(offset_1st = integer(1),
                    thresholds = integer(), 
                    tapers = double(), 
                    refundable = logical(1)) {
  checkmate::assert_integerish(offset_1st)
  stopifnot(length(thresholds) == length(tapers),
            is.numeric(thresholds), 
            is.numeric(tapers))
  check_TF(refundable)
  if (is.unsorted(thresholds)) {
    stop("`thresholds = ", thresholds, "` was unsorted.")
  }
  list(offset_1st = offset_1st,
       thresholds = thresholds,
       tapers = tapers, 
       refundable = refundable)
}
