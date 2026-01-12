#' Medicare levy
#' 
#' @description The (actual) amount payable for the Medicare levy.
#' 
#' @param income 
#' \describe{
#' \item{\code{numeric(N)}}{The income for medicare levy purposes of the taxpayer.}
#' }
#' @param fy.year 
#' \describe{
#' \item{\code{character(1)} or \code{character(N)} or \code{fy(N)} or \code{fy(1)}}{
#' The tax year in which \code{income} was earned. A vector satisfying \code{fy::validate_fys_permitted}.}
#' }
#' 
#' @param Spouse_income 
#' \describe{
#' \item{\code{numeric(1)} or \code{numeric(N)}}{The income of the taxpayer's spouse. Missing values are
#' imputed to zeroes. Values are truncated to integer.}
#' }
#' @param sapto.eligible 
#' \describe{
#' \item{\code{logical(1)} or \code{logical(N)}}{Is the taxpayer entitled to the SAPTO thresholds? Missing
#' values are imputed to \code{FALSE}.}
#' }
#' 
#' @param sato,pto Is the taxpayer eligible for the Senior Australians Tax Offset or Pensions Tax Offset?
#' \code{pto = TRUE} not supported and will be set to \code{FALSE}, with a warning.
#' @param family_status (Deprecated: use `is_married` and `n_dependants` instead)
#' @param n_dependants \describe{
#' \item{\code{integer(N)} or \code{integer(1)}}{Number of dependants the taxpayer has. If nonzero, 
#' the taxpayer is entitled to the family thresholds of the Medicare levy, and
#' each dependant child increases the thresholds.}
#' }
#' @param is_married \describe{
#' \item{\code{logical(N)}}{Is the taxpayer married? Married individuals (or those 
#' whose \code{Spouse_income > 0}) are deemed to be families when determining 
#' cut-off thresholds.}
#' }
#' 
#' @param .checks Whether or not to perform checks on inputs.
#' 
#' @return The Medicare levy payable for that taxpayer.
#' @details The Medicare levy for individuals is imposed by the \emph{Medicare Levy Act 1986} (Cth).
#' The function only calculates the levy for individuals (not trusts).
#' It includes the s 7 \emph{Levy in cases of small incomes}, including the differences for those
#' eligible for \code{\link{sapto}}.
#' s 8 \emph{Amount of levy---person who has spouse or dependants} (though the number of dependants
#' is not a variable in the sample files).
#' 
#' The function does \strong{not} include the Medicare levy surcharge; it assumes that all 
#' persons (who would potentially be liable for it) avoided it.
#' 
#' The Seniors and Pensioners Tax Offset was formed in 2012-13 as an amalgam
#'  of the Senior Australians Tax Offset and the Pensions Tax Offset. 
#' Medicare rates before 2012-13 were different based on these offsets. 
#' For most taxpayers, eligibility would be based on whether your age is over the pension age (currently 65).
#' If \code{sato} and \code{pto} are \code{NULL}, \code{sapto.eligible} stands for eligibility for the \code{sato} and not \code{pto}.
#' If \code{sato} or \code{pto} are not \code{NULL} for such years, only \code{sato} is currently considered. 
#' Supplying \code{pto} independently is currently a warning.
#' 
#' See \code{https://classic.austlii.edu.au/au/legis/cth/consol_act/mla1986131/}
#' for the \emph{Medicare Levy Act 1986} (Cth).
#' 
#' @export
#' 

medicare_levy <- function(income, 
                          fy.year = "2013-14",
                          Spouse_income = 0L,
                          sapto.eligible = FALSE,
                          sato = NULL,
                          pto = NULL,
                          family_status = "individual", 
                          n_dependants = 0L,
                          is_married = NULL,
                          .checks = FALSE) {
  fy.year <- validate_fys_permitted(fy.year)
  N <- length(income)
  
  # A person is entitled to the family thresholds if they are married
  # their spouse has nonzero income or they have children.
  if (missing(family_status)) {
    # Assume to be (all) individuals (unmarried, no children)
    is_married <- is_married %||% logical(N)
    n_dependants <- n_dependants %||% integer(N)
  } else {
    warning("argument `family_status` is deprecated. Use `is_married` and `n_dependants` instead.")
    is_married <- is_married %||% rep_len(family_status != "individual", N)
    n_dependants <- n_dependants %||% integer(N)
  }
  
  if (!is.null(sato) || !is.null(pto)) {
    # sato and pto are not really supported but we assume that sato and
    # sapto are the same thing, and warn if pto is provided.
    if (is.null(sato) && !is.null(pto)) {
      sato <- !pto
    }
    if (is.null(pto) && !is.null(sato)) {
      pto <- !sato
    }
    if (any(sato & pto)) {
      stop("`pto` and `sato` must not both be TRUE. First bad position: ",
           which.max(sato & pto), ".")
    }
    sapto.eligible <- sato | pto
    if (any(pto)){
      warning("`pto` includes TRUE values but will be assumed to be FALSE")
    }
  }
  
  Spouse_income <- do_rN(Spouse_income, integer(N))
  n_dependants <- do_rN(n_dependants, integer(N))
  
  do_medicare_levy(income = income, 
                   yr = fy2yr(fy.year),
                   spouse_income = Spouse_income, 
                   sapto_eligible = sapto.eligible,
                   is_married = is_married,
                   n_dependants = n_dependants)
}


do_medicare_levy <- function(income, yr, spouse_income, sapto_eligible, is_married, n_dependants,
                             sapto_const = FALSE,
                             lwr_single = 21980,
                             lwr_family = 37089,
                             lwr_single_sapto = 34758,
                             lwr_family_sapto = 48385,
                             lwr_up_per_child = 2253,
                             taper = 0.1,
                             rate = 0.02) {
  zero <- integer(length(income))
  .Call("Cdo_medicare_levy", 
        do_rN(income, zero),
        yr, 
        do_rN(spouse_income, zero),
        do_rN(sapto_eligible, zero),
        do_rN(is_married, zero),
        do_rN(n_dependants, zero),
        PACKAGE = packageName())
}

ML_LWR_THRESH <- function(yr, family = FALSE, sapto = FALSE) {
  .Call("C_ml_lower_thresh", as.integer(yr), family, sapto, PACKAGE = packageName())
}

ML_UPR_THRESH <- function(yr, family = FALSE, sapto = FALSE) {
  .Call("C_ml_upper_thresh", as.integer(yr), family, sapto, PACKAGE = packageName())
}

ML_CHILD_UPPER <- function(yr) {
  .Call("Cml_child", as.integer(yr), PACKAGE = packageName())
}

ML_TAPER <- function(yr) {
  .Call("C_ml_taper", as.integer(yr), PACKAGE = packageName())
}

ML_RATE <- function(yr) {
  .Call("C_ml_rate", as.integer(yr), PACKAGE = packageName())
}


