#' Medicare levy
#' 
#' @description The (actual) amount payable for the Medicare levy.
#' 
#' @param income The taxable income. A vector of numeric values.
#' @param fy.year The financial year. A character vector satisfying \code{is.fy}.
#' @param Spouse_income The spouse's adjusted income.
#' @param sapto.eligible (logical) Is the taxpayer eligible for SAPTO? See Details.
#' @param sato Is the taxpayer eligible for the Senior Australians Tax Offset?
#' @param pto Is the taxpayer eligible for the Pensions Tax Offset?
#' @param family_status What is the taxpayer's family status: family or individual?
#' @param n_dependants Number of children dependant on the taxpayer.
#' @param .checks Should checks of certain arguments be made? Provided to improve performance when checks are not necessary.
#' @return The Medicare levy payable for that taxpayer.
#' @details The Seniors and Pensioners Tax Offset was formed in 2012-13 as an amalgam of the Senior Australians Tax Offset and the Pensions Tax Offset. 
#' Medicare rates before 2012-13 were different based on these offsets. 
#' For most taxpayers, eligibility would be based on whether your age is over the pension age (currently 65).
#' If \code{sato} and \code{pto} are \code{NULL}, \code{sapto.eligible} stands for eligibility for the \code{sato} and not \code{pto}.
#' If \code{sato} or \code{pto} are not \code{NULL} for such years, only \code{sato} is currently considered. 
#' Supplying \code{pto} independently is currently a warning.
#' @export
#' 

medicare_levy <- function(income, 
                          fy.year = "2013-14",
                          Spouse_income = 0,
                          sapto.eligible = FALSE,
                          sato = NULL,
                          pto = NULL,
                          family_status = "individual", 
                          n_dependants = 0, 
                          .checks = TRUE){
  if (.checks){
    stopifnot(all_fy(fy.year), all(family_status %in% c("family", "individual")))
    prohibit_vector_recycling(income, fy.year, family_status, Spouse_income, sapto.eligible, n_dependants)
  }
  if (any(Spouse_income > 0 & family_status == "individual")){
    stop("If Spouse_income is nonzero, family_status cannot be 'individual'.")
  }
  
  # If sapto.eligible = TRUE, sato = TRUE, pto = FALSE
  stopifnot(is.null(sato) || is.logical(sato), 
            is.null(pto)  || is.logical(pto))
  
  # Allow a join on a complete sato, pto, sapto key
  # To do this we need to make sato = sapto.eligible
  # and pto = !sato when required. 
  if (is.null(sato) && is.null(pto)){
    sato <- sapto.eligible
    pto <- sapto.eligible & !sato
  } else {
    if (is.null(sato)){
      sato <- !pto
    } else {
      if (is.null(pto)){
        pto <- !sato
      }
    }
    if (any(sato & pto)) {
      stop("pto and sato must not both be TRUE")
    }
    sapto.eligible <- sato | pto
    if (any(pto)){
      warning("pto assumed to be FALSE")
    }
  }
  
  
  income_share <- NULL
  
  # It is no faster in the case of single-length fy.year
  # or sapto.eligble etc and then selecting the table as required. 
  # if (AND(length(fy.year) == 1L,
  #         AND(length(sapto.eligible) == 1L,
  
  input_with_parameters <-
    data.table(income = income, 
               Spouse_income = Spouse_income,
               fy_year = fy.year,
               sapto = sapto.eligible, 
               sato = sato, 
               pto = pto,
               family_status = family_status) %>%
    # Assume spouse income is included irrespective of Partner_status
    # This appears to be the correct treatment (e.g. if the Partner dies 
    # before the end of the tax year, they would have status 0 but 
    # income that is relevant for medicare income).  There are details
    # (such as if the partner is in gaol) that are overlooked here.
    # 
    # Enhancement: family taxable income should exclude super lump sums.
    .[, family_income := income + Spouse_income ] %>%
    medicare_tbl[., on = c("fy_year", "sapto", "sato", "pto")]
    
  input_with_parameters %>%
    # Person who has spouse or dependants
    ## subs.8(5) of Act
    .[, lower_family_threshold := lower_family_threshold + n_dependants * lower_up_for_each_child] %>%
    .[, upper_family_threshold := upper_family_threshold + n_dependants * lower_up_for_each_child] %>%
    .[, income_share := if_else(Spouse_income > 0, income / (income + Spouse_income), 1)] %>%
    # Levy in the case of small incomes (s.7 of Act)
    .[, if_else(and(family_status == "family",
                    and(family_income <= upper_family_threshold,
                        income > lower_threshold)),
                # subs.8(2)(c) of Medicare Levy Act 1986
                income_share * pminV(pmax0(taper * (family_income - lower_family_threshold)),
                                     rate * family_income),
                pminV(pmax0(taper * (income - lower_threshold)), 
                      rate * income))]
}
