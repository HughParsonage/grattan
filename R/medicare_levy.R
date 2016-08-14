#' Medicare levy
#' 
#' @description The (actual) amount payable for the medicare levy.
#' 
#' @param income The taxable income. A vector of numeric values.
#' @param fy.year The financial year. A character vector satisfying \code{is.fy}.
#' @param Spouse_income The spouse's adjusted income.
#' @param sapto.eligible (logical) Is the taxpayer eligible for SAPTO?
#' @param family_status What is the taxpayer's family status: family or individual?
#' @param n_dependants Number of children dependant on the taxpayer.
#' @return The medicare levy payable for that taxpayer.
#' @details The actual medicare levy payable for such a taxpayer in that financial year.
#' @export
#' 

medicare_levy <- function(income, 
                          fy.year = "2013-14",
                          Spouse_income = 0,
                          sapto.eligible = FALSE,
                          family_status = "individual", 
                          n_dependants = 0){
  stopifnot(all(is.fy(fy.year)), all(family_status %in% c("family", "individual")))
  prohibit_vector_recycling(income, fy.year, family_status, Spouse_income, sapto.eligible, n_dependants)
  if (any(Spouse_income > 0 & family_status == "individual")){
    stop("If Spouse_income is nonzero, family_status cannot be 'individual'.")
  }
  
  data.table(income = income, 
             Spouse_income = Spouse_income,
             fy_year = fy.year,
             sapto = sapto.eligible, 
             family_status = family_status) %>%
    # Assume spouse income is included irrespective of Partner_status
    # This appears to be the correct treatment (e.g. if the Partner dies 
    # before the end of the tax year, they would have status 0 but 
    # income that is relevant for medicare income).  There are details
    # (such as if the partner is in gaol) that are overlooked here.
    # 
    # Enhancement: family taxable income should exclude super lump sums.
    .[ ,family_income := income + Spouse_income ] %>%
    merge(medicare_tbl, 
          by = c("fy_year", "sapto"),
          sort = FALSE, 
          all.x = TRUE) %>%
    # Levy in the case of small incomes (s.7 of Act)
    .[ ,medicare_levy := pminV(pmaxC(taper * (income - lower_threshold),
                                     0), 
                               rate * income)] %>%
    # Person who has spouse or dependants
    ## subs.8(5) of Act
    .[ ,lower_family_threshold := lower_family_threshold + n_dependants * lower_up_for_each_child] %>%
    .[ ,medicare_levy := pmaxC(medicare_levy - 
                                 (family_status == "family") *
                                 # pmaxC <= "(if any)" subs.8(2)(c) of Medicare Levy Act 1986
                                 pmaxC(0.02 * lower_family_threshold - 0.08 * (family_income - lower_family_threshold), 0),
                               0)] %>%
    .[["medicare_levy"]]
}
