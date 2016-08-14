#' New medicare levy
#' 
#' @description Use a different way to calculate medicare levy.
#' 
#' @param parameter_table A \code{data.table} containing 
#' \describe{
#' \item{\code{elderly}}{A logical column whether to designate a different medicare.}
#' \item{\code{family_status}}{'family' for a different threshold for dependants}
#' \item{\code{lower_threshold}}{What is the lower medicare threshold, below which no medicare levy is applied, above which a tapering rate applies.}
#' \item{\code{taper}}{What is the taper above \code{lower_threshold}.}
#' \item{\code{rate}}{The medicare levy applicable above the medicare thresholds.}
#' \item{\code{lower_up_for_each_child}}{How much the lower threshold should increase with each \code{n_dependants}.}
#' }
#' @param elderly_age_is What age does the elderly row apply?
#' @return A function similar to \code{medicare_levy}.
#' @import data.table
#' @export
#' 

new_medicare_levy <- function(parameter_table, elderly_age_is = 65){
  stopifnot(is.data.table(parameter_table))
  
  medicare_function <- function(income, 
                                Spouse_income = 0,
                                age = 42,
                                family_status = "individual", 
                                n_dependants = 0){
    stopifnot(all(family_status %in% c("family", "individual")))
    prohibit_vector_recycling(income, family_status, Spouse_income, age, n_dependants)
    if (any(Spouse_income > 0 & family_status == "individual")){
      stop("If Spouse_income is nonzero, family_status cannot be 'individual'.")
    }
    
    data.table(income = income, 
               Spouse_income = Spouse_income,
               
               elderly = age >= elderly_age_is, 
               family_status = family_status) %>%
      # Assume spouse income is included irrespective of Partner_status
      # This appears to be the correct treatment (e.g. if the Partner dies 
      # before the end of the tax year, they would have status 0 but 
      # income that is relevant for medicare income).  There are details
      # (such as if the partner is in gaol) that are overlooked here.
      # 
      # Enhancement: family taxable income should exclude super lump sums.
      .[ ,family_income := income + Spouse_income ] %>%
      merge(parameter_table, 
            by = c("elderly"),
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
  
  return(medicare_function)
}