#' New medicare levy
#' 
#' @description Use a different way to calculate medicare levy.
#' 
#' @param parameter_table A \code{data.table} containing 
#' \describe{
#' \item{\code{switches}}{The value in a row specifying which different medicare function is to apply.}
#' \item{\code{lower_threshold}}{What is the lower medicare threshold, below which no medicare levy is applied, above which a tapering rate applies.}
#' \item{\code{taper}}{What is the taper above \code{lower_threshold}.}
#' \item{\code{rate}}{The medicare levy applicable above the medicare thresholds.}
#' \item{\code{lower_up_for_each_child}}{How much the lower threshold should increase with each \code{n_dependants}.}
#' \item{\code{lower_family_threshold}}{The threshold as applied to families (i.e. couples)}
#' }
#' @return A function similar to \code{medicare_levy}.
#' @import data.table
#' @export
#' 

new_medicare_levy <- function(parameter_table){
  stopifnot(is.data.table(parameter_table))
  
  if (!all(c("switches", "lower_threshold", "taper", "rate", "lower_up_for_each_child", "lower_family_threshold") %in% names(parameter_table))){
    stop("parameter_table must contain certain columns. See", "\n\t", "?new_medicare_levy")
  }
  
  medicare_function <- function(income, 
                                Spouse_income = 0,
                                age = 42,
                                family_status = "individual", 
                                n_dependants = 0, 
                                switch){
    stopifnot(all(family_status %in% c("family", "individual")))
    prohibit_vector_recycling(income, family_status, Spouse_income, age, n_dependants)
    is_married <- Spouse_income > 0
    if ("individual" %chin% family_status[is_married]) {
      if (length(family_status) == 1L) {
        stop("`family_status = 'individual'` yet `Spouse_income > 0`. ", 
             "Whenever `Spouse_income` is positive, `family_status` must be set to 'family'.")
      } else {
        first_bad <- which(family_status == "individual" & is_married)
        stop("In entry ", first_bad, " `family_status = 'individual' ", 
             "yet Spouse_income > 0 for that entry. ",
             "Whenever `Spouse_income` is positive, `family_status` must be set to 'family'.")
      }
    }
    
    # CRAN NOTE avoidance
    income_share <- NULL
    
    data.table(income = income, 
               Spouse_income = Spouse_income,
               switches = switch, 
               family_status = family_status) %>%
      # Assume spouse income is included irrespective of Partner_status
      # This appears to be the correct treatment (e.g. if the Partner dies 
      # before the end of the tax year, they would have status 0 but 
      # income that is relevant for medicare income).  There are details
      # (such as if the partner is in gaol) that are overlooked here.
      # 
      # Enhancement: family taxable income should exclude super lump sums.
      .[, family_income := income + Spouse_income ] %>%
      merge(parameter_table, 
            by = c("switches"),
            sort = FALSE, 
            all.x = TRUE) %>%
      # Person who has spouse or dependants
      ## subs.8(5) of Act
      .[, lower_family_threshold := lower_family_threshold + n_dependants * lower_up_for_each_child] %>%
      .[, upper_family_threshold := upper_family_threshold + n_dependants * lower_up_for_each_child] %>%
      .[, income_share := if_else(Spouse_income > 0, income / (income + Spouse_income), 1)] %>%
      # Levy in the case of small incomes (s.7 of Act)
      .[, medicare_levy := if_else(family_status == "family" & family_income <= upper_family_threshold & income > lower_threshold,
                                   # subs.8(2)(c) of Medicare Levy Act 1986
                                   income_share * pminV(pmaxC(taper * (family_income - lower_family_threshold), 
                                                              0), 
                                                        rate * family_income),
                                   pminV(pmaxC(taper * (income - lower_threshold),
                                               0), 
                                         rate * income))] %>%
      
      .[["medicare_levy"]]
  }
  
  return(medicare_function)
}

