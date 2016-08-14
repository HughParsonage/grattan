#' tax function
#' 
#' @name income_tax
#' @param income the personal assessable income
#' @param fy.year the financial year in which the income was earned
#' @param age the individual's age
#' @param family_status For medicare and sapto purposes. Still in development.
#' @param n_dependants An integer for the number of children of the taxpayer (for the purposes of the Medicare levy).
#' @param return.mode use numeric (integer not yet implemented).
#' @param sample_file (Not yet used) A sample file \code{data.table} for which the income tax payable is desired on each row.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files.
#' @param allow.forecasts should dates beyond 2014-15 be permitted?
#' @author Tim Cameron, Brendan Coates, Hugh Parsonage, William Young
#' @details The function 'rolling' is inflexible by design. It is designed to guarantee the correct tax payable in a year.
#' @useDynLib grattan
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @importFrom dplyr if_else
#' @import data.table
#' @return the total personal income tax payable
#' @export 

income_tax <- function(income, fy.year, age = 42, family_status = "individual", n_dependants = 0L, sample_file, .dots.ATO = NULL, return.mode = "numeric", allow.forecasts = FALSE){
  if (missing(fy.year)){
    stop("fy.year is missing, with no default")
  }
  
  if (any(!is.fy(fy.year))){
    bad <- which(!is.fy(fy.year))
    if (length(bad) > 5){
      stop("Entries ", bad[1:5], " (and others)", 
           " were not in correct form.", "\n", "First bad entry: ", fy.year[bad[1]])
    } else {
      stop("Entries ", bad, 
           " were not in correct form.", "\n", "First bad entry: ", fy.year[bad[1]])
    }
  }
  
  if (allow.forecasts || any(fy.year %notin% tax_tbl$fy_year)){
    stop("rolling income tax not intended for future years or years before 2003-04. ",
         "Consider old_income_tax() or new_income_tax().")
  }
  
  if (any(income < 0)){
    warning("Negative entries in income detected. These will have value NA.")
  }
  
  if (!missing(.dots.ATO)){
    if (!is.data.frame(.dots.ATO)){
      stop(".dots.ATO should be a data frame/data table")
    } else {
      if (nrow(.dots.ATO) != length(income)){
        stop("Number of rows of .dots.ATO does not match length of income.")
      }
    }
    
  }
  
  rolling_income_tax(income = income, fy.year = fy.year, age = age, 
                     family_status = family_status, n_dependants = n_dependants, 
                     sample_file = sample_file, .dots.ATO = .dots.ATO, return.mode = return.mode)
}


# rolling_income_tax is inflexible by design: returns the tax payable in the fy.year; no scope for policy change.
rolling_income_tax <- function(income, 
                               fy.year, 
                               age = 42, # answer to life, more importantly < 65, and so key to SAPTO, medicare
                               family_status = "individual",
                               n_dependants = 0L,
                               sample_file,
                               .dots.ATO = NULL, 
                               return.mode = "numeric"){
  # CRAN NOTE avoidance
  fy_year <- NULL; marginal_rate <- NULL; lower_bracket <- NULL; tax_at <- NULL; n <- NULL; tax <- NULL; ordering <- NULL; max_lito <- NULL; min_bracket <- NULL; lito_taper <- NULL; sato <- NULL; taper <- NULL; rate <- NULL; max_offset <- NULL; upper_threshold <- NULL; taper_rate <- NULL; medicare_income <- NULL; lower_up_for_each_child <- NULL;
  
  
  
  if (return.mode != "numeric"){
    stop("return.mode must be set to numeric only. Future versions may allow differently.")
  }
  # Assume everyone of pension age is eligible for sapto.
  sapto.eligible = age >= 65
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  prohibit_vector_recycling(income, fy.year, age, family_status, n_dependants)
  prohibit_length0_vectors(income, fy.year, age, family_status, n_dependants)
  
  input.lengths <- vapply(list(income, fy.year, age, family_status, n_dependants), FUN = length, FUN.VALUE = integer(1))
  max.length <- max(input.lengths)

  # tax_table2 provides the raw tax tables, but also the amount
  # of tax paid at each bracket, to make the rolling join 
  # calculation later a one liner.
  
  tax_table2 <- 
    tax_tbl %>%
    dplyr::group_by(fy_year) %>%
    dplyr::mutate(
      tax_at = cumsum(data.table::shift(marginal_rate, type = "lag", fill = 0) * (lower_bracket - data.table::shift(lower_bracket, type = "lag", fill = 0))),
      income = lower_bracket) %>%
    dplyr::select(fy_year, income, lower_bracket, marginal_rate, tax_at) %>%
    data.table::as.data.table(.) %>%
    data.table::setkey(fy_year, income) 
  
  input <- 
    data.table::data.table(income = income, 
                           fy_year = fy.year) 
  
  input <- input[ ,ordering := 1:.N]
  
  input.keyed <-
    # potentially expensive. Another way would be 
    # to add an argument such as data.table.copy = FALSE
    # to allow different way to preserve the order
    data.table::copy(input) %>%
    data.table::setkey(fy_year, income)
  
  tax_fun <- function(income, fy.year){
    tax_table2[input.keyed, roll = Inf][,tax := tax_at + (income - lower_bracket) * marginal_rate][order(ordering)] %$%
      tax
  }
  
  .lito <- function(income, fy.year){
    merge(lito_tbl, 
          input, 
          by = "fy_year", 
          # sort set to FALSE to avoid the key ruining the order
          sort = FALSE, 
          # right join because there may be no LITO
          all.y = TRUE) %$%
    {
      pminV(pmaxC(max_lito - (income - min_bracket) * lito_taper, 0),
            max_lito)
    }
  }
  
  base_tax. <- tax_fun(income, fy.year = fy.year)
  medicare_levy. <- 
    medicare_levy(income, 
                  Spouse_income = if (missing(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
                    0
                  } else {
                    .dots.ATO$Spouse_adjusted_taxable_inc
                  },
                  fy.year = fy.year, 
                  sapto.eligible = sapto.eligible, 
                  family_status = if (missing(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
                    family_status
                  } else {
                    if_else(.dots.ATO$Spouse_adjusted_taxable_inc > 0, "family", "individual")
                  }, 
                  n_dependants = n_dependants)
  
  lito. <- .lito(income, fy.year)
  
  if (!is.null(.dots.ATO) && !missing(.dots.ATO) && all(c("Rptbl_Empr_spr_cont_amt",
                                                          "Net_fincl_invstmt_lss_amt",
                                                          "Net_rent_amt", 
                                                          "Rep_frng_ben_amt") %in% names(.dots.ATO))){
    sapto. <- sapto.eligible * sapto(rebate_income = rebate_income(Taxable_Income = income,
                                                                   Rptbl_Empr_spr_cont_amt = .dots.ATO$Rptbl_Empr_spr_cont_amt,
                                                                   Net_fincl_invstmt_lss_amt = .dots.ATO$Net_fincl_invstmt_lss_amt,
                                                                   Net_rent_amt = .dots.ATO$Net_rent_amt,
                                                                   Rep_frng_ben_amt = .dots.ATO$Rep_frng_ben_amt), 
                                     fy.year = fy.year, 
                                     sapto.eligible = TRUE)
  } else {
    sapto. <- sapto.eligible * sapto(rebate_income = rebate_income(Taxable_Income = income), 
                                     fy.year = fy.year, 
                                     sapto.eligible = TRUE)
  }
  
  # https://www.legislation.gov.au/Details/C2014A00048
  # input[["fy_year"]] ensures it matches the length of income if length(fy.year) == 1.
  # Also using dplyr::if_else for safety.
  temp_budget_repair_levy. <- if_else(input[["fy_year"]] %in% c("2014-15", "2015-16", "2016-17"), 
                                      pmaxC(0.02 * (income - 180e3), 0), 
                                      0)
  
  
  
  pmaxC(base_tax. - lito. - sapto., 
        0) + medicare_levy. + temp_budget_repair_levy.
}

