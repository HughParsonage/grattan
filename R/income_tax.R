#' Income tax payable
#' 
#' @name income_tax
#' @param income The individual assessable income.
#' @param fy.year The financial year in which the income was earned. Tax years 2000-01 to 2016-17 are provided, as well as the tax years 2017-18 to 2019-20, for convenience, under the assumption the 2017 Budget measures will pass. 
#' In particular, the tax payable is calculated under the assumption that the rate of the Medicare levy will rise to 2.5\% in the 2019-20 tax year.
#' @param age The individual's age.
#' @param family_status For Medicare and SAPTO purposes.
#' @param n_dependants An integer for the number of children of the taxpayer (for the purposes of the Medicare levy).
#' @param return.mode The mode (numeric or integer) of the returned vector.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files. If \code{.dots.ATO} is a \code{data.table}, I recommend you enclose it with \code{copy()}.
#' @param allow.forecasts should dates beyond 2019-20 be permitted? Currently, not permitted.
#' @author Tim Cameron, Brendan Coates, Hugh Parsonage, William Young
#' @details The function 'rolling' is inflexible by design. It is designed to guarantee the correct tax payable in a year.
#' For years preceding the introduction of SAPTO, the maximum offset is assumed to apply to those above pensionable age. 
#' @return the total personal income tax payable
#' @export income_tax

income_tax <- function(income, fy.year, age = 42, family_status = "individual", n_dependants = 0L, .dots.ATO = NULL, return.mode = c("numeric", "integer"), allow.forecasts = FALSE){
  if (missing(fy.year)){
    stop("fy.year is missing, with no default")
  }
  
  if (!all(fy.year %chin% c("2000-01", "2001-02", 
                            "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", 
                            "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", 
                            "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20"))) {
    bad <- which(!is.fy(fy.year))
    if (length(bad) > 5){
      stop("Entries ", bad[1:5], " (and others)", 
           " were not in correct form.", "\n", "First bad entry: ", fy.year[bad[1]])
    } else {
      stop("Entries ", bad, 
           " were not in correct form.", "\n", "First bad entry: ", fy.year[bad[1]])
    }
  }
  
  if (allow.forecasts || any(fy.year %notin% tax_tbl[["fy_year"]])){
    stop("rolling income tax not intended for future years or years before 2003-04. ",
         "Consider new_income_tax().")
  }
  
  if (any(income < 0)){
    warning("Negative entries in income detected. These will have value NA.")
  }
  
  if (!is.null(.dots.ATO)){
    if (!is.data.frame(.dots.ATO)){
      stop(".dots.ATO should be a data frame/data table")
    } else {
      if (nrow(.dots.ATO) != length(income)){
        stop("Number of rows of .dots.ATO does not match length of income.")
      }
    }
    
  }
  
  out <- rolling_income_tax(income = income, fy.year = fy.year, age = age, 
                            family_status = family_status, n_dependants = n_dependants, 
                            .dots.ATO = .dots.ATO)
  
  return.mode <- match.arg(return.mode)
  
  switch(return.mode, 
         "numeric" = {
           return(out)
         }, 
         "integer" = {
           return(as.integer(floor(out)))
         })
}


# rolling_income_tax is inflexible by design: returns the tax payable in the fy.year; no scope for policy change.
rolling_income_tax <- function(income, 
                               fy.year, 
                               age = NULL, # answer to life, more importantly < 65, and so key to SAPTO, medicare
                               family_status = "individual",
                               n_dependants = 0L,
                               .dots.ATO = NULL){
  # CRAN NOTE avoidance
  fy_year <- NULL; marginal_rate <- NULL; lower_bracket <- NULL; tax_at <- NULL; n <- NULL; tax <- NULL; ordering <- NULL; max_lito <- NULL; min_bracket <- NULL; lito_taper <- NULL; sato <- NULL; taper <- NULL; rate <- NULL; max_offset <- NULL; upper_threshold <- NULL; taper_rate <- NULL; medicare_income <- NULL; lower_up_for_each_child <- NULL;
  
  # Assume everyone of pension age is eligible for sapto.
  if (is.null(.dots.ATO) || "age_range" %notin% names(.dots.ATO)) {
    if (is.null(age)) {
      sapto.eligible <- FALSE
    } else {
      sapto.eligible <- age >= 65
    }
  } else {
    sapto.eligible <- .subset2(.dots.ATO, "age_range") <= 1L
  }
  
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  prohibit_vector_recycling(income, fy.year, age, family_status, n_dependants)
  prohibit_length0_vectors(income, fy.year, age, family_status, n_dependants)
  
  input.lengths <- vapply(list(income, fy.year, age, family_status, n_dependants), FUN = length, FUN.VALUE = integer(1))
  max.length <- max(input.lengths)
  
  input <- 
    data.table(income = income, 
               fy_year = fy.year) 
  
  input[, ordering := seq_len(.N)]
  setkeyv(input, c("fy_year", "income"))
  
  # input.keyed <-
  #   # potentially expensive. Another way would be 
  #   # to add an argument such as data.table.copy = FALSE
  #   # to allow different way to preserve the order
  #   copy(input) %>%
  #   setkey(fy_year, income)
  
  # tax_fun <- function(income, fy.year){
  #   tax_table2[input, roll = Inf] %>%
  #     .[, tax := tax_at + (income - lower_bracket) * marginal_rate] %>%
  #     setorderv("ordering") %>%
  #     .[["tax"]]
  # }
  
  base_tax. <- 
    tax_table2[input, roll = Inf] %>%
    .[, tax := tax_at + (income - lower_bracket) * marginal_rate] %>%
    setorderv("ordering") %>%
    .[["tax"]]
  
  setkeyv(input, "fy_year")  # for .lito below
  
  # If .dots.ATO  is NULL, for loops over zero-length vector
  for (j in which(vapply(.dots.ATO, FUN = is.double, logical(1)))){
    set(.dots.ATO, j = j, value = as.integer(.dots.ATO[[j]]))
  }
  
  if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
    the_spouse_income <- 0L
  } else {
    the_spouse_income <- .dots.ATO[["Spouse_adjusted_taxable_inc"]]
    the_spouse_income[is.na(the_spouse_income)] <- 0L
  }
  
  medicare_levy. <- 
    medicare_levy(income, 
                  Spouse_income = the_spouse_income,
                  fy.year = fy.year, 
                  sapto.eligible = sapto.eligible, 
                  family_status = if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
                    family_status
                  } else {
                    FS <- rep_len("individual", max.length)
                    FS[the_spouse_income > 0] <- "family"
                    FS
                  }, 
                  n_dependants = n_dependants, 
                  .checks = FALSE)
  
  lito. <- .lito(input)
  
  if (any(sapto.eligible)) {
    if (!is.null(.dots.ATO) && all(c("Rptbl_Empr_spr_cont_amt",
                                     "Net_fincl_invstmt_lss_amt",
                                     "Net_rent_amt", 
                                     "Rep_frng_ben_amt") %chin% names(.dots.ATO))) {
      sapto. <- sapto.eligible * sapto(rebate_income = rebate_income(Taxable_Income = income,
                                                                     Rptbl_Empr_spr_cont_amt = .dots.ATO[["Rptbl_Empr_spr_cont_amt"]],
                                                                     Net_fincl_invstmt_lss_amt = .dots.ATO[["Net_fincl_invstmt_lss_amt"]],
                                                                     Net_rent_amt = .dots.ATO[["Net_rent_amt"]],
                                                                     Rep_frng_ben_amt = .dots.ATO[["Rep_frng_ben_amt"]]), 
                                       fy.year = fy.year,
                                       Spouse_income = the_spouse_income,
                                       family_status = {
                                         FS_sapto <- rep_len("single", max.length)
                                         FS_sapto[the_spouse_income > 0] <- "married"
                                         FS_sapto
                                       },
                                       sapto.eligible = TRUE)
    } else {
      sapto. <- sapto.eligible * sapto(rebate_income = rebate_income(Taxable_Income = income), 
                                       fy.year = fy.year, 
                                       sapto.eligible = TRUE)
    }
  } else {
    sapto. <- 0
  }
  
  # https://www.legislation.gov.au/Details/C2014A00048
  # input[["fy_year"]] ensures it matches the length of income if length(fy.year) == 1.
  temp_budget_repair_levy. <- (input[["fy_year"]] %chin% c("2014-15", "2015-16", "2016-17") & income > 180e3) * (0.02 * (income - 180e3))
  
  pmaxC(base_tax. - lito. - sapto., 
        0) + medicare_levy. + temp_budget_repair_levy.
}

