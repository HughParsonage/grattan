#' tax function
#' 
#' @name income_tax
#' @param income the personal assessable income
#' @param fy.year the financial year in which the income was earned
#' @param age the individual's age
#' @param family_status For medicare and sapto purposes. Still in development.
#' @param return.mode use numeric or integer
#' @param sample_file (Not yet used) A sample file \code{data.table} for which the income tax payable is desired on each row.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files.
#' @param allow.forecasts should dates beyond 2014-15 be permitted?
#' @author Tim Cameron, Brendan Coates, Hugh Parsonage
#' @details The function rolling is inflexible by design. It is designed to guarantee the correct tax payable in a year.
#' @useDynLib grattan
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom magrittr %<>%
#' @import data.table
#' @return the total personal income tax payable
#' @export 

income_tax <- function(income, fy.year, age = 42, family_status = "individual", sample_file, .dots.ATO = NULL, return.mode = "numeric", allow.forecasts = FALSE){
  if (allow.forecasts || any(!(fy.year %in% tax_tbl$fy_year))){
    stop("rolling income tax not intended for future years or years before 2003-04. Consider old_income_tax() or new_income_tax().")
  }
  
  if (any(income < 0)){
    warning("Negative entries in income detected. These will have value NA.")
  }
  
  rolling_income_tax(income = income, fy.year = fy.year, age = age, family_status = family_status, sample_file = sample_file, .dots.ATO = .dots.ATO)
}


# rolling_income_tax is inflexible by design: returns the tax payable in the fy.year; no scope for policy change.
rolling_income_tax <- function(income, 
                               fy.year, 
                               age = 42, # answer to life, more importantly < 65.
                               family_status = "individual",
                               sample_file,
                               .dots.ATO = NULL, 
                               return.mode = "numeric"){
  # CRAN NOTE avoidance
  fy_year <- NULL; marginal_rate <- NULL; lower_bracket <- NULL; tax_at <- NULL; n <- NULL; tax <- NULL; ordering <- NULL; max_lito <- NULL; min_bracket <- NULL; lito_taper <- NULL; sato <- NULL; taper <- NULL; rate <- NULL; max_offset <- NULL; upper_threshold <- NULL; taper_rate <- NULL
  
  if (missing(fy.year)){
    stop("fy.year is missing, with no default")
  }
  

  
  if (return.mode != "numeric"){
    stop("return.mode must currently be set to numeric only")
  }
  # Assume everyone of pension age is eligible for sapto.
  sapto.eligible = age >= 65
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  prohibit_vector_recycling(income, fy.year)

  # tax_table2 provides the raw tax tables, but also the amount
  # of tax paid at each bracket, to make the rolling join 
  # calculation later a one liner.
  
  tax_table2 <- 
    tax_tbl %>%
    dplyr::group_by(fy_year) %>%
    dplyr::mutate(
      tax_at = cumsum(data.table::shift(marginal_rate, type = "lag", fill = 0) * (lower_bracket - data.table::shift(lower_bracket, type = "lag", fill = 0))),
      income = lower_bracket) %>%
    data.table::setkey(fy_year, income) %>%
    dplyr::select(fy_year, income, lower_bracket, marginal_rate, tax_at)
  
  input <- 
    data.table::data.table(income = income, 
                           fy_year = fy.year) %>% 
    dplyr::mutate(ordering = 1:n()) 
  
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

  
  medicare_levy <- function(income, fy.year,
                            sapto.eligible,
                            family_status = "individual"){
    # Temporary. The system table should have sapto
    medicare_tbl_indiv <- 
      medicare_tbl_indiv %>%
      dplyr::mutate(sapto = as.logical(sato), 
                    family_status = "individual") %>%
      data.table::setkeyv(c("fy_year", "sapto", "family_status")) %>%
      unique
    
    data.table::data.table(income = income, 
                           fy_year = fy.year,
                           sapto = sapto.eligible, 
                           family_status = family_status) %>%
      merge(medicare_tbl_indiv, 
            by = c("fy_year", "sapto", "family_status"),
            sort = FALSE, 
            all.x = TRUE) %>%
      dplyr::mutate(medicare_levy = pminV(pmaxC(taper * (income - lower_bracket), 
                                                0), 
                                          rate * income)) %$%
      medicare_levy
  }
  
  base_tax. <- tax_fun(income, fy.year = fy.year)
  medicare_levy. <- medicare_levy(income, fy.year = fy.year, sapto.eligible = sapto.eligible)
  lito. <- .lito(income, fy.year)
  
  if (!is.null(.dots.ATO) && !missing(.dots.ATO)){
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
  temp_budget_repair_levy. <- ifelse(fy.year %in% c("2014-15", "2015-16", "2016-17"), 
                                     pmaxC(0.02 * (income - 180e3), 0), 
                                     0)
  
  pmaxC(base_tax. + medicare_levy. - lito. - sapto. + temp_budget_repair_levy., 
        0)
}

old_income_tax <- function(income, fy.year = "2012-13", include.temp.budget.repair.levy = FALSE, return.mode = "numeric", age = 44, age_group, is.single = TRUE){
  stopifnot(length(fy.year) == 1L)
  # If not applicable:
  LITO <- 0
  SAPTO <- .sapto(income, age, age_group, is.single, fy.year = fy.year)
  medicare.levy <- 0.015 * income
  flood.levy <- 0
  
  if (fy.year == "2017-18" || fy.year == "2015-16"){
    warning("Uhh, you're applying a (plausible) tax rate to 2017-18.")
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income < 180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    
    # Assumed the levy will go.
    if(isTRUE(include.temp.budget.repair.levy)){
      temp.budget.repair.levy <- ifelse(income < 180e3,
                                        0,
                                        0.02 * (income - 180e3))
    } else {
      temp.budget.repair.levy <- 0
    }
    
    medicare.levy <- ifelse(income < 20896,
                            0,
                            ifelse(income < 26121,
                                   0.10 * (income - 20896),
                                   0.02 * income))
    
    medicare.surcharge <- 0
    
    LITO <- ifelse(income < 37000, 445,
                   ifelse(income < 66667, 445 - ((income - 37000)*0.015),
                          0))
    
    out <- pmax(tax + temp.budget.repair.levy + medicare.levy + medicare.surcharge - LITO - SAPTO, 0)
  }
  
  if (fy.year == "2014-15"){
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income < 180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    
    if(isTRUE(include.temp.budget.repair.levy)){
      temp.budget.repair.levy <- ifelse(income < 180e3,
                                        0,
                                        0.02 * (income - 180e3))
    } else {
      temp.budget.repair.levy <- 0
    }
    
    medicare.levy <- ifelse(income < 20542, 
                            0,
                            ifelse(income <= 20542,
                                   0.1 * (income - 20542),
                                   0.02 * income))
    
    # We assume no-one pays the medicare surcharge
    medicare.surcharge <- 0 * income * ifelse(income <= 88000,
                                          0,
                                          ifelse(income <= 102000,
                                                 0.01,
                                                 ifelse(income <= 136000,
                                                        0.0125,
                                                        0.015)))
    LITO <- ifelse(income < 37000, 445,
                   ifelse(income < 66667, 445 - ((income - 37000)*0.015),
                          0))
    
    out <- pmax(tax + temp.budget.repair.levy + medicare.levy + medicare.surcharge - LITO - SAPTO, 0)
  }
  

  
  if (fy.year == "2013-14"){
    
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income<180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    #ATO
    medicare.levy <- ifelse(income < 20542,0, 
                            ifelse(income < 24167, (income - 20542)*.1,
                                   0.015*income))
    # https://www.ato.gov.au/Individuals/Medicare-levy/Medicare-levy-surcharge/
    # no medicare surcharge
    medicare.surcharge <- 0 * income * ifelse(income <= 88000,
                                          0,
                                          ifelse(income <= 102000,
                                                 0.01,
                                                 ifelse(income <= 136000,
                                                        0.0125,
                                                        0.015)))
    
    
    LITO <- ifelse(income < 37000, 445,
                   ifelse(income < 66667, 445 - ((income - 37000)*0.015),
                          0))
    
    out <- pmax(tax + medicare.levy + medicare.surcharge - LITO - SAPTO, 0)
    
  }
  
  if (fy.year == "2012-13"){
    ML.lower <- 20542
    ML.upper <- 24167
    medicare.base.rate <- 0.015
    
    LITO <- ifelse(income < 37000, 
                   445,
                   ifelse(income < 66667, 
                          445 - ((income - 37000)*0.015),
                          0))
    
    tax <- ifelse(income < 18200, 
                  0, 
                  ifelse(income < 37000, 
                         0.19*(income - 18200), 
                         ifelse(income < 80000, 
                                3572 + 0.325*(income - 37000),
                                ifelse(income < 180000, 
                                       17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    # medicare.levy 
    
    
    medicare.levy <- ifelse (income <= ML.lower,
                             0,
                             ifelse(income > ML.lower & income <= ML.upper,
                                    0.10 * (income - 20542),
                                    0.015 * income
                             )
    )
                             
    out <- pmax(tax + medicare.levy - LITO - SAPTO, 0)
  }
  
  
  if (fy.year %in% c("2011-12", "2010-11")){
    tax <- ifelse(income < 6000, 0, 
                  ifelse(income < 37000, (income - 6000) * 0.15, 
                         ifelse(income < 80000, 4650 + (income - 37000) * 0.30,
                                ifelse(income < 180000, 17550 + (income - 80000) * 0.37, 
                                       54550 + 0.45*(income - 180000)))))
    
    #http://www.lewistaxation.com.au/tax/historic-tax/medicare-levy-historical
    medicare.levy <- ifelse(income < 19405,0, 
                            ifelse(income < 22829, (income - 19405)*.1,
                                   0.015*income))
    #Plunky
    if (fy.year == "2011-12"){
      flood.levy <- ifelse(income < 50000, 0,
                           ifelse(income < 100000, (income - 50000) * 0.005, 
                                  250 + (income - 100000)*0.01))
    } else {
      flood.levy <- 0
    }
    
    LITO <- ifelse(income < 30000, 1500,
                   ifelse(income < 65000, 1500 - ((income - 30000)*0.04),
                          0))
                          
    out <- pmax(tax + medicare.levy + flood.levy - LITO - SAPTO, 0)
    
  }
  
  if (fy.year == "2009-10"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 35e3,
                         0.15 * (income - 6000),
                         ifelse(income < 80e3,
                                4350 + 0.30*(income - 35e3),
                                ifelse(income < 180e3,
                                       17850 + 0.38*(income - 80e3),
                                       55850 + 0.45*(income - 180e3))
                         )
                  )
    )
    
    medicare.levy <- ifelse(income < 18840, 
                            0, 
                            ifelse(income < 22164,
                                   0.100*(income - 18840),
                                   0.015*income)
    )
    
    
    # http://www.lewistaxation.com.au/tax/historic-tax/low-income-tax-offset-historical
    LITO <- ifelse(income < 30e3,
                   1350,
                   ifelse(income < 63750,
                          1350 - 0.04*(income - 30e3),
                          0))
    
    out <- pmax(tax + medicare.levy  - LITO, 0)
    
  }
  
  if (fy.year == "2008-09"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 34000,
                         0.15 * (income - 6000),
                         ifelse(income < 80000,
                                4200 + 0.30*(income - 34000),
                                ifelse(income < 180e3,
                                       18000 + 0.40*(income - 80000),
                                       58000 + 0.45*(income - 180e3)))))
    
    LITO <- ifelse(income < 30e3,
                   1200,
                   ifelse(income < 60e3,
                          1200 - 0.04 * (income - 30e3),
                          0))
  }
  
  if (fy.year == "2007-08"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 30e3,
                         0.15 * (income - 6000),
                         ifelse(income < 75e3,
                                3600 + 0.30*(income - 30e3),
                                ifelse(income < 150e3,
                                       17100 + 0.40*(income - 75e3),
                                       47100 + 0.45*(income - 150e3)))))
    
    LITO <- ifelse(income < 30e3,
                   750,
                   ifelse(income < 48750,
                          750 - 0.04 * (income - 30e3),
                          0))
    
    
  }
  
  if (fy.year == "2006-07"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 25e3,
                         0.15 * (income - 6000),
                         ifelse(income < 75e3,
                                2850 + 0.30 * (income - 25e3),
                                ifelse(income < 150e3,
                                       17850 + 0.40 * (income - 75e3),
                                       47850 + 0.45 * (income - 150e3)))))
    
    LITO <- ifelse(income < 25e3,
                   600,
                   ifelse(income < 40e3,
                          600 - 0.04 * (income - 25e3),
                          0))
  }
  
  if (fy.year == "2005-06"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.15 * (income - 6000),
                         ifelse(income < 63e3,
                                2340 + 0.30*(income - 21600),
                                ifelse(income < 95e3,
                                       14760 + 0.42*(income - 63e3),
                                       28200 + 0.47*(income - 95e3)))))
  }
  
  if (fy.year == "2004-05"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.17 * (income - 6000),
                         ifelse(income < 58e3,
                                2652 + 0.30*(income - 21601),
                                ifelse(income < 70e3,
                                       13572 + 0.42*(income - 58e3),
                                       18612 + 0.47*(income - 70e3)))))
  }

  
  if (fy.year == "2003-04"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.17*(income - 6000),
                         ifelse(income < 52e3,
                                2652 + 0.30 * (income - 21600),
                                ifelse(income < 62500,
                                       11771 + 0.42*(income - 52000),
                                       16182 + 0.47*(income - 62500)))))
  }
  
  #
  #
  
  if (fy.year %in% grattan::yr2fy(2004:2010))
    out <- pmax(tax + medicare.levy + flood.levy - LITO, 0)
  
  
  if (return.mode == "integer")
    return(as.integer(floor(out)))
  else
    return(out)
}
