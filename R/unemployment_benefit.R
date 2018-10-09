#' Unemployment benefit
#' @description Calculates the unemployment benefit (Newstart Allowance) payable for individuals in the specified financial year(s), 
#' given each individual's income and assets, and whether they are married, have children, or own their own home.
#' @param income Numeric vector of fortnightly income for the income test. 
#' @param assets Numeric vector of the value of assets. By default, \code{income} and \code{assets} are both zero, thus returning the maximum benefit payable.
#' @param fy.year A character vector of valid financial years between "2000-01" and "2020-21" specifying which financial year the allowance is to be calculated.
#' @param Date (Date vector or coercible to such). An alternative to \code{fy.year} to specify the period over which the allowance is calculated.
#' @param has_partner (logical vector, default: \code{FALSE}) Does the individual have a partner?
#' @param has_dependant (logical vectpr, default: \code{FALSE}) Does the indvidiual have any dependant children?
#' @param is_home_owner (logical vector, default: \code{FALSE}) Does the individual own their own home?
#' @return The fortnightly unemployment benefit payable for each entry. 
#' The function is vectorized over its arguments, with any length-1 argument
#' recycled. (Other vector recycling is not supported and will result in an error.)
#' 
#' @details The income test for long-term employed persons above 60 happens to be the same as 
#' that for singles with dependants, so calculating the benefit payable for such 
#' individuals can be performed by setting \code{has_partner = FALSE, has_dependant = TRUE}.
#' @export

unemployment_benefit <- function(income = 0,
                                 assets = 0,
                                 fy.year = NULL,
                                 Date = NULL,
                                 has_partner = FALSE,
                                 has_dependant = FALSE,
                                 is_home_owner = FALSE) {
  
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(income,
                                        assets,
                                        has_partner,
                                        has_dependant,
                                        is_home_owner)
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      fy.year <- date2fy(Sys.Date())
      message("`fy.year` not set, so using fy.year = ", fy.year)
    }
    
    if (length(fy.year) != max.length &&
        length(fy.year) != 1L) {
      if (max.length == 1L) {
        max.length <- length(fy.year)
      } else {
        Lengths <- 
          autonamed_list(income,
                         assets,
                         has_partner,
                         has_dependant,
                         is_home_owner) %>%
          vapply(length, integer(1L))
        stop("`fy.year` had length ", length(fy.year), ". ",
             "Ensure it has length-1 or length-", max.length, ", ",
             "the maximum of the lengths of the arguments ",
             "length(", names(Lengths)[which.max(Lengths)], ").")
      }
    }
    
    permitted_fys <-
      c("2000-01", "2001-02", "2002-03",
        "2003-04", "2004-05", "2005-06", 
        "2006-07", "2007-08", "2008-09",
        "2009-10", "2010-11", "2011-12", 
        "2012-13", "2013-14", "2014-15",
        "2015-16", "2016-17", "2017-18", 
        "2018-19", "2019-20", "2020-21")
    
    fy.year <- validate_fys_permitted(fy.year, permitted_fys)
    
  } else {
    if (length(Date) != max.length &&
        max.length != 1L) {
      
      Lengths <- 
        vapply(list(income,
                    assets,
                    has_partner,
                    has_dependant,
                    is_home_owner),
               length,
               integer(1))
      
      stop("`Date` had length ", length(Date), ". ",
           "Ensure it has length-1 or length-", max.length, ", ",
           "the maximum of the lengths of the arguments ",
           "length(", names(Lengths)[which.max(Lengths)], ").")
    }
    
    if (!inherits(Date, "Date")) {
      Date <-
        tryCatch(as.Date(Date),
                 # To avoid arcane error messages when the method is dispatched
                 error = function(e) {
                   stop("`Date` was supplied to `unemployment_benefits()`, ",
                        "but is neither a Date object ",
                        "nor safely coercible as such.\n\n", 
                        "When attempting `as.Date(Date)`, encountered the error:\n\t", e$m, 
                        call. = FALSE)
                 })
    }
    
    if (!all(between(year(Date), 2000L, 2020L))) {
      i_bad_date <- which(!between(year(Date), 2000L, 2020L))
      first_bad_date_i <- i_bad_date[1]
      first_bad_date <- Date[first_bad_date_i]
      stop("`Date` had value ", first_bad_date, " at position ", first_bad_date_i, ". ",
           "Ensure `Date` only includes dates between 2000 and 2020.")
    }
  }
  
  input <- 
    data.table(income, 
               assets, 
               fy_or_date = fy.year %||% Date, 
               hasPartner = has_partner,
               hasDependant = has_dependant, 
               HomeOwner = is_home_owner)
  input[, "ordering" := .I]
  setnames(input,
           "fy_or_date", 
           if (is.null(Date)) "fy_year" else "Date")
  
  
  
  MBR <- ES <- 
    taper_1 <- taper_2 <-
    IncomeThreshold_1 <- IncomeThreshold_2 <- 
    asset_cutout <- NULL
  
  if (is.null(Date)) {
    output <-
      unemployment_income_tests[input,
                                on = c("fy_year",
                                       "hasPartner",
                                       "hasDependant")] %>%
      unemployment_annual_rates[., on = c("fy_year",
                                          "hasPartner", 
                                          "hasDependant")] %>%
      unemployment_assets_tests[.,
                                on = c("fy_year", 
                                       "hasPartner",
                                       "HomeOwner")]
  } else {
    setkeyv(input,
            c("hasPartner",
              "hasDependant",
              "Date"))
    output <-
      unemployment_income_tests_by_date[input,
                                on = c("hasPartner",
                                       "hasDependant",
                                       "Date"),
                                roll = -Inf]
    output <-
      unemployment_rates_by_date[output,
                                 on = c("hasPartner", 
                                        "hasDependant",
                                        "Date"),
                                 roll = -Inf]
    setkeyv(input,
            c("hasPartner",
              "HomeOwner",
              "Date"))
    output <- 
      unemployment_assets_tests_by_date[output,
                                        on = c("hasPartner",
                                               "HomeOwner", 
                                               "Date"),
                                        roll = -Inf]
    setorderv(output, "ordering")
  }

  multiple <- 
    if (is.null(Date)) {
      # number of fortnights in a financial year
      26
    } else {
      1
    }
  
  # Do asset test during
  out <- ok <- NULL
  output %>%
    .[, out := 0] %>%
    # assets ok?
    .[, ok := asset_cutout > assets] %>%
    .[(ok), out := MBR + ES] %>%
    .[(ok), out := out - taper_1 * pmaxC(pminV(income, IncomeThreshold_2) - IncomeThreshold_1,
                                         0)] %>%
    .[(ok), out := out - taper_2 * pmaxC(income - IncomeThreshold_2,
                                         0)] %>%
    
    .subset2("out") * multiple
}



