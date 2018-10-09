#' Youth unemployment
#' @param income Numeric vector of fortnightly income for the income test. 
#' @param assets Numeric vector of the value of assets. By default, \code{income} and \code{assets} are both zero, thus returning the maximum benefit payable.
#' @param fy.year A character vector of valid financial years between "2000-01" and "2020-21" specifying which financial year the allowance is to be calculated.
#' @param Date (Date vector or coercible to such). An alternative to \code{fy.year} to specify the period over which the allowance is calculated.
#' @param has_partner (logical, default: \code{FALSE}) Does the individual have a partner?
#' @param has_dependant (logical, default: \code{FALSE}) Does the indvidiual have any dependant children?
#' @param age Age (only determines whether the 16-17 age or 18 or over rates will apply).
#' @param lives_at_home (logical, default: \code{FALSE}) Is the individual a dependant who lives at home?
#' @param independent (logical, default: \code{TRUE}) Should the individual be considered independent.
#' @param unemployed (logical, default: \code{FALSE}) Is the individual unemployed?
#' @return The fortnightly unemployment benefit payable for each entry. 
#' The function is vectorized over its arguments, with any length-1 argument
#' recycled. (Other vector recycling is not supported and will result in an error.)
#' 
#' @export

youth_unemployment <- function(income = 0,
                               assets = 0,
                               fy.year = NULL,
                               Date = NULL,
                               has_partner = FALSE,
                               has_dependant = FALSE,
                               age = 23,
                               lives_at_home = FALSE,
                               independent = TRUE,
                               unemployed = FALSE) {
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(income,
                                        assets,
                                        has_partner,
                                        has_dependant,
                                        lives_at_home)
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      fy.year <- date2fy(Sys.Date())
      message("`fy.year` not set, so using fy.year = ", fy.year)
    }
    
    if (length(fy.year) != max.length &&
        max.length != 1L) {
      
      Lengths <- 
        vapply(list(income,
                    assets,
                    has_partner,
                    has_dependant,
                    lives_at_home),
               length,
               integer(1))
      
      stop("`fy.year` had length ", length(fy.year), ". ",
           "Ensure it has length-1 or length-", max.length, ", ",
           "the maximum of the lengths of the arguments ",
           "length(", names(Lengths)[which.max(Lengths)], ").")
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
                    lives_at_home),
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
  
  Age <- as.integer(age)
  
  input <-
    data.table(income = income,
               assets = assets,
               fy_year = fy.year %||% date2fy(Date), 
               Age = Age,
               Age16or17 = Age == 16L | Age == 17L,
               HasPartner = has_partner,
               HasDependant = has_dependant,
               LivesAtHome = lives_at_home, 
               Unemployed = unemployed,
               Independent = independent)
  input[, "ordering" := .I]
  
  MBR <- ES <- 
    taper_1 <- taper_2 <-
    IncomeThreshold_1 <- IncomeThreshold_2 <- NULL
  
  multiple <- 
    if (is.null(Date)) {
      # number of fortnights in a financial year
      26
    } else {
      1
    }
  
  out <- ok <- NULL
  
  Age <- Unemployed <- Independent <- NULL
  
  output <-
    youth_income_tests[input, on = c("fy_year")] %>%
    youth_annual_rates[., on = c("fy_year",
                                 "HasPartner", 
                                 "HasDependant",
                                 "LivesAtHome")] %>%
    .[, out := 0] %>%
    .[Age >= 21 + (fy_year >= "2012-13"), Independent := TRUE] %>%
    .[, ok := or(Independent, coalesce(Unemployed, FALSE))] %>%
    .[(ok), out := MBR + ES] %>%
    .[(ok), out := out - taper_1 * pmaxC(pminV(income, IncomeThreshold_1) - IncomeThreshold_2,
                                         0)] %>%
    .[(ok), out := out - taper_2 * pmaxC(income - IncomeThreshold_2,
                                         0)]
    
  .subset2(output, "out") * multiple
}

