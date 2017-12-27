#' Unemployment benefit
#' @description Calculates the unemployment benefit (Newstart Allowance) payable for individuals in the specified financial year(s), 
#' given each individual's income and assets, and whether they are married, have children, or own their own home.
#' @param income Numeric vector of fortnightly income for the income test. 
#' @param assets Numeric vector of the value of assets. By default, \code{income} and \code{assets} are both zero, thus returning the maximum benefit payable.
#' @param fy.year A character vector of valid financial years between "2000-01" and "2020-21" specifying which financial year the allowance is to be calculated.
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

unemployment_benefit <- function(income = 0,
                                 assets = 0,
                                 fy.year,
                                 has_partner = FALSE,
                                 has_dependant = FALSE,
                                 is_home_owner = FALSE) {
  if (missing(fy.year)) {
    stop("`fy.year` is missing, with no default.")
  }
  
  permitted_fys <-
    c("2000-01", "2001-02", "2002-03",
      "2003-04", "2004-05", "2005-06", 
      "2006-07", "2007-08", "2008-09",
      "2009-10", "2010-11", "2011-12", 
      "2012-13", "2013-14", "2014-15",
      "2015-16", "2016-17", "2017-18", 
      "2018-19", "2019-20", "2020-21")
  
  if (!all(fy.year %chin% permitted_fys)) {
    if (any(!is.fy(fy.year))) {
      i <- which(!is.fy(fy.year))
      i1 <- i[1]
      if (length(i) > 1) {
        stop("`fy.year` contained invalid FYs. ",
             "There were ",
             length(i), " invalid entries (", length(i) / length(fy.year), "%).\n\n",
             "First invalid FY:\n\t", fy.year[i1], "\n",
             "at position ", i)
      } else {
        if (length(fy.year) == 1L) {
          stop("`fy.year` set to '", fy.year, "', which is not a valid FY. ",
               "Select a valid fy.year between ",
               permitted_fys[1], " and ", last(permitted_fys), ".")
        } else {
          stop("`fy.year` contained invalid entry ",
               fy.year[i1], " at position ", i1, ".")
        }
      }
    }
    i <- which(fy.year %notin% permitted_fys)
    i1 <- i[1]
    
    if (length(fy.year) == 1L) {
      stop("`fy.year = ", fy.year, "` was not within the allowed range: ",
           permitted_fys[1], " <= fy.year <= ", last(permitted_fys))
    } else {
      stop("`fy.year` were not within the allowed range: ",
           permitted_fys[1], " <= fy.year <= ", last(permitted_fys), "\n\n",
           "First invalid FY:\n\t", fy.year[i], "\n",
           "at position ", i)
    }
  }
  
  
  prohibit_vector_recycling(income,
                            assets,
                            fy.year,
                            has_partner,
                            has_dependant,
                            is_home_owner)
  
  input <- 
    data.table(income, 
               assets, 
               fy_year = fy.year, 
               HasPartner = has_partner,
               HasDependant = has_dependant, 
               HomeOwner = is_home_owner)
  
  MBR <- ES <- 
    taper_1 <- taper_2 <-
    IncomeThreshold_1 <- IncomeThreshold_2 <- NULL
  
  output <-
    unemployment_income_tests[input,
                              on = c("fy_year",
                                     "HasPartner",
                                     "HasDependant")] %>%
    unemployment_annual_rates[., on = c("fy_year",
                                        "HasPartner", 
                                        "HasDependant")] %>%
    unemployment_assets_tests[.,
                              on = c("fy_year", 
                                     "HasPartner",
                                     "HomeOwner")] %>%
    # Do asset test during
    .[, out := 0] %>%
    # assets ok?
    .[, ok := asset_cutout > assets]
    .[(ok), out := MBR + ES] %>%
    .[(ok), out := out - taper_1 * pmaxC(income - IncomeThreshold_1, 0)] %>%
    .[(ok), out := out - taper_2 * pmaxC(income - IncomeThreshold_2, 0)]
    
  .subset2(output, "out")
}



