#' Beneficiary tax offset
#' @param benefit_amount The amount of Tax Offsetable benefit received by the taxpayer during the income year.
#' @param fy.year The income year. Not used by default.
#' @param rate1 The coefficient in Division 2, section 13(2) of the Income Tax Assessment (1936 Act) Regulation 2015
#' (the regulations).
#' @param benefit_threshold The amount of benefits above which the offset applies.
#' @param tax_threshold The \emph{threshold at the upper conclusion of the lowest marginal tax rate} in the
#' words of the section 13(3) of the regulations.
#' @param rate2 The second coefficient in section 13(3) of the regulations.

#' @return The beneficiary tax offset. 
#' @section WARNING:
#' This function disagrees with the ATO online calculator. 
#' @export

bto <- function(benefit_amount,
                fy.year = NULL,
                rate1 = 0.15,
                benefit_threshold = 6000,
                tax_threshold = 37000,
                rate2 = 0.15) {
  if (is.null(fy.year) || all(fy.year > "2005-06")) {
    pmaxC(ceiling(rate1 * (benefit_amount - benefit_threshold) + rate2 * pmaxC(benefit_amount - tax_threshold, 0)), 0)
  } else {
    fys_with_bto <- .subset2(bto_tbl, "fy_year")
    if (any(fy.year %notin% fys_with_bto)) {
      stop("fy.year must be in the range ", min(fys_with_bto), " to ", max(fys_with_bto))
    }
    
    # Taxpayer's benefit amount is the amount of Tax Offsetable benefit received by the taxpayer during 
    # the income year, rounded down to the nearest whole dollar.
    
    input <- 
      data.table(benefit_amount = floor(benefit_amount), 
                 fy_year = fy.year)
    
    # names(bto_tbl) for CRAN Note avoidance
    lowest_marginal_rate <-
      tax_free_threshold <-
      next_threshold <-
      coefficient_abv_next_threshold <- NULL
    
    output <- 
      bto_tbl[input,
              j = .(out = lowest_marginal_rate * (benefit_amount - tax_free_threshold) + 
                      coefficient_abv_next_threshold * pmaxC(benefit_amount - next_threshold, 0)),
              on = "fy_year"]
    
    # If the Tax Offset amount calculated using the above formulae is not a whole dollar amount, it is rounded 
    # up to the nearest whole dollar.
    pmaxC(ceiling(.subset2(output, "out")), 0)
  }
}
