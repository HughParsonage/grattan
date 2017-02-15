#' Beneficiary tax offset
#' @param benefit_amount The amount of Tax Offsetable benefit received by the taxpayer during the income year.
#' @param fy.year The income year. 
#' @return The beneficiary tax offset. 
#' @section WARNING:
#' This function disagrees with the ATO online calculator. 

bto <- function(benefit_amount, fy.year){
  fys_with_bto <- unique(bto_tbl[["fy_year"]])
  if (any(fy.year %notin% fys_with_bto)){
    stop("fy.year must be in the range ", min(fys_with_bto), " to ", max(fys_with_bto))
  }
  
  # Taxpayer's benefit amount is the amount of Tax Offsetable benefit received by the taxpayer during 
  # the income year, rounded down to the nearest whole dollar.
  
  input <- 
    data.table(benefit_amount = floor(benefit_amount), 
               fy_year = fy.year) %>%
    .[, order := 1:.N] %>%
    setkeyv("fy_year")
  
  
  
  # names(bto_tbl) for CRAN Note avoidance
  fy_year <- lowest_marginal_rate <- tax_free_threshold <- next_threshold <- coefficient_abv_next_threshold <- NULL
  
  output <- 
    merge(input, bto_tbl) %>%
    .[,bto := lowest_marginal_rate * (benefit_amount - tax_free_threshold) + coefficient_abv_next_threshold * pmaxC(benefit_amount - next_threshold, 0)] %>%
    setorderv("order")
  
  # If the Tax Offset amount calculated using the above formulae is not a whole dollar amount, it is rounded 
  # up to the nearest whole dollar.
  pmaxC(ceiling(output[["bto"]]), 0)
}
