#' Senior Australians and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param family_status Family status of the individual.

sapto <- function(rebate_income, fy.year, fill = 0,  sapto.eligible, family_status = "single"){
  upper_threshold <- NULL; taper_rate <- NULL; max_offset <- NULL
  input <- dplyr::data_frame(fy_year = fy.year, 
                             family_status = family_status, 
                             sapto.eligible = sapto.eligible,
                             rebate_income = rebate_income)
  
  out <- 
    dplyr::left_join(input, 
                     sapto_tbl, 
                     by = c("fy_year", "family_status")) %>%
    dplyr::mutate(sapto = pmaxC(pminV(max_offset, 
                                      upper_threshold * taper_rate - rebate_income * taper_rate),
                                0)) %$%
    sapto
  
  # Eligibility for SAPTO
  out[!sapto.eligible] <- 0
  out[is.na(out)] <- fill
  out
}