#' Senior Australians and Pensioner Tax Offset

sapto <- function(rebate_income, fy.year, family_status = "single"){
  input <- dplyr::data_frame(fy_year = fy.year, 
                             family_status = family_status, 
                             rebate_income = rebate_income)
  
  dplyr::left_join(input, .sapto_tbl, 
                   by = c("fy_year", "family_status")) %>%
    dplyr::mutate(sapto = pmaxC(pminV(max_offset, 
                                      upper_threshold * taper_rate - rebate_income * taper_rate),
                                0)) %$%
    sapto
}