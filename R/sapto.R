#' Seniors and Pensioner Tax Offset
#' 
#' @name sapto
#' @param rebate_income The rebate income of the individual.
#' @param fy.year The financial year in which sapto is to be calculated.
#' @param fill If SAPTO was not applicable, what value should be used?
#' @param sapto.eligible Is the individual eligible for sapto?
#' @param family_status Family status of the individual.
#' @export

sapto <- function(rebate_income,
                  fy.year,
                  fill = 0,
                  sapto.eligible = TRUE,
                  Spouse_income = 0,
                  family_status = "single"){
  upper_threshold <- taper_rate <- max_offset <- NULL
  input <- data.table(fy_year = fy.year, 
                      family_status = family_status, 
                      sapto.eligible = sapto.eligible,
                      rebate_income = rebate_income,
                      Spouse_income = Spouse_income)
  
  if (any(Spouse_income > 0 & family_status == "single")){
    stop("family_status may be 'single' if and only if Spouse_income > 0.")
  }
  
  ordering <- NULL
  input[, ordering := 1:.N]
  
  setkeyv(input, c("fy_year", "family_status"))
  
  
  my_printer <- function(x){print(x); x}
  
  out <- 
    sapto_tbl[input] %>%
    .[, sapto := pmaxC(pminV(max_offset, 
                             upper_threshold * taper_rate - rebate_income * taper_rate),
                       0)] %>%
    .[, partner_sapto := pmaxC(pminV(max_offset, 
                                     upper_threshold * taper_rate - Spouse_income * taper_rate),
                               0)] %>%
    # Transfer unutilized SAPTO:
    .[, sapto := sapto + pminC(max_offset - partner_sapto, 0)] %>%
    setkey(ordering) %>%
    unique(by = key(.)) %>%
    .[["sapto"]]
  
  # Eligibility for SAPTO
  out[!sapto.eligible] <- 0
  out[is.na(out)] <- fill
  out
}