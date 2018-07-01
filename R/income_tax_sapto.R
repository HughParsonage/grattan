#' Income tax payable as a function of SAPTO
#' 
#' @name income_tax_sapto
#' @param income The individual assessable income.
#' @param fy.year The financial year in which the income was earned. Only tax years from 2000-01 to 2016-17 are available. If fy.year is not given, the current financial year is used by default.
#' @param age The individual's age.
#' @param family_status For Medicare and SAPTO purposes.
#' @param n_dependants An integer for the number of children of the taxpayer (for the purposes of the Medicare levy).
#' @param return.mode The mode (numeric or integer) of the returned vector.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files. If \code{.dots.ATO} is a \code{data.table}, I recommend you enclose it with \code{copy()}.
#' @param allow.forecasts should dates beyond 2016-17 be permitted? Currently, not permitted.
#' @param sapto.eligible Specify explicitly the eligibility for SAPTO. If missing, defaults to ages over 65.
#' @param medicare.sapto.eligible Specify explicitly the eligibility for SAPTO with respect to the Medicare levy for low-income earners. If missing, defaults to ages over 65.
#' @param new_sapto_tbl If not \code{NULL}, supplied to \code{\link{new_sapto}}. Otherwise, \code{fy.year} is passed to \code{\link{sapto}}.
#' @details Used to cost simple changes to SAPTO.
#' @export income_tax_sapto

income_tax_sapto <- function(income,
                             fy.year=NULL,
                             age = 42,
                             family_status = "individual",
                             n_dependants = 0L,
                             return.mode = c("numeric", "integer"),
                             .dots.ATO = NULL,
                             allow.forecasts = FALSE,
                             sapto.eligible, 
                             medicare.sapto.eligible, 
                             new_sapto_tbl = NULL){
  if (is.null(fy.year)){
    fy.year <- date2fy(Sys.Date())
    warning("fy.year is missing, using current financial year")
  }
  
  if (!identical(allow.forecasts, FALSE)) {
    .NotYetUsed("allow.forecasts")
  }
  
  return.mode <- match.arg(return.mode)
  
  # CRAN NOTE avoidance
  fy_year <- marginal_rate <- lower_bracket <- tax_at <- n <- tax <- ordering <- max_lito <- min_bracket <- lito_taper <- sato <- taper <- rate <- max_offset <- upper_threshold <- taper_rate <- medicare_income <- lower_up_for_each_child <-NULL
  
  if (missing(sapto.eligible)){
    # Assume everyone of pension age is eligible for sapto.
    sapto.eligible <- age >= 65
  }
  
  if (missing(medicare.sapto.eligible)){
    medicare.sapto.eligible <- sapto.eligible
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
  
  input <- input[ ,ordering := 1:.N]
  setkeyv(input, "fy_year")
  
  # input.keyed <-
  #   # potentially expensive. Another way would be 
  #   # to add an argument such as data.table.copy = FALSE
  #   # to allow different way to preserve the order
  #   copy(input) %>%
  #   setkey(fy_year, income)
  
  tax_fun <- function(income, fy.year){
    setkey(input, fy_year, income)
    
    tax_table2[input, roll = Inf] %>%
      .[,tax := tax_at + (income - lower_bracket) * marginal_rate] %>%
      setorderv("ordering") %>%
      .[["tax"]]
  }
  
  # If .dots.ATO  is NULL, for loops over zero-length vector
  for (j in which(vapply(.dots.ATO, FUN = is.double, logical(1)))){
    set(.dots.ATO, j = j, value = as.integer(.dots.ATO[[j]]))
  }
  
  base_tax. <- tax_fun(income, fy.year = fy.year)
  
  if (missing(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
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
                  family_status = if (missing(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
                    family_status
                  } else {
                    FS <- rep_len("individual", max.length)
                    FS[the_spouse_income > 0] <- "family"
                    FS
                  }, 
                  n_dependants = n_dependants, 
                  .checks = FALSE)
  
  lito. <- .lito(input)
  
  if (!is.null(.dots.ATO) && !missing(.dots.ATO) && all(c("Rptbl_Empr_spr_cont_amt",
                                                          "Net_fincl_invstmt_lss_amt",
                                                          "Net_rent_amt", 
                                                          "Rep_frng_ben_amt") %in% names(.dots.ATO))){
    if (is.null(new_sapto_tbl)){
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
      sapto. <-
        sapto.eligible * new_sapto(rebate_income = rebate_income(Taxable_Income = income,
                                                                     Rptbl_Empr_spr_cont_amt = .dots.ATO[["Rptbl_Empr_spr_cont_amt"]],
                                                                     Net_fincl_invstmt_lss_amt = .dots.ATO[["Net_fincl_invstmt_lss_amt"]],
                                                                     Net_rent_amt = .dots.ATO[["Net_rent_amt"]],
                                                                     Rep_frng_ben_amt = .dots.ATO[["Rep_frng_ben_amt"]]), 
                                   new_sapto_tbl = new_sapto_tbl, 
                                   Spouse_income = the_spouse_income,
                                   family_status = {
                                     FS_sapto <- rep_len("single", max.length)
                                     FS_sapto[the_spouse_income > 0] <- "married"
                                     FS_sapto
                                   },
                                   sapto.eligible = TRUE)
    }
  } else {
    if (is.null(new_sapto_tbl)){
      sapto. <- sapto.eligible * sapto(rebate_income = rebate_income(Taxable_Income = income), 
                                       fy.year = fy.year, 
                                       sapto.eligible = TRUE)
    } else {
      sapto. <-
        sapto.eligible * new_sapto(rebate_income = rebate_income(Taxable_Income = income), 
                                   new_sapto_tbl = new_sapto_tbl, 
                                   sapto.eligible = TRUE)
    }
  }
  
  # https://www.legislation.gov.au/Details/C2014A00048
  # input[["fy_year"]] ensures it matches the length of income if length(fy.year) == 1.
  temp_budget_repair_levy. <- (input[["fy_year"]] %in% c("2014-15", "2015-16", "2016-17") & income > 180e3) * (0.02 * (income - 180e3))
  
  switch(return.mode,
         "integer" = as.integer(pmaxC(base_tax. - lito. - sapto., 
                                       0) + medicare_levy. + temp_budget_repair_levy.),
         pmaxC(base_tax. - lito. - sapto., 
               0) + medicare_levy. + temp_budget_repair_levy.)
}


