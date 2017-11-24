#' Small Business Tax Offset
#' @param taxable_income Individual's assessable income.
#' @param basic_income_tax_liability Tax liability (in dollars) according to the method in the box in s 4.10(3) of the \emph{Income Tax Assessment Act 1997} (Cth). In general, \code{basic_income_tax_liability} is the ordinary tax minus offsets. In particular, it does not include levies (such as the Medicare levy or the Temporary Budget Repair Levy). 
#' \if{latex}{\deqn{\mathrm{Income\;Tax} = \mathrm{Taxable\;income\times\mathrm{Rate}-\mathrm{Tax\;offsets}}}}
#' For example, in 2015-16, an individual with an assessable income of \$100,000 had a basic tax liability of 
#' approximately \$25,000.
#' 
#' @param total_net_business_income Total net business income within the meaning of the Act. For most taxpayers, this is simply any net income from a business they own (or their share of net income from a business in which they have an interest). The only difference being in the calculation of the net business income of some minors (vide Division 6AA of Part III of the Act).
#' @param fy_year The financial year for which the small business tax offset is to apply.
#' @source
#' Basic income tax method s4-10(3) \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html}.
#' Explanatory memorandum \url{http://parlinfo.aph.gov.au/parlInfo/download/legislation/ems/r5494_ems_0a26ca86-9c3f-4ffa-9b81-219ac09be454/upload_pdf/503041.pdf}.
#' @export


small_business_tax_offset <- function(taxable_income,
                                      basic_income_tax_liability,
                                      total_net_small_business_income,
                                      fy_year = NULL,
                                      tax_discount = NULL) {
  if (fy_year < "2015-16") {
    return(0)
  } else {
    
    max.length <-
      prohibit_vector_recycling.MAXLENGTH(taxable_income,
                                          basic_income_tax_liability,
                                          total_net_small_business_income)
    
    # See explanatory memorandum: p. 16
    prop_business_income <- total_net_small_business_income / taxable_income
    prop_tax_for_biz <- prop_business_income * basic_income_tax_liability 
    
    # Legislated rates
    if (is.null(tax_discount)) {
      if (is.null(fy_year)) {
        stop("`fy_year` and `tax_discount` are both NULL. ", 
             "Provide one of arguments: fy_year for the settings ", 
             "as legislated in a particular tax year, or ", 
             "tax_discount for a custom discount.")
      } else {
        smbto_p <- double(length(fy_year))
        smbto_p[fy_year == "2015-16"] <- 0.05
        smbto_p[fy_year %chin% c("2016-17", "2017-18", "2018-19", "2019-20",
                                 "2020-21", "2021-22", "2022-23", "2023-24")] <- 0.08
        smbto_p[fy_year == "2024-25"] <- 0.10
        smbto_p[fy_year == "2025-26"] <- 0.13
        smbto_p[fy_year == "2026-27"] <- 0.16
      }
    } else {
      
      if (!is.numeric(tax_discount)) {
        stop("`tax_discount` was class '", storage.mode(tax_discount), "'. ",
             "Replace `tax_discount` with a numeric vector.")
      }
      
      if (length(tax_discount) > 1 && length(tax_discount) != max.length) {
        stop("length(tax_discount) = ", length(tax_discount), ", ", 
             "but the only permissible lengths are 1 or ", max.length, ". ", 
             "Provide a single value or a value for every observation.")
      }
      smbto_p <- tax_discount
    }
    
    pminC(prop_tax_for_biz * smbto_p, 1000)
  }
}
