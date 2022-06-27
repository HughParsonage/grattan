#' Small Business Tax Offset
#' @param taxable_income Individual's assessable income.
#' @param basic_income_tax_liability Tax liability (in dollars) according to the method in the box in s 4.10(3) of the \emph{Income Tax Assessment Act 1997} (Cth). In general, \code{basic_income_tax_liability} is the ordinary tax minus offsets. In particular, it does not include levies (such as the Medicare levy or the Temporary Budget Repair Levy). 
#' \if{latex}{\deqn{\mathrm{Income\;Tax} = \mathrm{Taxable\;income\times\mathrm{Rate}-\mathrm{Tax\;offsets}}}}
#' For example, in 2015-16, an individual with an assessable income of 100,000 had a basic tax liability of 
#' approximately 25,000.
#' 
#' @param .dots.ATO A \code{data.table} of tax returns.
#' If provided, it must contain the variables
#'  \code{Total_PP_BE_amt},
#'  \code{Total_PP_BI_amt},
#'  \code{Total_NPP_BE_amt},
#'  \code{Total_NPP_BI_amt}.
#' If both \code{.dots.ATO} and either \code{aggregated_turnover} or \code{total_net_small_business_income} are provided, 
#' \code{.dots.ATO} takes precedence, with a warning.
#' 
#' If \code{.dots.ATO} contains the variable \code{Tot_net_small_business_inc}, it is used instead of the income variables.
#' 
#' @param aggregated_turnover A numeric vector the same length as \code{taxable_income}.
#' Only used to determine whether or not the offset is applicable; that is, the offset only
#' applies if aggregated turnover is less than 2 million.
#' 
#' Aggregated turnover of a taxpayer is the sum of the following:
#' \itemize{
#' \item{the taxpayer's annual turnover for the income year,}
#' \item{the annual turnover of any entity connected with the taxpayer's, for that part of the income year that the entity is connected with the taxpayer's}
#' \item{the annual turnover of any entity that is an affiliate of the taxpayer, for that part of the income year that the entity is affiliated with the taxpayer's}
#' \item{When you calculate aggregated turnover for an income year, do not include either:}
#' \itemize{
#' \item{the annual turnover of other entities for any period of time that the entities are either not connected with the taxpayer or are not the taxpayer's affiliate, or}
#' \item{amounts resulting from any dealings between these entities for that part of the income year that the entity is connected or affiliated with the taxpayer.}
#' }
#' }
#' \url{https://www.ato.gov.au/Business/Research-and-development-tax-incentive/Claiming-the-tax-offset/Steps-to-claiming-the-tax-offset/Step-3---Calculate-your-aggregated-turnover/}
#' @param total_net_small_business_income Total net business income within the meaning of the Act. For most taxpayers, this is simply any net income from a business they own (or their share of net income from a business in which they have an interest). The only difference being in the calculation of the net business income of some minors (vide Division 6AA of Part III of the Act).
#' @param fy_year The financial year for which the small business tax offset is to apply.
#' @param tax_discount If you do not wish to use the legislated discount rate from a particular \code{fy_year}, 
#' you can specify it via \code{tax_discount}. If both are provided, \code{tax_discount} prevails, with a warning.
#' @source
#' Basic income tax method s4-10(3) \url{http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html}.
#' Explanatory memorandum \url{https://github.com/HughParsonage/grattan/blob/master/data-raw/parlinfo/small-biz-explanatory-memo-2015.pdf} from the original \verb{http://parlinfo.aph.gov.au/parlInfo/download/legislation/ems/r5494_ems_0a26ca86-9c3f-4ffa-9b81-219ac09be454/upload_pdf/503041.pdf}.
#' @export


small_business_tax_offset <- function(taxable_income,
                                      basic_income_tax_liability,
                                      .dots.ATO = NULL,
                                      aggregated_turnover = NULL,
                                      total_net_small_business_income = NULL,
                                      fy_year = NULL,
                                      tax_discount = NULL) {
  
  if (!is.null(fy_year) && length(fy_year) == 1L && fy_year < "2015-16") {
    double(length(taxable_income))
  } else {
    if (!is.null(.dots.ATO)) {
      if (!is.null(aggregated_turnover)) {
        warning("Both `.dots.ATO` and `aggregated_turnover` were provided. ",
                "`aggregated_turnover` will be ignored.")
      }
      if (!is.null(total_net_small_business_income)) {
        warning("Both `.dots.ATO` and `total_net_small_business_income` were provided. ",
                "`total_net_small_business_income` will be ignored.")
      }
      
      if (all(c("Total_PP_BE_amt", 
                "Total_PP_BI_amt", 
                "Total_NPP_BE_amt",
                "Total_NPP_BI_amt") %chin% names(.dots.ATO))) {
        aggregated_turnover <-
          .subset2(.dots.ATO, "Total_PP_BI_amt") + 
          .subset2(.dots.ATO, "Total_NPP_BI_amt")
        
        if ("Tot_net_small_business_inc" %chin% names(.dots.ATO)) {
          total_net_small_business_income <-
            .subset2(.dots.ATO, "Tot_net_small_business_inc")
        } else {
          total_net_small_business_income <-
            aggregated_turnover -
            .subset2(.dots.ATO, "Total_PP_BE_amt") - 
            .subset2(.dots.ATO, "Total_NPP_BE_amt")
        }
      } else {
        stop("\n`.dots.ATO` was provided but does not contain the necessary variables. ", 
             "Ensure `.dots.ATO`, if provided, has the following variables:\n\n\t",
             paste0(c("Total_PP_BE_amt", 
                      "Total_PP_BI_amt", 
                      "Total_NPP_BE_amt",
                      "Total_NPP_BI_amt"), sep = "\n\t"))
      }
    } else {
      max.length <-
        prohibit_vector_recycling.MAXLENGTH(taxable_income,
                                            basic_income_tax_liability,
                                            aggregated_turnover,
                                            total_net_small_business_income)
    }
    
    # 1.26 explanatory memorandum
    total_net_small_business_income <- pmax0(total_net_small_business_income)
    
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
      
      if (!is.null(fy_year)) {
        warning("Both `fy_year` and `tax_discount` are provided. ",
                "`fy_year` will be ignored.")
      }
      smbto_p <- tax_discount
    }
    out <- prop_tax_for_biz * smbto_p
    out[is.nan(out)] <- 0
    out <- pminC(out, 1000)
    
    # Only business with an aggregate annual turnover of less than $2M
    out[aggregated_turnover >= 2e6] <- 0
    return(out)
  }
}
