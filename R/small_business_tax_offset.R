#' Small Business Tax Offset
#' @param taxable_income Individual's taxable income.
#' @param basic_income_tax_liability Ordinary income tax (in dollars) before
#' subtracting tax offsets, as used in s 4-10(3) of the
#' \emph{Income Tax Assessment Act 1997} (Cth). It does not include levies
#' such as the Medicare levy or the Temporary Budget Repair Levy.
#' \if{latex}{\deqn{\mathrm{Income\;Tax} = \mathrm{Taxable\;income\times\mathrm{Rate}-\mathrm{Tax\;offsets}}}}
#' For example, in 2015-16, an individual with an assessable income of 100,000 had a basic tax liability of 
#' approximately 25,000.
#' 
#' @param .dots.ATO A data frame with eligible sole-trader income (aLife
#' \code{isn_sbi_net}) and qualifying partnership and trust shares
#' (\code{pt_share_sbi_pships}, \code{pt_share_sbi_trusts}). A supplied
#' \code{Tot_net_small_business_inc} is the complete eligible total. For
#' 2\% sample records, gross business receipts approximate turnover and net
#' sole-trader income approximates the eligible amount. General partnership
#' and trust distributions do not identify qualifying shares. Return fields
#' take precedence over separately supplied turnover and income arguments.
#' See \code{system.file("tax-return-fields.txt", package = "grattan")}.
#' @param previous_aggregated_turnover Prior-year aggregated turnover for a
#' continuing business. Either year's turnover can establish eligibility.
#' @param aggregated_turnover A numeric vector the same length as \code{taxable_income}.
#' The current or prior turnover must be below 2 million in 2015-16, or
#' 5 million from 2016-17 onward. With a custom discount and no year supplied,
#' the 5 million threshold is used.
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
#' Original URL was https://www.ato.gov.au/Business/Research-and-development-tax-incentive/Claiming-the-tax-offset/Steps-to-claiming-the-tax-offset/Step-3---Calculate-your-aggregated-turnover/
#' @param total_net_small_business_income Total net business income within the meaning of the Act. For most taxpayers, this is simply any net income from a business they own (or their share of net income from a business in which they have an interest). The only difference being in the calculation of the net business income of some minors (vide Division 6AA of Part III of the Act).
#' @param fy_year The financial year for which the small business tax offset is to apply.
#' @param tax_discount If you do not wish to use the legislated discount rate from a particular \code{fy_year}, 
#' you can specify it via \code{tax_discount}. If both are provided,
#' \code{tax_discount} overrides the rate, with a warning; the year still
#' determines the turnover threshold.
#' @source
#' Basic income tax method s4-10(3) \code{https://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html}.
#' Explanatory memorandum \url{https://github.com/HughParsonage/grattan/blob/master/data-raw/parlinfo/small-biz-explanatory-memo-2015.pdf} from the original \verb{http://parlinfo.aph.gov.au/parlInfo/download/legislation/ems/r5494_ems_0a26ca86-9c3f-4ffa-9b81-219ac09be454/upload_pdf/503041.pdf}.
#' @export


small_business_tax_offset <- function(taxable_income,
                                      basic_income_tax_liability,
                                      .dots.ATO = NULL,
                                      aggregated_turnover = NULL,
                                      total_net_small_business_income = NULL,
                                      fy_year = NULL,
                                      tax_discount = NULL,
                                      previous_aggregated_turnover = NULL) {
  if (is.null(fy_year) && is.null(tax_discount)) stop("`fy_year` and `tax_discount` are both NULL.")
  year <- if (is.null(fy_year)) NULL else fy::fy2yr(fy_year)
  if (length(year) == 1L && year < 2016L && is.null(tax_discount)) return(double(length(taxable_income)))
  if (!is.null(.dots.ATO)) {
    if (!is.null(aggregated_turnover) || !is.null(previous_aggregated_turnover)) {
      warning("Both `.dots.ATO` and `", if (is.null(aggregated_turnover)) "previous_aggregated_turnover" else "aggregated_turnover",
              "` were provided. The return fields take precedence.")
    }
    if (!is.null(total_net_small_business_income)) {
      warning("Both `.dots.ATO` and `total_net_small_business_income` were provided. The return fields take precedence.")
    }
    n <- prohibit_arg_recycling.MAXLENGTH(Filter(length, list(taxable_income, basic_income_tax_liability, year)))
    if (nrow(.dots.ATO) == 1L && n > 1L) {
      rows <- rep.int(1L, n)
      .dots.ATO <- if (is.data.table(.dots.ATO)) .dots.ATO[rows] else .dots.ATO[rows, , drop = FALSE]
    }
    net <- business_income_from_return(.dots.ATO, year %||% 2017L)
    if (is.null(net)) stop("`.dots.ATO` does not contain the necessary variables for small business income.")
  } else {
    limit <- ifelse((year %||% 2017L) == 2016L, 2e6, 5e6)
    prohibit_arg_recycling.MAXLENGTH(Filter(length, list(taxable_income, basic_income_tax_liability,
      total_net_small_business_income, aggregated_turnover, previous_aggregated_turnover, year)))
    eligible <- (aggregated_turnover %||% Inf) < limit |
      (previous_aggregated_turnover %||% Inf) < limit
    net <- (total_net_small_business_income %||% 0) * eligible
  }
  if (!is.null(tax_discount) && !is.null(fy_year)) {
    warning("Both `fy_year` and `tax_discount` were provided. Using the supplied discount.")
  }
  business_offset_amount(taxable_income, basic_income_tax_liability, net, year, tax_discount)
}

business_offset_amount <- function(taxable, basic_tax, net, year, discount) {
  n <- prohibit_arg_recycling.MAXLENGTH(Filter(length, list(taxable, basic_tax, net, year)))
  if (is.null(discount)) {
    discount <- ifelse(year < 2016L, 0, ifelse(year == 2016L, 0.05,
                       ifelse(year <= 2020L, 0.08, ifelse(year == 2021L, 0.13, 0.16))))
  } else {
    if (!is.numeric(discount)) stop("`tax_discount` was class '", storage.mode(discount), "'.")
    if (!(length(discount) %in% c(1L, n))) stop("Provide a single value or a value for every observation.")
  }
  share <- pmin(1, pmax(net, 0) / pmax(taxable, 1))
  pmin(1000, basic_tax * discount * share * (taxable > 0))
}

business_income_from_return <- function(data, year) {
  if (is.null(data)) return(NULL)
  field <- function(...) as.double(return_field(data, ..., default = 0))
  gross_fields <- c("Total_PP_BI_amt", "Total_NPP_BI_amt")
  expense_fields <- c("Total_PP_BE_amt", "Total_NPP_BE_amt")
  net_fields <- c("is_bus_pp", "is_bus_npp", "Net_PP_BI_amt", "Net_NPP_BI_amt")
  reported_fields <- c("Tot_net_small_business_inc", "isn_sbi_net", "pt_share_sbi_pships", "pt_share_sbi_trusts")
  has_gross <- all(gross_fields %in% names(data))
  has_expenses <- all(expense_fields %in% names(data))
  can_test <- has_gross || any(c("small_business_eligible", "aggregated_turnover",
                                  "previous_aggregated_turnover") %in% names(data))
  if (!any(reported_fields %in% names(data)) &&
      !(can_test && (any(net_fields %in% names(data)) || (has_gross && has_expenses)))) return(NULL)
  gross <- if (has_gross) as.double(data$Total_PP_BI_amt) + data$Total_NPP_BI_amt else NA_real_
  turnover <- return_field(data, "aggregated_turnover", default = gross)
  prior <- return_field(data, "previous_aggregated_turnover", default = Inf)
  limit <- ifelse(year == 2016L, 2e6, 5e6)
  eligible <- return_field(data, "small_business_eligible",
                           default = (!is.na(turnover) & turnover < limit) | prior < limit)
  estimated <- if (has_expenses) gross - field("Total_PP_BE_amt") - field("Total_NPP_BE_amt") else 0
  pp <- return_field(data, "is_bus_pp", "Net_PP_BI_amt", default = NA_real_)
  npp <- return_field(data, "is_bus_npp", "Net_NPP_BI_amt", default = NA_real_)
  have_net <- rep_len(!is.na(pp) | !is.na(npp), nrow(data))
  estimated <- ifelse(!have_net, estimated,
                      as.double(ifelse(is.na(pp), 0, pp)) + ifelse(is.na(npp), 0, npp))
  estimated[is.na(estimated)] <- 0
  # Label 15A already identifies eligible sole-trader income. Labels 13D
  # and 13E belong to the distributing entities, each tested separately.
  sole <- return_field(data, "isn_sbi_net", default = ifelse(eligible, pmax(0, estimated), 0))
  total <- pmax(sole, 0) + field("pt_share_sbi_pships") + field("pt_share_sbi_trusts")
  rep_len(return_field(data, "Tot_net_small_business_inc", default = total), nrow(data))
}
