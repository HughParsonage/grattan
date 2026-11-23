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
#' When calculating from separate arguments, supply current or prior turnover.
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


small_business_tax_offset <- function(taxable_income, basic_income_tax_liability, .dots.ATO = NULL, aggregated_turnover = NULL, total_net_small_business_income = NULL, fy_year = NULL, tax_discount = NULL, previous_aggregated_turnover = NULL) {
  if (is.null(fy_year) && is.null(tax_discount)) {
    stop("`fy_year` and `tax_discount` are both NULL.")
  }
  year <-
    if (is.null(fy_year)) {
      NULL
    } else {
      fy::fy2yr(fy_year)
    }

  if (is.null(tax_discount) &&
      length(year) &&
      isTRUE(year[1L] < 2016L) &&
      (length(year) == 1L || isTRUE(max(year) < 2016L))) {
    if (!length(taxable_income)) {
      return(double())
    }

    inputs <- list(taxable_income, basic_income_tax_liability, year)
    if (is.null(.dots.ATO)) {
      inputs <- c(inputs, list(aggregated_turnover, previous_aggregated_turnover, total_net_small_business_income))
    } else {
      inputs <- c(inputs, list(seq_len(nrow(.dots.ATO))))
    }

    return(double(prohibit_arg_recycling.MAXLENGTH(Filter(length, inputs))))
  }

  if (!is.null(.dots.ATO)) {
    if (!is.null(aggregated_turnover) || !is.null(previous_aggregated_turnover)) {
      turnover_argument <-
        if (is.null(aggregated_turnover)) {
          "previous_aggregated_turnover"
        } else {
          "aggregated_turnover"
        }

      warning("Both `.dots.ATO` and `", turnover_argument, "` were provided. The return fields take precedence.")
    }
    if (!is.null(total_net_small_business_income)) {
      warning("Both `.dots.ATO` and `total_net_small_business_income` were provided. The return fields take precedence.")
    }
    prohibit_arg_recycling.MAXLENGTH(Filter(length, list(taxable_income, basic_income_tax_liability, year)))
    net <- business_income_from_return(.dots.ATO, year %||% 2017L)
    if (is.null(net)) {
      stop("`.dots.ATO` does not contain the necessary variables for small business income.")
    }
  } else {
    if (is.null(aggregated_turnover) && is.null(previous_aggregated_turnover)) {
      stop("Supply `aggregated_turnover` or `previous_aggregated_turnover` when calculating from separate arguments.")
    }
    limit <- fifelse((year %||% 2017L) == 2016L, 2e6, 5e6)
    prohibit_arg_recycling.MAXLENGTH(Filter(length, list(taxable_income, basic_income_tax_liability,
      total_net_small_business_income, aggregated_turnover, previous_aggregated_turnover, year)))
    eligible <- (aggregated_turnover %||% previous_aggregated_turnover) < limit
    if (!is.null(aggregated_turnover) && !is.null(previous_aggregated_turnover)) {
      eligible <- eligible | previous_aggregated_turnover < limit
    }
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
    # Ending years 2015 and 2022 also represent all earlier and later years.
    rates <- c(0, 0.05, 0.08, 0.08, 0.08, 0.08, 0.13, 0.16)
    discount <- rates[hutilscpp::squish(year, 2015L, 2022L) - 2014L]
  } else {
    if (!is.numeric(discount)) {
      stop("`tax_discount` was class '", storage.mode(discount), "'.")
    }
    if (!(length(discount) %in% c(1L, n))) {
      stop("Provide a single value or a value for every observation.")
    }
  }
  # Cap fresh arithmetic results in place. Caller-owned inputs can be shared.
  if (is.logical(net)) {
    net <- as.integer(net)
  }
  if (is.logical(taxable)) {
    taxable <- as.integer(taxable)
  }
  share <- pmax0(net, keep_nas = is.integer(net)) / pmaxC(taxable, 1L, keep_nas = is.integer(taxable))
  share <- pminC(share, 1, in_place = TRUE, keep_nas = TRUE)
  amount <- basic_tax * discount * share * (taxable > 0)
  pminC(amount, 1000, in_place = TRUE, keep_nas = TRUE)
}

business_income_from_return <- function(data, year) {
  if (is.null(data)) {
    return(NULL)
  }
  columns <- names(data)
  has_gross <- all(c("Total_PP_BI_amt", "Total_NPP_BI_amt") %chin% columns)
  has_expenses <- all(c("Total_PP_BE_amt", "Total_NPP_BE_amt") %chin% columns)
  net_fields <- c("is_bus_pp", "is_bus_npp", "Net_PP_BI_amt", "Net_NPP_BI_amt")
  reported_fields <- c("Tot_net_small_business_inc", "isn_sbi_net", "pt_share_sbi_pships", "pt_share_sbi_trusts")
  can_test <- has_gross || any(c("small_business_eligible", "aggregated_turnover",
                                  "previous_aggregated_turnover") %chin% columns)
  if (!any(reported_fields %chin% columns) &&
      !(can_test && (any(net_fields %chin% columns) || (has_gross && has_expenses)))) {
    return(NULL)
  }

  # Only construct a proxy for missing label 15A amounts. gross is shared
  # lazily between the turnover test and the receipts-minus-expenses fallback.
  estimate_sole <- function(gross) {
    if (!can_test) {
      return(0)
    }
    eligible <- return_field(data, "small_business_eligible", default = {
      limit <- fifelse(year == 2016L, 2e6, 5e6)
      turnover <- return_field(data, "aggregated_turnover", default = gross)
      qualifies <- turnover < limit
      if (anyNA(qualifies)) {
        qualifies <- coalesce0(qualifies)
      }
      if ("previous_aggregated_turnover" %chin% columns) {
        qualifies <- qualifies | return_field(data, "previous_aggregated_turnover", default = Inf) < limit
      }
      qualifies
    })
    pp <- return_field(data, "is_bus_pp", "Net_PP_BI_amt")
    npp <- return_field(data, "is_bus_npp", "Net_NPP_BI_amt")
    if (is.null(pp) || is.null(npp)) {
      net <- pp %||% npp
    } else {
      # Sum in double precision even when both return columns are integers.
      net <- as.double(pp) + npp
      if (anyNA(net)) {
        # Use the other component when one is missing. Two missing items
        # still use gross less expenses. An indeterminate sum contributes zero.
        net <- fifelse(is.na(pp), as.double(npp),
                       fifelse(is.na(npp), as.double(pp), coalesce0(net)))
      }
    }
    if (is.null(net) || anyNA(net)) {
      from_gross <-
        if (has_gross && has_expenses) {
          gross - return_field(data, "Total_PP_BE_amt", default = 0) -
            return_field(data, "Total_NPP_BE_amt", default = 0)
        } else {
          0
        }

      net <-
        if (is.null(net)) {
          from_gross
        } else {
          fcoalesce(as.double(net), from_gross)
        }

      if (anyNA(net)) {
        net <- coalesce0(net)
      }
    }
    eligible <- as.logical(eligible)
    if (length(eligible) == 1L && !is.na(eligible)) {
      if (eligible) {
        net
      } else {
        0
      }
    } else {
      fifelse(eligible, net, 0)
    }
  }
  # The complete reported total supersedes all three components. Label 15A
  # already qualifies; the partnership and trust amounts have their own tests.
  total <- return_field(data, "Tot_net_small_business_inc", default = {
    sole <-
      return_field(data,
                   "isn_sbi_net",
                   default = estimate_sole(if (has_gross) {
                     as.double(data$Total_PP_BI_amt) + data$Total_NPP_BI_amt
                   } else {
                     NA_real_
                   }))

    amount <- pmax0(as.double(sole))
    for (share in c("pt_share_sbi_pships", "pt_share_sbi_trusts")) {
      if (share %chin% columns) {
        amount <- amount + return_field(data, share, default = 0)
      }
    }
    amount
  })
  if (length(total) == 1L && nrow(data) > 1L) {
    rep_len(total, nrow(data))
  } else {
    total
  }
}
