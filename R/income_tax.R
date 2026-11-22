#' Income tax payable
#' 
#' @name income_tax
#' @param income The individual's taxable income.
#' @param fy.year The financial year in which the income was earned. Native schedules
#' are available from 1983-84 through 2029-30. For future years, enacted rates
#' are used and annually indexed amounts remain at their latest enacted values.
#' If \code{fy.year} is not given, the current financial year is used by default.
#' @param age Age at 30 June, including fractions of a year when known.
#' A return age takes precedence where present.
#' @param return.mode Return the numeric or integer assessment vector, or its
#' \code{"sum"} or \code{"mean"}.
#' @param .dots.ATO A data frame with ATO 2\% sample or aLife individual-return
#' fields. Supply one record per person and assessment year. The installed guide
#' \code{system.file("tax-return-fields.txt", package = "grattan")} lists the
#' source fields, years of coverage, assumptions and optional inputs.
#' \code{sp_taxable_income_loss} supplies spouse taxable income for Medicare
#' and SAPTO transfers. \code{spc_rebate_income} supplies spouse rebate income
#' for the SAPTO income test. Use a reported senior claim code where available.
#' When the 2\% file supplies only age bands, family and age eligibility are
#' approximate. \code{ic_rebate_income} takes precedence over reconstructed
#' rebate income. For working Australians tax offset projections from 2027-28,
#' \code{net_labour_income} supplies the complete statutory amount if known.
#'
#' @param System A \code{tax-system} created by \code{System()} or \code{NULL}, the default,
#' corresponding to the tax system of the given year.
#' 
#' @param nThread Number of threads to use.
#' 
#' @author Tim Cameron, Brendan Coates, Matthew Katzen, Hugh Parsonage, William Young
#' @return The total personal income tax payable.
#' @details The function is inflexible by design.
#' It is designed to return the correct tax payable in a year, not to model the tax payable 
#' under different tax settings. (Use \code{\link{model_income_tax}} for that purpose.)
#' 
#' 
#' The function aims to produce the personal income tax payable for the inputs given
#' in the tax year \code{fy.year}. The function is specified to produce the most accurate 
#' calculation of personal income tax given the variables in the ATO's 2\% sample files.
#'  However, many components are absent from these files, while other components could
#'  not be computed reliably.
#'  
#' For the 2018-19 tax year, the function calculates
#' \describe{
#' \item{tax on ordinary taxable income}{The tax as specified in Schedule 7 of the 
#' \emph{Income Tax Rates Act 1986} (Cth).}
#' \item{Medicare levy}{See \code{\link{medicare_levy}} for details.}
#' \item{LITO}{See \code{\link{lito}} for details.}
#' \item{SAPTO}{See \code{\link{sapto}}. Earlier years use the implemented senior
#' Australian offset schedule. Without an explicit claim code, eligibility is
#' inferred from the available age and family-status variables.}
#' \item{SBTO}{See \code{\link{small_business_tax_offset}} for details.}
#' \item{Historical levies}{The flood levy and the temporary budget repair levy.}
#' }
#' 
#' Missing spouse income and dependant counts can affect Medicare, even at
#' the same taxable income. The published 2\% indexes for 2011-12 through
#' 2023-24 omit the dependant count. aLife supplies \code{c_depend_child}.
#' From 2027-28 the working Australians tax offset uses identified net labour
#' income. Supply taxable income after the deductions applying in that year.
#'
#' @examples 
#' 
#' ## Income tax payable on a taxable income of 50,000 
#' ## for the 2013-14 tax year
#' income_tax(50e3, "2013-14")
#' 
#' ## Calculate tax for each lodger in the 2013-14 sample file.
#' 
#'  # library(data.table)
#'  # library(taxstats)
#'   
#'  # s1314 <- as.data.table(sample_file_1314)
#'  # s1314[, tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = s1314)]
#' 
#' @export income_tax

income_tax <- function(income,
                       fy.year = NULL,
                       age = NULL,
                       .dots.ATO = NULL,
                       System = NULL,
                       return.mode = c("numeric", "integer", "sum", "mean"),
                       nThread = getOption("grattan.nThread", 1L)) {
  ans <- income_tax2(income, 
                     fy.year = fy.year,
                     .dots.ATO = .dots.ATO,
                     System = System, 
                     nThread = nThread,
                     age = age,
                     summary = identical(return.mode, "sum") + 2L * identical(return.mode, "mean"))
  if (match.arg(return.mode) == "integer") {
    ans <- whole_dollar_tax(ans)
  }
  ans
}

# Preserve truncation of genuine fractions when arithmetic lands one or two
# floating-point units below an exact dollar.
whole_dollar_tax <- function(x) {
  .Call("Cwhole_dollar_tax", x, PACKAGE = "grattan")
}

get_column_from <- function(DT, nom, ..., NULL_OK = FALSE) {
  if (is.character(substitute(nom))) {
    if (!hasName(DT, nom)) {
      if (missing(..1)) {
        if (NULL_OK) {
          return(NULL)
        } else {
          stop("DT lacked name '", nom, "'.")
        }
      }
      return(get_column_from(DT, ...))
    }
    return(.subset2(DT, nom))
  }
  if (all(hasName(DT, all.vars(substitute(nom))))) {
    return(eval(substitute(nom), envir = DT))
  }
  get_column_from(DT, ...)
}

# Accept aLife and published sample spellings, including when different
# releases have been stacked. A reported zero takes precedence over a proxy.
return_field <- function(data, ..., default = NULL) {
  out <- NULL
  for (name in c(...)) {
    value <- data[[name]]
    if (is.null(value)) next
    if (is.null(out)) {
      out <- value
    } else {
      if (!anyNA(out)) return(out)
      out <- fill_return_field(out, value)
    }
  }
  if (is.null(out)) return(default)
  if (missing(default) || !anyNA(out) || is.null(default)) return(out)
  fill_return_field(out, default)
}

fill_return_field <- function(x, fallback) {
  if (length(x) == 1L && is.na(x)) return(fallback)
  if (!is.object(x) && !is.object(fallback)) {
    if (typeof(x) == typeof(fallback)) return(fcoalesce(x, fallback))
    if ((is.numeric(x) || is.logical(x)) && (is.numeric(fallback) || is.logical(fallback))) {
      if (is.double(x) || is.double(fallback)) return(fcoalesce(as.double(x), as.double(fallback)))
      return(fcoalesce(as.integer(x), as.integer(fallback)))
    }
  }
  # Keep the coercions of the historical interface for classed/mixed fields.
  ifelse(is.na(x), fallback, x)
}

age_from_file <- function(.dots.ATO, age = NULL) {
  if (!is.null(age) && any(c("c_age_30_june", "Age_range", "age_range", "Birth_year") %chin% names(.dots.ATO))) {
    warning("Return age fields take precedence over the age argument where supplied.")
  }
  return_field(.dots.ATO, "c_age_30_june", default = {
    band <- return_field(.dots.ATO, "Age_range", "age_range", "Birth_year")
    if (is.null(band) || (length(band) == 1L && is.na(band))) {
      age %||% 42L
    } else {
      decoded <- decode_age_range(as.integer(band))
      if (anyNA(band)) {
        missing_band <- is.na(band)
        fallback <- age %||% 42L
        decoded[missing_band] <- if (length(fallback) == 1L) fallback else fallback[missing_band]
      }
      decoded
    }
  })
}




income_tax2 <- function(income = NULL,
                        fy.year = NULL,
                        .dots.ATO = NULL,
                        System = NULL,
                        nThread = getOption("grattan.nThread", 1L),
                        summary = 0L, age = NULL, sbto_discount = NULL,
                        sapto_eligible = NULL) {
  summary <- as.integer(summary)
  if (!is.null(.dots.ATO) && !is.data.frame(.dots.ATO)) {
    stop("`.dots.ATO` must be a data.frame or data.table.")
  }
  column <- function(..., default = 0L) return_field(.dots.ATO, ..., default = default)
  taxable <- income %||% column("ic_taxable_income_loss", "Taxable_Income", default = NULL)
  if (is.null(taxable)) {
    stop("Unable to determine taxable income: .dots.ATO lacked names: 'Taxable_Income' or 'ic_taxable_income_loss'.")
  }
  if (!length(taxable)) return(if (summary == 2L) NaN else if (summary == 1L) 0 else numeric())
  fy.year <- fy.year %||% fy::date2fy(Sys.Date())
  n_years <- length(fy.year)
  N <- max(length(taxable), n_years, nrow(.dots.ATO) %||% 0L, length(age))
  if (!is.null(age) && !(length(age) %in% c(1L, N))) stop("Incompatible age length.")
  if (!(length(taxable) %in% c(1L, N)) || !(n_years %in% c(1L, N))) {
    stop("`income` and `fy.year` must have length 1 or the number of records.")
  }
  if (!is.null(.dots.ATO) && !(nrow(.dots.ATO) %in% c(1L, N))) {
    stop("The number of rows in `.dots.ATO` must be 1 or the number of records.")
  }
  if (n_years > 1L && is.character(fy.year) && !isntConstant(fy.year)) fy.year <- fy.year[1L]
  yr <- fy2yr(fy.year)
  # Keep each row index once. Annual returns need no indices or column copies.
  groups <- if (length(yr) > 1L) setDT(list(fy = yr))[, list(rows = list(.I)), by = "fy"]
  years <- if (is.null(groups)) yr else groups$fy
  min_year <- min(years)
  max_year <- max(years)

  zero <- integer(N)
  rN <- function(x) .rN(x, zero, nThread)
  taxable <- rN(taxable)
  age <- age_from_file(.dots.ATO, age)
  spouse_status <- return_field(.dots.ATO, "sp_flag", "Partner_status", "Marital_status")
  married <- rN(spouse_status %||% 0L)
  children <- rN(column("c_depend_child"))
  spouse_amount <- return_field(.dots.ATO, "sp_taxable_income_loss", "Spouse_taxable_income",
                                "Spouse_adjusted_taxable_inc", "spc_rebate_income")
  spouse_taxable <- rN(spouse_amount %||% 0L)
  infer_status <- is.null(spouse_status) || anyNA(spouse_status)
  if (infer_status) {
    unknown_status <- if (is.null(spouse_status)) TRUE else is.na(spouse_status)
    if (!is.null(spouse_amount)) married[unknown_status & spouse_taxable > 0L] <- 1L
  }
  claim <- return_claim_codes(.dots.ATO, age, married, yr, System, sapto_eligible)
  # A reported partnered claim also identifies couples with zero spouse income.
  if (infer_status && any(c("on_sapto_cd", "on_sato_cd") %chin% names(.dots.ATO))) {
    married[unknown_status & claim > as.raw(1L)] <- 1L
  }

  if (max_year <= 2009L || is.null(.dots.ATO)) {
    rebate <- pmax0(taxable, nThread = nThread)
    spouse_rebate <- if (is.null(spouse_amount)) zero else pmax0(spouse_taxable, nThread = nThread)
  } else {
    # Reported complete amounts bypass reconstruction. NULL addbacks are zero
    # in the native calculation, including for records from stacked releases.
    spouse_rebate <- column("spc_rebate_income", default = {
      spouse_fields <- c("sp_rept_super_cont", "sp_net_invest_loss", "sp_rept_fringe_benefit",
                         "sp_rept_fringe_benefit_exe", "sp_rept_fringe_benefit_nexe", "spouse_assessable_fhss")
      if (any(spouse_fields %chin% names(.dots.ATO))) {
        .Call("Crebate_income", spouse_taxable, .dots.ATO[["sp_rept_super_cont"]], NULL,
              .dots.ATO[["sp_net_invest_loss"]], NULL, .dots.ATO[["sp_rept_fringe_benefit"]],
              .dots.ATO[["sp_rept_fringe_benefit_exe"]], .dots.ATO[["sp_rept_fringe_benefit_nexe"]],
              .dots.ATO[["spouse_assessable_fhss"]], yr, nThread, PACKAGE = "grattan")
      } else if (is.null(spouse_amount)) zero else pmax0(spouse_taxable, nThread = nThread)
    })
    rebate <- column("ic_rebate_income", default = {
      loss <- .dots.ATO[["it_property_loss"]]
      rent <- if (is.null(loss)) {
        return_field(.dots.ATO, "is_net_rent", "Net_rent_amt")
      } else {
        signed <- -pmax0(loss, keep_nas = TRUE, nThread = nThread)
        if (anyNA(signed)) fill_return_field(signed, column("is_net_rent", "Net_rent_amt")) else signed
      }
      .Call("Crebate_income", taxable,
            return_field(.dots.ATO, "it_rept_empl_super_cont", "Rptbl_Empr_spr_cont_amt"),
            return_field(.dots.ATO, "ds_pers_super_cont", "Non_emp_spr_amt"),
            return_field(.dots.ATO, "it_invest_loss", "Net_fincl_invstmt_lss_amt", "Net_fincl_invstmnt_lss_amt"),
            rent, return_field(.dots.ATO, "it_rept_fringe_benefit", "Rep_frng_ben_amt"),
            .dots.ATO[["it_rept_fringe_benefit_exe"]], .dots.ATO[["it_rept_fringe_benefit_nexe"]],
            .dots.ATO[["is_other_cat3_fhss"]], yr, nThread, PACKAGE = "grattan")
    })
    rebate <- rN(rebate)
    spouse_rebate <- rN(spouse_rebate)
    if (min_year <= 2009L) {
      historical <- yr <= 2009L
      rebate[historical] <- pmax0(taxable[historical], nThread = nThread)
      spouse_rebate[historical] <- pmax0(spouse_taxable[historical], nThread = nThread)
    }
  }
  # Reg 12(5) includes specified exempt pensions in the transfer tax test.
  transfer_taxable <- function(income, pension) {
    if (max_year < 2013L || is.null(pension)) return(income)
    if (anyNA(pension)) pension <- coalesce0(pension, nThread = nThread)
    if (min_year < 2013L) pension <- (yr >= 2013L) * pension
    rN(as.double(income) + pension)
  }
  own_transfer <- transfer_taxable(taxable, .dots.ATO[["sapto_exempt_pension"]])
  spouse_transfer <- transfer_taxable(spouse_taxable, .dots.ATO[["sp_pension_exempt"]])
  franking <- .dots.ATO[["i_frank_cr"]]
  if (!is.null(franking)) franking <- rN(franking)
  flood_exempt <- .dots.ATO[["tn_flood_levy_exempt"]]
  if (!is.null(flood_exempt)) {
    flood_exempt <- if (is.character(flood_exempt) || is.factor(flood_exempt)) {
      as.character(flood_exempt) %chin% c("1", "TRUE", "X", "Y")
    } else flood_exempt == 1L
    flood_exempt <- rN(flood_exempt)
  }
  if (!is.null(sbto_discount) && !(length(sbto_discount) %in% c(1L, N))) {
    stop("Supply one business discount or a discount per taxpayer.")
  }
  business <- if (max_year >= 2016L) business_income_from_return(.dots.ATO, yr)
  labour <- if (max_year >= 2028L) net_labour_from_return(.dots.ATO)
  if (!is.null(labour)) labour <- .rN(labour, if (length(labour) == 1L) 0L else zero, nThread)
  assess <- function(year, rows = NULL, summarise = 0L) {
    take <- if (is.null(rows)) identity else function(x) if (length(x) > 1L) x[rows] else x
    income <- take(taxable)
    income_offsets <- NULL
    if (year >= 2016L && !is.null(business)) {
      basic_tax <- .Call("Cordinary_tax", year, income, System, PACKAGE = "grattan")
      income_offsets <- business_offset_amount(income, basic_tax, take(business), year, take(sbto_discount))
    }
    if (year >= 2028L && !is.null(labour)) {
      labour_tax <- .Call("Cordinary_tax", year, take(labour), System, PACKAGE = "grattan")
      income_offsets <- (income_offsets %||% 0) + pminC(labour_tax, 250, nThread = nThread)
    }
    .Call("Cincome_tax", year, income, take(rebate), as.integer(take(age)), take(married),
          take(children), take(spouse_rebate), take(spouse_taxable), take(claim), take(franking),
          income_offsets, take(flood_exempt), take(own_transfer), take(spouse_transfer), System,
          nThread, as.integer(summarise), PACKAGE = "grattan")
  }
  if (length(years) == 1L) return(assess(years, summarise = summary))
  tax <- numeric(N)
  for (g in seq_along(years)) {
    rows <- groups$rows[[g]]
    tax[rows] <- assess(years[g], rows)
  }
  if (summary == 1L) sum(tax) else if (summary == 2L) mean(tax) else tax
}





income_tax222 <- function(alife) {
  income <- .subset2(alife, "ic_taxable_income_loss")
  rebateIncome <- .subset2(alife, "ic_rebate_income")
  zero <- integer(length(income))
  .Call("Cincome2022", income, 
        .subset2(alife, "spc_rebate_income"),
        rebateIncome, 
        .subset2(alife, "c_age_30_june"), 
        .subset2(alife, "sp_flag"), 
        .subset2(alife, "c_depend_child"), 1L, PACKAGE = "grattan")
}




