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
  nearest <- round(x)
  whole <- is.finite(x) & abs(x - nearest) <= 2 * .Machine$double.eps * pmax(1, abs(x))
  x[whole] <- nearest[whole]
  as.integer(x)
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
    if (is.null(out)) out <- value else out <- ifelse(is.na(out), value, out)
    if (!anyNA(out)) return(out)
  }
  if (is.null(out)) return(default)
  if (is.null(default)) return(out)
  ifelse(is.na(out), default, out)
}

age_from_file <- function(.dots.ATO, age = NULL) {
  exact <- return_field(.dots.ATO, "c_age_30_june")
  band <- return_field(.dots.ATO, "Age_range", "age_range", "Birth_year")
  if (!is.null(age) && (!is.null(exact) || !is.null(band))) {
    warning("Return age fields take precedence over the age argument where supplied.")
  }
  fallback <- age %||% 42L
  if (!is.null(band)) {
    fallback <- ifelse(is.na(band), fallback, decode_age_range(as.integer(band)))
  }
  if (is.null(exact)) fallback else ifelse(is.na(exact), fallback, exact)
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
  fy.year <- validate_fys_permitted(fy.year %||% fy::date2fy(Sys.Date()))
  yr <- fy2yr(fy.year)
  N <- max(length(taxable), length(yr), nrow(.dots.ATO) %||% 0L, length(age))
  if (!is.null(age) && !(length(age) %in% c(1L, N))) stop("Incompatible age length.")
  if (!(length(taxable) %in% c(1L, N)) || !(length(yr) %in% c(1L, N))) {
    stop("`income` and `fy.year` must have length 1 or the number of records.")
  }
  if (is.data.frame(.dots.ATO)) {
    if (!(nrow(.dots.ATO) %in% c(1L, N))) {
      stop("The number of rows in `.dots.ATO` must be 1 or the number of records.")
    }
    if (nrow(.dots.ATO) == 1L && N > 1L) {
      rows <- rep.int(1L, N)
      .dots.ATO <- if (is.data.table(.dots.ATO)) .dots.ATO[rows] else .dots.ATO[rows, , drop = FALSE]
    }
  }
  zero <- integer(N)
  rN <- function(x) .rN(x, zero, nThread)
  taxable <- rN(taxable)
  age <- rep_len(age_from_file(.dots.ATO, age), N)
  spouse_status <- column("sp_flag", "Partner_status", "Marital_status", default = NULL)
  married <- rN(spouse_status %||% 0L)
  children <- rN(column("c_depend_child"))
  # Reconstruct spouse rebate income from its own components when possible.
  # Medicare and the unused-offset test use the spouse's taxable income.
  spouse_taxable <- rN(column("sp_taxable_income_loss", "Spouse_taxable_income",
                              "Spouse_adjusted_taxable_inc", "spc_rebate_income"))
  spouse_rebate <- column("spc_rebate_income", default = NULL)
  if (is.null(spouse_rebate) || anyNA(spouse_rebate)) {
    spouse_components <- .Call("Crebate_income", spouse_taxable, column("sp_rept_super_cont"),
      NULL, column("sp_net_invest_loss"), NULL, column("sp_rept_fringe_benefit"),
      column("sp_rept_fringe_benefit_exe", default = NULL),
      column("sp_rept_fringe_benefit_nexe", default = NULL), column("spouse_assessable_fhss"),
      yr, nThread, PACKAGE = "grattan")
    spouse_rebate <- column("spc_rebate_income", default = spouse_components)
  }
  spouse_rebate <- rN(spouse_rebate)
  unknown_status <- if (is.null(spouse_status)) TRUE else is.na(spouse_status)
  married <- as.integer(married != 0L | (unknown_status & spouse_taxable > 0L))
  claim <- return_claim_codes(.dots.ATO, age, married, yr, System, sapto_eligible)
  # A supplied year-end status takes precedence. Where absent, a partnered
  # claim also identifies couples whose spouse reported no taxable income.
  married <- as.integer(married != 0L | (unknown_status & as.integer(claim) %in% 2:5))
  reported <- column("ic_rebate_income", default = NULL)
  if (is.null(reported) || anyNA(reported)) {
    rent <- column("it_property_loss", default = NULL)
    net_rent <- column("is_net_rent", "Net_rent_amt")
    if (!is.null(rent)) net_rent <- ifelse(is.na(rent), net_rent, -pmax(rent, 0))
    rebate <- .Call("Crebate_income", taxable,
                    column("it_rept_empl_super_cont", "Rptbl_Empr_spr_cont_amt"),
                    column("ds_pers_super_cont", "Non_emp_spr_amt"),
                    column("it_invest_loss", "Net_fincl_invstmt_lss_amt",
                           "Net_fincl_invstmnt_lss_amt"),
                    net_rent,
                    column("it_rept_fringe_benefit", "Rep_frng_ben_amt"),
                    column("it_rept_fringe_benefit_exe", default = NULL),
                    column("it_rept_fringe_benefit_nexe", default = NULL),
                    column("is_other_cat3_fhss"),
                    yr, nThread, PACKAGE = "grattan")
    if (!is.null(reported)) rebate <- rN(ifelse(is.na(reported), rebate, reported))
  } else {
    rebate <- rN(reported)
  }
  historical <- yr <= 2009L
  if (any(historical)) {
    if (all(historical)) {
      rebate <- pmax(0L, taxable)
      spouse_rebate <- pmax(0L, spouse_taxable)
    } else {
      rebate[historical] <- pmax(0L, taxable[historical])
      spouse_rebate[historical] <- pmax(0L, spouse_taxable[historical])
    }
  }
  # Reg 12(5) includes specified exempt pensions in the transfer tax test.
  # Keep both Medicare incomes and the separate combined rebate test intact.
  transfer_taxable <- function(income, pension) {
    if (is.null(pension)) return(income)
    rN(as.double(income) + (yr >= 2013L) * ifelse(is.na(pension), 0, pension))
  }
  own_transfer <- transfer_taxable(taxable, column("sapto_exempt_pension", default = NULL))
  spouse_transfer <- transfer_taxable(spouse_taxable, column("sp_pension_exempt", default = NULL))
  franking <- rN(column("i_frank_cr"))
  flood_exempt <- rN(column("tn_flood_levy_exempt") %in% c(1L, TRUE, "X", "Y"))
  if (!is.null(sbto_discount) && !(length(sbto_discount) %in% c(1L, N))) {
    stop("Supply one business discount or a discount per taxpayer.")
  }
  business <- if (any(yr >= 2016L)) business_income_from_return(.dots.ATO, yr)
  labour <- if (any(yr >= 2028L)) net_labour_from_return(.dots.ATO)
  assess <- function(rows, year, summarise = 0L) {
    income_offsets <- NULL
    if (!is.null(business) && year >= 2016L) {
      basic_tax <- .Call("Cordinary_tax", year, taxable[rows], System,
                         PACKAGE = "grattan")
      discount <- if (length(sbto_discount) > 1L) sbto_discount[rows] else sbto_discount
      income_offsets <- business_offset_amount(taxable[rows], basic_tax, business[rows],
                                               year, discount)
    }
    if (!is.null(labour) && year >= 2028L) {
      labour_tax <- .Call("Cordinary_tax", year, rN(labour)[rows], System, PACKAGE = "grattan")
      income_offsets <- (income_offsets %||% 0) + pmin(250, labour_tax)
    }
    .Call("Cincome_tax", year, taxable[rows], rebate[rows], as.integer(age[rows]),
          married[rows], children[rows], spouse_rebate[rows], spouse_taxable[rows],
          claim[rows], franking[rows], income_offsets, flood_exempt[rows],
          own_transfer[rows], spouse_transfer[rows], System,
          nThread, as.integer(summarise), PACKAGE = "grattan")
  }
  if (length(yr) == 1L || !isntConstant(yr)) {
    return(assess(seq_len(N), yr[1L], summary))
  }
  tax <- numeric(N)
  for (year in unique(yr)) {
    rows <- which(yr == year)
    tax[rows] <- assess(rows, year)
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




