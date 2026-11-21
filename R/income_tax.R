#' Income tax payable
#' 
#' @name income_tax
#' @param income The individual's taxable income.
#' @param fy.year The financial year in which the income was earned. Native schedules
#' are available from 1983-84 through 2029-30. For future years, enacted rates
#' are used and annually indexed amounts remain at their latest enacted values.
#' If \code{fy.year} is not given, the current financial year is used by default.
#' @param age The individual's age. Ignored if \code{.dots.ATO} is provided (and contains
#' an age variable such as \code{age_range} or \code{Birth_year}).
#' @param return.mode Return the numeric or integer assessment vector, or its
#' \code{"sum"} or \code{"mean"}.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files.
#' 
#' Age variables in \code{.dots.ATO} take precedence over \code{age} and providing both
#' is a warning.
#' Supply \code{sp_taxable_income_loss} separately from \code{spc_rebate_income}
#' when both are available: Medicare uses the former and SAPTO the latter.
#' An explicit \code{on_sapto_cd} supplies actual SAPTO eligibility; otherwise
#' eligibility is inferred from age and family status. Supply \code{ic_rebate_income}
#' when the aggregate fringe-benefit field cannot identify the exempt-employer
#' amount or other rebate-income adjustments are required.
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
#' Notably, when used with a 2\% sample file, the function will not be able to correctly account
#' for different tax rates and offsets among taxpayers with dependants since the sample files
#' (as of 2015-16) do not have this information.
#' 
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
  if (is.null(.dots.ATO)) {
    .dots.ATO <- data.table(ic_taxable_income_loss = income, 
                            sp_flag = FALSE,
                            c_age_30_june = as.integer(age %||% 42L))
  }
  ans <- income_tax2(income, 
                     fy.year = fy.year,
                     .dots.ATO = .dots.ATO,
                     System = System, 
                     nThread = nThread,
                     summary = identical(return.mode, "sum") + 2L * identical(return.mode, "mean"))
  if (match.arg(return.mode) == "integer") {
    ans <- as.integer(ans)
  }
  ans
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

age_from_file <- function(.dots.ATO, age = NULL) {
  if (is.null(.dots.ATO)) {
    return(age %||% 42L)
  }
  if (!is.null(age)) {
    warning("`age` is not NULL but `.dots.ATO` is supplied with an age variable, so age will be ignored.")
  }
  if (hasName(.dots.ATO, "c_age_30_june")) {
    return(.subset2(.dots.ATO, "c_age_30_june"))
  }
  if (!is.null(v1 <- .subset2(.dots.ATO, "age_range")) &  # single amp so v2 occurs
      !is.null(v2 <- .subset2(.dots.ATO, "Birth_year"))) {
    return(decode_age_range(coalesce(v1, v2, 6L))) # 6 decoded to 42
  }
  if (!is.null(v1)) {
    return(decode_age_range(v1))
  }
  if (!is.null(v2)) {
    return(decode_age_range(v2))
  } 
  age %||% 42L
}




income_tax2 <- function(income = NULL,
                        fy.year = NULL,
                        .dots.ATO = NULL,
                        System = NULL,
                        nThread = getOption("grattan.nThread", 1L),
                        summary = 0L) {
  summary <- as.integer(summary)
  if (!is.null(.dots.ATO) && !is.data.frame(.dots.ATO)) {
    stop("`.dots.ATO` must be a data.frame or data.table.")
  }
  column <- function(..., default = 0L) {
    for (name in c(...)) {
      value <- .subset2(.dots.ATO, name)
      if (!is.null(value)) return(value)
    }
    default
  }
  taxable <- income %||% column("ic_taxable_income_loss", "Taxable_Income", default = NULL)
  if (is.null(taxable)) {
    stop("Unable to determine taxable income: .dots.ATO lacked names: 'Taxable_Income' or 'ic_taxable_income_loss'.")
  }
  if (!length(taxable)) return(if (summary == 2L) NaN else if (summary == 1L) 0 else numeric())
  fy.year <- validate_fys_permitted(fy.year %||% fy::date2fy(Sys.Date()))
  yr <- fy2yr(fy.year)
  N <- max(length(taxable), length(yr), nrow(.dots.ATO) %||% 0L)
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
  age <- rN(age_from_file(.dots.ATO))
  married <- rN(column("sp_flag", "Partner_status", "Marital_status"))
  children <- rN(column("c_depend_child"))
  spouse_rebate <- rN(column("spc_rebate_income", "Spouse_adjusted_taxable_inc",
                             "sp_taxable_income_loss"))
  # Medicare uses taxable income, while SAPTO uses rebate income. With only
  # the older adjusted-income column available, retain that approximation.
  spouse_taxable <- rN(column("sp_taxable_income_loss", "Spouse_taxable_income",
                              default = spouse_rebate))
  claim <- column("on_sapto_cd", default = NULL)
  claim <- if (is.null(claim)) {
    claim_data <- .dots.ATO %||% data.table(c_age_30_june = age, sp_flag = married)
    sf2osc(claim_data, yr = yr, System = System)
  } else {
    as_raw_sapto_cd(claim)
  }
  if (length(claim) != 1L && length(claim) != N) {
    stop("`on_sapto_cd` must have length 1 or the number of incomes.")
  }
  claim <- rep_len(claim, N)
  rebate <- column("ic_rebate_income", default = NULL)
  if (is.null(rebate)) {
    rebate <- .Call("Crebate_income", taxable,
                    column("it_rept_empl_super_cont", "Rptbl_Empr_spr_cont_amt"),
                    NULL, # Total employer contributions include compulsory SG.
                    column("ds_pers_super_cont", "Non_emp_spr_amt"),
                    column("it_invest_loss", "Net_fincl_invstmt_lss_amt",
                           "Net_fincl_invstmnt_lss_amt"),
                    column("is_net_rent", "Net_rent_amt"),
                    column("it_rept_fringe_benefit", "Rep_frng_ben_amt"),
                    yr, nThread, PACKAGE = "grattan")
  } else {
    rebate <- rN(rebate)
  }
  franking <- rN(column("i_frank_cr"))
  business_fields <- c("Total_PP_BE_amt", "Total_PP_BI_amt",
                       "Total_NPP_BE_amt", "Total_NPP_BI_amt")
  has_business <- all(business_fields %in% names(.dots.ATO))
  assess <- function(rows, year, summarise = 0L) {
    business_offset <- NULL
    if (has_business && year >= 2016L) {
      basic_tax <- .Call("Cordinary_tax", year, taxable[rows], System,
                         PACKAGE = "grattan")
      returns <- if (is.data.table(.dots.ATO)) .dots.ATO[rows] else .dots.ATO[rows, , drop = FALSE]
      business_offset <- small_business_tax_offset(
        taxable[rows], basic_tax, .dots.ATO = returns, fy_year = yr2fy(year))
    }
    .Call("Cincome_tax", year, taxable[rows], rebate[rows], age[rows],
          married[rows], children[rows], spouse_rebate[rows], spouse_taxable[rows],
          claim[rows], franking[rows], business_offset, System,
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




