#' Income tax payable
#' 
#' @name income_tax
#' @param income The individual assessable income.
#' @param fy.year The financial year in which the income was earned. Tax years 2000-01 to 2018-19 are supported, as well as the tax year 2019-20, for convenience. 
#' If \code{fy.year} is not given, the current financial year is used by default.
#' @param age The individual's age. Ignored if \code{.dots.ATO} is provided (and contains
#' an age variable such as \code{age_range} or \code{Birth_year}).
#' @param return.mode The mode (numeric or integer) of the returned vector.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files.
#' 
#' Age variables in \code{.dots.ATO} take precedence over \code{age} and providing both
#' is a warning.
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
#' \item{SAPTO}{See \code{\link{sapto}}. For years preceding the introduction of SAPTO, 
#' the maximum offset is assumed to apply to those above age 65 (since the sample files only provide 5-year 
#' age groups).}
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
  has_nom <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && hasName(.dots.ATO, x)
  }
  
  s2 <- function(x, ..., default = 0L, .r = FALSE) {
    if (is.null(.dots.ATO)) {
      return(default)
    }
    # Takes column names returning the first (without evaluting the others)
    # then uses the defaults
    if (has_nom(x)) {
      return(.subset2(.dots.ATO, x))
    }
    if (missing(..1)) {
      return(eval(default))
    }
    if (.r) {
      # necessary to avoid returning substitute 
      # while also not prematurely evaluating
      return(s2(..., default = default, .r = TRUE))
    } else {
      s2(..., default = substitute(default), .r = TRUE)
    }
  }
  
  ic_taxable_income_loss <- 
    income %||%
    s2("ic_taxable_income_loss",
       "Taxable_Income", default = NULL)
  if (is.null(ic_taxable_income_loss)) {
    stop("Unable to determine taxable income: .dots.ATO lacked names: 'Taxable_Income' or 'ic_taxable_income_loss'.")
  }
  
  N <- length(ic_taxable_income_loss)
  if (is.data.frame(.dots.ATO) && N != nrow(.dots.ATO)) {
    stop("`length(income) = ", length(income), "` yet `nrow(.dots.ATO) = ", nrow(.dots.ATO), ".")
  }
  
  c_age_30_june <- age_from_file(.dots.ATO)
  
  
  
  is_net_rent <- 
    s2("is_net_rent", "Net_rent_amt")
  
  it_property_loss <- 
    s2("it_property_loss",
       "Net_rent_amt")
  
  it_rept_empl_super_cont <-
    s2("it_rept_empl_super_cont",
       "Rptbl_Empr_spr_cont_amt")
  
  it_rept_fringe_benefit <-
    s2("it_rept_fringe_benefit",
       "Rep_frng_ben_amt")
  
  it_invest_loss <- 
    s2("it_invest_loss",
       "Net_fincl_invstmnt_lss_amt")
  
  spc_rebate_income <- 
    s2("spc_rebate_income",
       "sp_taxable_income_loss",
       "Spouse_adjusted_taxable_inc")
  
  is_married <-
    s2("sp_flag", 
       "Partner_status",
       "Marital_status")
  
  n_dependants <- .subset2(.dots.ATO, "c_depend_child") %||% 0L
  
  sc_empl_cont <-
    s2("sc_empl_cont",
       "MCS_Emplr_Contr")
  
  ds_pers_super_cont <-
    s2("ds_pers_super_cont",
       "Non_emp_spr_amt")
  
  i_frank_cr <-
    s2("i_frank_cr")
  
  # Want this to totally dictate sapto eligibility and rates
  on_sapto_cd <-
    if (is.data.table(.dots.ATO)) {
      if (hasName(.dots.ATO, "on_sapto_cd")) {
        rep_len(as_raw_sapto_cd(.subset2(.dots.ATO, "on_sapto_cd")), N)
      } else {
        rep_len(sf2osc(.dots.ATO), N)
      } 
    } else if (is.null(c_age_30_june)) {
      # Assume never get SAPTO
      rep.int(as.raw(0), N)
    } else {
      # Assume based on Age, no couples
      rep_len(as_raw_sapto_cd(fifelse(c_age_30_june > 65, "A", "")), N)
    }
  
  # on_sapto_cd <- rep_len(as_raw_sapto_cd("A"), N)
  
  # don't allocate the same vector
  zero <- integer(N)
  rN <- function(x) .rN(x, zero, nThread)
  
  fy.year <- fy.year %||% fy::date2fy(Sys.Date())
  fy.year <- validate_fys_permitted(fy.year)
  yr <- fy2yr(fy.year)
  rebateIncome <- .subset2(.dots.ATO, "ic_rebate_income")
  
  if (is.null(rebateIncome)) {
    rebateIncome <- 
      .Call("Crebate_income", 
            rN(ic_taxable_income_loss),  
            it_rept_empl_super_cont, 
            sc_empl_cont,
            ds_pers_super_cont,
            it_invest_loss,
            is_net_rent, 
            it_rept_fringe_benefit,
            yr, 
            nThread,
            PACKAGE = "grattan")
  }
  
  if (isntConstant(yr)) {
    DT <- data.table(yr,
                     ic_taxable_income_loss = rN(ic_taxable_income_loss),
                     rebateIncome,
                     c_age_30_june = rN(c_age_30_june),
                     is_married = rN(is_married),
                     n_dependants = rN(n_dependants),
                     on_sapto_cd = on_sapto_cd %||% charToRaw('A'),
                     i_frank_cr = i_frank_cr %||% 0L,
                     spc_rebate_income = rN(spc_rebate_income))
    ans <- DT[, "tax" := .Call("Cincome_tax",
                               .BY[[1]],
                               ic_taxable_income_loss,
                               rebateIncome,
                               c_age_30_june,
                               is_married,
                               n_dependants,
                               spc_rebate_income,
                               on_sapto_cd,
                               i_frank_cr,
                               System, # RSystem
                               1L,
                               summary,
                               PACKAGE = "grattan"),
              by = "yr"]
    return(.subset2(ans, "tax"))
  }
  
  .Call("Cincome_tax",
        yr[1],
        rN(ic_taxable_income_loss),
        rebateIncome,
        rN(c_age_30_june),
        rN(is_married),
        rN(n_dependants),
        rN(spc_rebate_income),
        on_sapto_cd %||% rep.int(charToRaw('A'), N),
        i_frank_cr,
        System, # RSystem
        nThread,
        summary,
        PACKAGE = "grattan")
  
  
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




