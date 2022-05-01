#' Income tax payable
#' 
#' @name income_tax
#' @param income The individual assessable income.
#' @param fy.year The financial year in which the income was earned. Tax years 2000-01 to 2018-19 are supported, as well as the tax year 2019-20, for convenience. 
#' If \code{fy.year} is not given, the current financial year is used by default.
#' @param age The individual's age. Ignored if \code{.dots.ATO} is provided (and contains
#' an age variable such as \code{age_range} or \code{Birth_year}).
#' @param family_status For Medicare and SAPTO purposes.
#' @param n_dependants An integer for the number of children of the taxpayer (for the purposes of the Medicare levy).
#' @param return.mode The mode (numeric or integer) of the returned vector.
#' @param .dots.ATO A data.frame that contains additional information about the individual's circumstances, with columns the same as in the ATO sample files.
#' 
#' Age variables in \code{.dots.ATO} take precedence over \code{age} and providing both
#' is a warning.
#' 
#' @param allow.forecasts should dates beyond 2019-20 be permitted? Currently, not permitted.
#' @param .debug (logical, default: \code{FALSE})  If \code{TRUE}, returns a \code{data.table} containing the components of income tax calculated. (This argument and its result is liable to change in future versions, possibly without notice.)
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
#' if (requireNamespace("taxstats", quietly = TRUE)) {
#'   library(data.table)
#'   library(taxstats)
#'   
#'   s1314 <- as.data.table(sample_file_1314)
#'   s1314[, tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = s1314)]
#' }
#' 
#' @export income_tax

income_tax <- function(income,
                       fy.year = NULL,
                       age = NULL,
                       family_status = "individual",
                       n_dependants = NULL,
                       .dots.ATO = NULL,
                       return.mode = c("numeric", "integer"),
                       allow.forecasts = FALSE,
                       .debug = FALSE) {
  tax <- NULL
  if (is.null(fy.year)) {
    fy.year <- date2fy(Sys.Date())
    warning("fy.year is missing, using current financial year")
  }
  
  if (!is.null(.dots.ATO)) {
    if (!is.data.frame(.dots.ATO)) {
      stop(".dots.ATO should be a data frame/data table")
    } else {
      if (nrow(.dots.ATO) != length(income)){
        stop("Number of rows of .dots.ATO does not match length of income.")
      }
    }
  }
  return.mode <- match.arg(return.mode)
  
  fy.year <- fy::validate_fys_permitted(fy.year, min.yr = 1984L, max.yr = 2030L)
  yr <- fy::fy2yr(fy.year)
  
  if (!missing(family_status) && any(family_status != "individual")) {
    warning("`family_status` was provided, but is deprecated, since `family_status = 'married'` ",
            "and `family_status = 'family' have different behaviour.\n",
            "Use `.dots.ATO`, creating a `sp_flag`, `c_depend_child`, and
            `spc_rebate_income` to more precisely identify the thresholds.")
  }
  
  if (min(income, na.rm = TRUE) < 0) {
    warning("`income` has negative values, which will be set to zero.")
    income <- pmax0(income)
  }
  
  
  # Now set values based on arguments, columns in .dots.ATO 
  # Order of preference:
  #   aLife variables (most accurate)
  #   sample file variables
  #   supplied variables
  #   defaults
  
  # We handle defaults here so that it's more visible to users (rather than
  # the body() of the function just referring to an Rcpp function), and
  # because it's easier (and no real damage to performance) to handle 
  # mixture of NULLs, length-ones, vectors, and symbols
  
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
       "Taxable_Income")
  
  # Normally this is the default ... 
  N <- length(ic_taxable_income_loss)
  #    ... but we allow
  # comparisons for a single taxable income across years
  if (length(yr) != N && length(yr) != 1L) {
    if (N == 1L) {
      N <- length(yr)
      ic_taxable_income_loss <- rep.int(ic_taxable_income_loss, N)
    } else {
      stop("`length(fy.year) = ", length(fy.year),
           "`, yet `length(income) = ", length(income), "`, ",
           if (NROW(.dots.ATO)) "NROW(.dots.ATO) = ", NROW(.dots.ATO), ". ",
           "The only permitted lengths for `fy.year` are the length of income, ",
           "and 1.")
    }
  }
  
  c_age_30_june <- age_from_file(.dots.ATO, age)
    
  rebateIncome <- 
    .subset2(.dots.ATO, "ic_rebate_income")

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
       "Marital_status", 
       default = rep_len(family_status == "married", N))
  
  if (is.null(n_dependants)) {
    n_dependants <- .subset2(.dots.ATO, "c_depend_child") %||% 0L
  }
  
  
  sc_empl_cont <-
    s2("sc_empl_cont",
       "MCS_Emplr_Contr")
  
  ds_pers_super_cont <-
    s2("ds_pers_super_cont",
       "Non_emp_spr_amt")
    
  integerN <- integer(N)
  rN <- function(x) {
    if (is.integer(x) && length(x) == N && !anyNA(x)) {
      return(x)
    }
    if (is.integer(x)) {
      if (identical(x, 0L)) {
        return(integerN)
      }
      return(rep_len(fcoalesce(x, 0L), N))
    }
    if (is.double(x)) {
      # Some doubles are almost certainly integers
      # that have been mistranscribed (like phone numbers)
      # return(do_rN(x, N, max_allowed = 99e6))
      return(rep_len(as.integer(pminC(x, .Machine$integer.max)), N))
    }
    x
  }
  
  
  isn_sbi_net <- .subset2(.dots.ATO, "isn_sbi_net")
  if (is.null(isn_sbi_net)) {
    # default
    isn_sbi_net <- 0L
    if (all(c("Total_PP_BE_amt",
              "Total_PP_BI_amt",
              "Total_NPP_BE_amt",
              "Total_NPP_BI_amt") %in% names(.dots.ATO))) {
      isn_sbi_net <-
        with(.dots.ATO, 
             sbto_avbl(Total_PP_BE_amt,
                       Total_PP_BI_amt,
                       Total_NPP_BE_amt,
                       Total_NPP_BI_amt))
    }
  }
  
  out <- 
    if (length(yr) == 1L) {
      do_income_tax_sf(yr,
                       # Arguments
                       # Length of output
                       N = N,
                       
                       # Other variables from sample file
                       ic_taxable_income_loss = rN(ic_taxable_income_loss), 
                       c_age_30_june = rN(c_age_30_june),
                       rebateIncome = rebateIncome,
                       ds_pers_super_cont = rN(ds_pers_super_cont),
                       is_net_rent = rN(is_net_rent),
                       it_property_loss = rN(it_property_loss),
                       it_rept_empl_super_cont = rN(it_rept_empl_super_cont),
                       it_rept_fringe_benefit = rN(it_rept_fringe_benefit),
                       it_invest_loss = rN(it_invest_loss),
                       sc_empl_cont = rN(sc_empl_cont),
                       spc_rebate_income = rN(spc_rebate_income),
                       isn_sbi_net = rN(isn_sbi_net),
                       is_married = is_married,
                       n_dependants = rN(n_dependants))
    } else {
      if (length(yr) != N) {
        stop("`length(fy.year) = ", length(fy.year), "`, but ", 
             "`length(income) = ", N, ".")
      }
      
      DT <- list(yr = yr,
                 ic_taxable_income_loss = rN(ic_taxable_income_loss), 
                 c_age_30_june = rN(c_age_30_june),
                 ds_pers_super_cont = rN(ds_pers_super_cont),
                 is_net_rent = rN(is_net_rent),
                 it_property_loss = rN(it_property_loss),
                 it_rept_empl_super_cont = rN(it_rept_empl_super_cont),
                 it_rept_fringe_benefit = rN(it_rept_fringe_benefit),
                 it_invest_loss = rN(it_invest_loss),
                 sc_empl_cont = rN(sc_empl_cont),
                 spc_rebate_income = rN(spc_rebate_income),
                 isn_sbi_net = rN(isn_sbi_net),
                 is_married = rN(is_married),
                 n_dependants = rN(n_dependants))
      setDT(DT)
      DT[, tax := do_income_tax_sf(yr = .BY[["yr"]], 
                                   N = .N,
                                   ic_taxable_income_loss = ic_taxable_income_loss, 
                                   c_age_30_june = c_age_30_june,
                                   rebateIncome = rebateIncome,
                                   ds_pers_super_cont = ds_pers_super_cont,
                                   is_net_rent = is_net_rent,
                                   it_property_loss = it_property_loss,
                                   it_rept_empl_super_cont = it_rept_empl_super_cont,
                                   it_rept_fringe_benefit = it_rept_fringe_benefit,
                                   it_invest_loss = it_invest_loss,
                                   sc_empl_cont = sc_empl_cont,
                                   spc_rebate_income = spc_rebate_income,
                                   isn_sbi_net = isn_sbi_net,
                                   is_married = is_married,
                                   n_dependants = n_dependants),
         by = "yr"]
      return(.subset2(DT, "tax"))
    }
  
  if (return.mode == "integer") {
    out <- as.integer(out)
  }
  return(out)
}







get_column_from <- function(DT, nom, ..., NULL_OK = FALSE) {
  if (is.character(substitute(nom))) {
    if (!hasName(DT, nom)) {
      if (missing(..1)) {
        if (NULL_OK) {
          return(NULL)
        } else {
          stop("DT lacked name '", nom, "'.", typeof)
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




income_tax2 <- function(income, 
                        fy.year = NULL, 
                        .dots.ATO = NULL,
                        ordinary_tax_thresholds = NULL,
                        ordinary_tax_rates = NULL,
                        temp_levy_brack = NULL,
                        temp_levy_rates = NULL,
                        medicare_levy_taper = NULL, 
                        medicare_levy_rate = NULL,
                        medicare_levy_lower_threshold = NULL,
                        medicare_levy_lower_sapto_threshold = NULL,
                        medicare_levy_lower_family_threshold = NULL,
                        medicare_levy_lower_family_sapto_threshold = NULL,
                        medicare_levy_lower_up_for_each_child = NULL,
                        offsets = NULL,
                        sbto_discount = NULL,
                        sapto_eligible = NULL,
                        sapto_max_offset = NULL,
                        sapto_lower_threshold = NULL,
                        sapto_taper = NULL,
                        sapto_max_offset_married = NULL,
                        sapto_lower_threshold_married = NULL,
                        sapto_taper_married = NULL, 
                        nThread = getOption("grattan.nThread", 1L)) {
  
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
       "Taxable_Income")
  
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
  
  # don't allocate the same vector
  integerN <- integer(N)
  rN <- function(x) {
    if (is.logical(x)) {
      return(as.integer(x))
    }
    return(.Call("Cdo_rn", x, integerN, PACKAGE = utils::packageName()))
  }
  
  
  fy.year <- fy.year %||% fy::date2fy(Sys.Date())
  fy.year <- validate_fys_permitted(fy.year)
  yr <- fy2yr(fy.year)
  rebateIncome <- .subset2(.dots.ATO, "ic_rebate_income")
  
  if (is.null(rebateIncome)) {
    rebateIncome <- 
      .Call("Crebate_income", 
            ic_taxable_income_loss,  
            it_rept_empl_super_cont, 
            sc_empl_cont,
            ds_pers_super_cont,
            it_invest_loss,
            is_net_rent, 
            it_rept_fringe_benefit,
            yr, 
            nThread,
            PACKAGE = "grattanDev")
  }
  
  if (isntConstant(yr)) {
    return(accel_repetitive_input(yr, function(yr) {
      .Call("Cincome_tax",
            yr,
            ic_taxable_income_loss,
            rebateIncome,
            rN(c_age_30_june),
            rN(is_married),
            rN(n_dependants),
            rN(spc_rebate_income),
            NULL, # RSystem
            nThread,
            PAKCAGE = "grattanDev")
    }))
  }
  
  .Call("Cincome_tax",
        yr[1],
        ic_taxable_income_loss,
        rebateIncome,
        rN(c_age_30_june),
        rN(is_married),
        rN(n_dependants),
        rN(spc_rebate_income),
        NULL, # RSystem
        nThread,
        PAKCAGE = "grattanDev")
  
  
}

set_offset <- function(offset_1st = integer(1),
                       thresholds = integer(), 
                       tapers = double(), 
                       refundable = logical(1)) {
  checkmate::assert_integerish(offset_1st)
  stopifnot(length(thresholds) == length(tapers),
            is.numeric(thresholds), 
            is.numeric(tapers))
  check_TF(refundable)
  if (is.unsorted(thresholds)) {
    stop("`thresholds = ", thresholds, "` was unsorted.")
  }
  list(offset_1st = offset_1st,
       thresholds = thresholds,
       tapers = tapers, 
       refundable = refundable)
}

.Medicare <- function(yr,
                      medicare_levy_taper = NULL, 
                      medicare_levy_rate = NULL,
                      medicare_levy_lower_threshold = NULL,
                      medicare_levy_lower_sapto_threshold = NULL,
                      medicare_levy_lower_family_threshold = NULL,
                      medicare_levy_lower_family_sapto_threshold = NULL,
                      medicare_levy_lower_up_for_each_child = NULL) {
  
}

.Sapto <- function(yr,
                   sapto_max_offset = NULL,
                   sapto_lower_threshold = NULL,
                   sapto_taper = NULL,
                   sapto_max_offset_married = NULL,
                   sapto_lower_threshold_married = NULL,
                   sapto_taper_married = NULL) {
  
}

.System <- function(yr, 
                    ordinary_tax_thresholds = NULL,
                    ordinary_tax_rates = NULL,
                    temp_levy_brack = NULL,
                    temp_levy_rates = NULL,
                    medicare_levy_taper = NULL, 
                    medicare_levy_rate = NULL,
                    medicare_levy_lower_threshold = NULL,
                    medicare_levy_lower_sapto_threshold = NULL,
                    medicare_levy_lower_family_threshold = NULL,
                    medicare_levy_lower_family_sapto_threshold = NULL,
                    medicare_levy_lower_up_for_each_child = NULL,
                    offsets = NULL,
                    sapto_max_offset = NULL,
                    sapto_lower_threshold = NULL,
                    sapto_taper = NULL,
                    sapto_max_offset_married = NULL,
                    sapto_lower_threshold_married = NULL,
                    sapto_taper_married = NULL) {
  RSystem <- mget(ls(sorted = FALSE))
  Filter(length, RSystem)
  
  # .Call("C_RSystem", Filter(length, RSystem), PACKAGE = "grattanDev")
}

income_tax222 <- function(income, yincome, age, rebate) {
  zero <- integer(length(income))
  .Call("Cincome2022", income, as.integer(yincome), rebate, age, zero, zero, 4L, PACKAGE = "grattanDev")
}




