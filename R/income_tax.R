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
      return(do_rN(x, N, max_allowed = 99e6))
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

income_tax_ <- function(.sample.file, yr, n_dependants = 0L) {
  gcf <- function(nom, ...) {
    eval.parent(substitute(get_column_from(.sample.file, nom, ...)))
  }
  
  do_income_tax_sf(yr,
                   nrow(.sample.file), 
                   ic_taxable_income_loss = gcf("ic_taxable_income_loss", "Taxable_Income"), 
                   c_age_30_june = gcf("c_age_30_june", decode_age_range(age_range)), 
                   is_net_rent = gcf("is_net_rent", "Net_rent_amt"), 
                   rebateIncome = gcf("ic_rebate_income", "ic_taxable_income_loss"),
                   it_property_loss = gcf("it_property_loss", -pmin0(Net_rent_amt)), 
                   it_rept_empl_super_cont = gcf("it_rept_empl_super_cont", "Rptbl_Empr_spr_cont_amt"), 
                   it_rept_fringe_benefit = gcf("it_rept_fringe_benefit", "Rep_frng_ben_amt"), 
                   it_invest_loss = gcf("it_invest_loss", "Rep_frng_ben_amt"),
                   spc_rebate_income = pminC(gcf("spc_rebate_income", "Spouse_adjusted_taxable_inc", "Partner_status"), 1e7L),
                   # partner_status = gcf("sp_flag", "Partner_status"),
                   n_dependants = n_dependants %||% gcf("n_dependants", "Partner_status"))
}


# rolling_income_tax is inflexible by design: returns the tax payable in the fy.year; no scope for policy change.
rolling_income_tax <- function(income, 
                               fy.year, 
                               age = NULL, # answer to life, more importantly < 65, and so key to SAPTO, medicare
                               family_status = "individual",
                               n_dependants = 0L,
                               .dots.ATO = NULL,
                               allow.forecasts = FALSE, 
                               .debug = FALSE) {
  
  if (!all(fy.year %chin% c("2000-01", "2001-02", 
                            "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", 
                            "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", 
                            "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20"))) {
    if (allow.forecasts) {
      stop("rolling income tax not intended for future years or years before 2003-04. ",
           "Consider new_income_tax().")
    }
    bad <- which(!is.fy(fy.year))
    if (length(bad) > 5) {
      stop("Entries ", bad[1:5], " (and others)", 
           " were not in correct form.", "\n", "First bad entry: ", fy.year[bad[1]])
    } else {
      stop("Entries ", bad, 
           " were not in correct form.", "\n", "First bad entry: ", fy.year[bad[1]])
    }
  }
  

  
  # CRAN NOTE avoidance
  fy_year <- NULL; marginal_rate <- NULL; lower_bracket <- NULL; tax_at <- NULL; n <- NULL; tax <- NULL; ordering <- NULL; max_lito <- NULL; min_bracket <- NULL; lito_taper <- NULL; sato <- NULL; taper <- NULL; rate <- NULL; max_offset <- NULL; upper_threshold <- NULL; taper_rate <- NULL; medicare_income <- NULL; lower_up_for_each_child <- NULL;
  
  .dots.ATO.noms <- names(.dots.ATO)
  
  # Assume everyone of pension age is eligible for sapto.
  if (OR(is.null(.dots.ATO),
         AND("age_range" %notin% .dots.ATO.noms,
             "Birth_year" %notin% .dots.ATO.noms))) {
    if (is.null(age)) {
      sapto.eligible <- FALSE
    } else {
      sapto.eligible <- age >= 65
    }
  } else {
    if (!is.null(age)) {
      warning("`age` is not NULL but `.dots.ATO` is supplied with an age variable, ",
              "so age will be ignored.")
    }
    if ("age_range" %chin% .dots.ATO.noms) {
      if ("Birth_year" %chin% .dots.ATO.noms) {
        sapto.eligible <- coalesce(between(.subset2(.dots.ATO, "age_range"), 0L, 1L), 
                                   between(.subset2(.dots.ATO, "Birth_year"), 0L, 1L),
                                   FALSE)
      } else {
        sapto.eligible <- coalesce(between(.subset2(.dots.ATO, "age_range"), 0L, 1L), FALSE)  
      }
    } else {
      sapto.eligible <- coalesce(between(.subset2(.dots.ATO, "Birth_year"), 0L, 1L), FALSE)
    }
  }
  
  if (is.null(age)) {
    age <- 42
  }
  
  # Don't like vector recycling
  # http://stackoverflow.com/a/9335687/1664978
  # Also lengths() -- but doesn't appear to be faster, despite being so advertised 
  input.lengths <-
    prohibit_vector_recycling.MAXLENGTH(income, fy.year, age, family_status, n_dependants)
  max.length <- max(input.lengths)
  
  input <- 
    data.table(income = income, 
               fy_year = fy.year) 
  
  input[, ordering := seq_len(.N)]
  setkeyv(input, c("fy_year", "income"))
  
  # input.keyed <-
  #   # potentially expensive. Another way would be 
  #   # to add an argument such as data.table.copy = FALSE
  #   # to allow different way to preserve the order
  #   copy(input) %>%
  #   setkey(fy_year, income)
  
  # tax_fun <- function(income, fy.year){
  #   tax_table2[input, roll = Inf] %>%
  #     .[, tax := tax_at + (income - lower_bracket) * marginal_rate] %>%
  #     setorderv("ordering") %>%
  #     .[["tax"]]
  # }
  
  base_tax. <- 
    tax_table2[input, roll = Inf] %>%
    .[, tax := tax_at + (income - lower_bracket) * marginal_rate] %>%
    setorderv("ordering") %>%
    .[["tax"]]
  
  setkeyv(input, "fy_year")  # for .lito below
  
  # If .dots.ATO  is NULL, for loops over zero-length vector
  for (j in which(vapply(.dots.ATO, FUN = is.double, logical(1)))){
    set(.dots.ATO, j = j, value = as.integer(.dots.ATO[[j]]))
  }
  
  if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
    # integer(max.length) rather than just 0L because we subset later
    #   the_spouse_income[wse]
    # to help solving #158
    the_spouse_income <- integer(max.length)
  } else {
    the_spouse_income <- .subset2(.dots.ATO, "Spouse_adjusted_taxable_inc")
    the_spouse_income[is.na(the_spouse_income)] <- if (is.double(the_spouse_income)) 0 else 0L
  }
  
  medicare_levy. <- 
    medicare_levy(income, 
                  Spouse_income = the_spouse_income,
                  fy.year = fy.year, 
                  sapto.eligible = sapto.eligible, 
                  family_status = if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% names(.dots.ATO)){
                    family_status
                  } else {
                    FS <- rep_len("individual", max.length)
                    FS[the_spouse_income > 0] <- "family"
                    FS
                  }, 
                  n_dependants = n_dependants, 
                  .checks = FALSE)
  
  lito. <- .lito(input)
  
  if (any(sapto.eligible)) {
    sapto. <- double(max.length)
    if (!is.null(.dots.ATO)) {
      wse <- which(sapto.eligible)
      dase <- function(v) {
        if (v %in% names(.dots.ATO)) {
          .subset2(.dots.ATO, v)[wse]
        } else {
          # If the name is absent, choose zero, 
          # length is irrelevant since we just add it inside rebate income
         0
        }
      }
      
      sapto.[wse] <-
        sapto(rebate_income = rebate_income(Taxable_Income = income[wse],
                                            Rptbl_Empr_spr_cont_amt = dase("Rptbl_Empr_spr_cont_amt"),
                                            Net_fincl_invstmt_lss_amt = dase("Net_fincl_invstmt_lss_amt"),
                                            Net_rent_amt = dase("Net_rent_amt"),
                                            Rep_frng_ben_amt = dase("Rep_frng_ben_amt")), 
              fy.year = if (length(fy.year) > 1) fy.year[wse] else fy.year,
              Spouse_income = the_spouse_income[wse],
              family_status = {
                FS_sapto <- rep_len("single", max.length)
                FS_sapto[the_spouse_income > 0] <- "married"
                FS_sapto[wse]
              },
              sapto.eligible = TRUE)
    } else {
      sapto.[sapto.eligible] <- 
        sapto(rebate_income = rebate_income(Taxable_Income = income[sapto.eligible]), 
              fy.year = if (length(fy.year) > 1) fy.year[sapto.eligible] else fy.year,
              sapto.eligible = TRUE)
    }
  } else {
    sapto. <- 0
  }
  
  # https://www.legislation.gov.au/Details/C2014A00048
  # input[["fy_year"]] ensures it matches the length of income if length(fy.year) == 1.
  if (any(c("2014-15", "2015-16", "2016-17") %fin% .subset2(input, "fy_year"))) {
    temp_budget_repair_levy. <-
      0.02 * pmax0(income - 180e3) *
      {.subset2(input, "fy_year") %chin% c("2014-15", "2015-16", "2016-17")}
  } else {
    temp_budget_repair_levy. <- 0
  }
  
  if (any("2011-12" %fin% .subset2(input, "fy_year"))) {
    flood_levy. <- 
      0.005 *
      {.subset2(input, "fy_year") == "2011-12"} * 
      {pmax0(income - 50e3) + pmax0(income - 100e3)}
  } else {
    flood_levy. <- 0
  }
  
  # http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html
  S4.10_basic_income_tax_liability <- pmax0(base_tax. - lito. - sapto.)
  
  # SBTO can only be calculated off .dots.ATO
  if (is.null(.dots.ATO)) {
    sbto. <- 0
  } else {
    sbto. <-
      small_business_tax_offset(taxable_income = income,
                                basic_income_tax_liability = S4.10_basic_income_tax_liability,
                                .dots.ATO = .dots.ATO,
                                fy_year = fy.year)
  }
  
  if (.debug && is.data.table(.dots.ATO)) {
    new_tax <-
      pmax0(S4.10_basic_income_tax_liability - sbto.) +
      medicare_levy. +
      temp_budget_repair_levy. +
      flood_levy.
    
    result <- 
      as.data.table(.dots.ATO) %>%
      .[, "base_tax" := base_tax.] %>%
      .[, "lito" := lito.] %>%
      .[wse, "rebate_income" := rebate_income(Taxable_Income = income[wse],
                                              Rptbl_Empr_spr_cont_amt = dase("Rptbl_Empr_spr_cont_amt"),
                                              Net_fincl_invstmt_lss_amt = dase("Net_fincl_invstmt_lss_amt"),
                                              Net_rent_amt = dase("Net_rent_amt"),
                                              Rep_frng_ben_amt = dase("Rep_frng_ben_amt"))] %>%
      .[, "sapto" := sapto.] %>%
      .[, "medicare_levy" := medicare_levy.] %>%
      .[, "SBTO" := sbto.] %>%
      .[, "new_tax" := new_tax]
    
    # if (fy.year == "2011-12") {
    #   result[, "flood_levy" := flood_levy.]
    # }
    
    return(result[])
  }
  
  # SBTO is non-refundable (Para 1.6 of explanatory memo)
  pmax0(S4.10_basic_income_tax_liability - sbto.) +
    medicare_levy. +
    temp_budget_repair_levy. +
    flood_levy.
}






income_tax_cpp <- function(income,
                           fy.year,
                           .dots.ATO = NULL, 
                           sapto.eligible = NULL,
                           n_dependants = 0L, 
                           .debug = FALSE) {
  # Assume everyone of pension age is eligible for sapto.
  .dots.ATO.noms <- names(.dots.ATO)
  
  # Assume everyone of pension age is eligible for sapto.
  if (is.null(sapto.eligible)) {
    if (OR(is.null(.dots.ATO),
           AND("age_range" %notin% .dots.ATO.noms,
               "Birth_year" %notin% .dots.ATO.noms))) {
      sapto.eligible <- FALSE
      
    } else {
      if ("age_range" %chin% .dots.ATO.noms) {
        if ("Birth_year" %chin% .dots.ATO.noms) {
          sapto.eligible <- coalesce(between(.subset2(.dots.ATO, "age_range"), 0L, 1L), 
                                     between(.subset2(.dots.ATO, "Birth_year"), 0L, 1L),
                                     FALSE)
        } else {
          sapto.eligible <- coalesce(between(.subset2(.dots.ATO, "age_range"), 0L, 1L), FALSE)  
        }
      } else {
        if ("Birth_year" %chin% .dots.ATO.noms) {
          sapto.eligible <- coalesce(between(.subset2(.dots.ATO, "Birth_year"), 0L, 1L), FALSE)
          # nocov start
        } else stop("Not reachable. Please report: income_tax_cpp:319.")
        # nocov end
      }
    }
  }
  
  max.length <- length(income)
  if (is.null(.dots.ATO) || "Spouse_adjusted_taxable_inc" %notin% .dots.ATO.noms) {
    SpouseIncome <- double(max.length)
    isFamily <- logical(max.length)
  } else {
    SpouseIncome <- .subset2(.dots.ATO, "Spouse_adjusted_taxable_inc")
    isFamily <- SpouseIncome > 0
  }
  
  if (max.length > 1 && length(n_dependants) == 1L) {
    n_dependants <- rep_len(n_dependants, max.length)
  }
  
  if (length(fy.year) != 1L) {
    stop("`fy.year` was length-",
         prettyNum(length(fy.year), big.mark = ","), ". ",
         "Use a single-length financial year in `income_tax_cpp()` ",
         "or use `income_tax()`.",
         call. = FALSE)
  }
  
  if (fy.year < "2012-13") {
    stop("`fy.year` was ", fy.year, ". ",
         "Use a single financial year from 2012-13 onwards ", 
         "or use `income_tax()`.", 
         call. = FALSE)
  }
  
  base_tax. <- 
    IncomeTax(income, 
              thresholds = c(0, 18200, 37000,
                             if (fy.year < "2016-17") 80e3 else if (fy.year < "2018-19") 87e3 else 90e3,
                             180000),
              rates = c(0, 0.19, 0.325, 0.37, 
                        0.45))
  
  lito. <- pminC(pmax0(445 - (income - 37000) * 0.015),
                 445)

  switch(fy.year,
         "2012-13" = {
           Year.int <- 2013L
         },
         "2013-14" = {
           Year.int <- 2014L
         },
         "2014-15" = {
           Year.int <- 2015L
         },
         "2015-16" = {
           Year.int <- 2016L
         },
         "2016-17" = {
           Year.int <- 2017L
         },
         "2017-18" = {
           Year.int <- 2018L
         },
         "2018-19" = {
           Year.int <- 2019L
         }, 
         "2019-20" = {
           Year.int <- 2020L
         },
         "2020-21" = {
           Year.int <- 2021L
         },
         stop("`fy.year` was ", fy.year, ". ",
              "Use a single financial year from 2012-13 onwards ", 
              "or use `income_tax()`.", 
              call. = FALSE))
  
  if (any(sapto.eligible)) {
    # Use this a lot, saves 0.5ms each time
    which_sapto <- which(sapto.eligible)
    which_not_sapto <- which(!sapto.eligible)
    
    SpouseIncome_sapto_eligible <- SpouseIncome[which_sapto]
    
    if (length(sapto.eligible) != 1L) {
      medicare_levy. <- double(max.length)
      medicare_levy.[which_sapto] <- 
        MedicareLevySaptoYear(income[which_sapto],
                              SpouseIncome = SpouseIncome_sapto_eligible,
                              NDependants = n_dependants[which_sapto], 
                              TRUE, 
                              Year.int)
      
      medicare_levy.[which_not_sapto] <- 
        MedicareLevySaptoYear(income[which_not_sapto],
                              SpouseIncome = SpouseIncome[which_not_sapto],
                              NDependants = n_dependants[which_not_sapto],
                              FALSE,
                              Year.int)
      
    } else {
      # All are SAPTO.
      medicare_levy. <-
        MedicareLevySaptoYear(income,
                              SpouseIncome = SpouseIncome,
                              NDependants = n_dependants,
                              TRUE, 
                              Year.int)
    }
    
    
    
    if (AND(!is.null(.dots.ATO),
            all(c("Rptbl_Empr_spr_cont_amt",
                  "Net_fincl_invstmt_lss_amt",
                  "Net_rent_amt", 
                  "Rep_frng_ben_amt") %chin% .dots.ATO.noms))) {
      sapto. <- double(max.length)
      .dAse <- function(v) {
        .subset2(.dots.ATO, v)[which_sapto]
      }
      
      sapto.[which_sapto] <-
        sapto_rcpp_yr(RebateIncome = rebate_income(Taxable_Income = income[which_sapto],
                                                   Rptbl_Empr_spr_cont_amt = .dAse("Rptbl_Empr_spr_cont_amt"),
                                                   Net_fincl_invstmt_lss_amt = .dAse("Net_fincl_invstmt_lss_amt"),
                                                   Net_rent_amt = .dAse("Net_rent_amt"),
                                                   Rep_frng_ben_amt = .dAse("Rep_frng_ben_amt")),
                      SpouseIncome = SpouseIncome_sapto_eligible,
                      IsMarried = SpouseIncome_sapto_eligible > 0,
                      yr = Year.int)
      
    } else {
      sapto. <- sapto.eligible * sapto(rebate_income = rebate_income(Taxable_Income = income), 
                                       fy.year = fy.year, 
                                       sapto.eligible = TRUE)
    }
  } else {
    medicare_levy. <-
      MedicareLevySaptoYear(income = income,
                            SpouseIncome = SpouseIncome,
                            NDependants = n_dependants,
                            FALSE,
                            Year.int)
    sapto. <- 0
  }
  
  # 2011-12 is never used
  flood_levy. <- 0
  
  # http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html
  S4.10_basic_income_tax_liability <- pmax0(base_tax. - lito. - sapto.)
  
  # SBTO can only be calculated off .dots.ATO
  if (is.null(.dots.ATO)) {
    sbto. <- 0
  } else {
    sbto. <-
      small_business_tax_offset(taxable_income = income,
                                basic_income_tax_liability = S4.10_basic_income_tax_liability,
                                .dots.ATO = .selector(.dots.ATO, c("Total_PP_BE_amt", 
                                                                   "Total_PP_BI_amt", 
                                                                   "Total_NPP_BE_amt",
                                                                   "Total_NPP_BI_amt")),
                                fy_year = fy.year)
  }
  
  out <-
    pmax0(S4.10_basic_income_tax_liability - sbto.) +
    medicare_levy. +
    flood_levy. 
  
  # temp budget repair levy
  if (Year.int <= 2017L && Year.int >= 2015L) {
    out <- out + 0.02 * pmax0(income - 180e3)
  }
  
  if (any(income < 0)) {
    warning("Negative entries in income detected. These will have value NA.")
    out[income < 0] <- switch(storage.mode(out), 
                              "integer" = NA_integer_,
                              "double" = NA_real_)
  }
  
  if (.debug && is.data.table(.dots.ATO)) {
    result <- 
      copy(.dots.ATO) %>%
      .[, "base_tax" := base_tax.] %>%
      .[, "lito" := lito.] %>%
      .[, "sapto" := sapto.] %>%
      .[, "medicare_levy" := medicare_levy.] %>%
      .[, "income_tax" := out] %>%
      .[, "SBTO" := sbto.] 
      
    # if (fy.year == "2011-12") {
    #   result[, "flood_levy" := flood_levy.]
    # }
    
    return(result[])
  }
  
  out
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
      return(do_rN(x, N, max_allowed = 99e6))
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
  fy.year <- fy.year %||% fy::date2fy(Sys.Date())
  fy.year <- validate_fys_permitted(fy.year)
  yr <- fy2yr(fy.year)
  
  if (is.null(offsets)) {
    # offsets <- 
    #   list(LITO = set_offset(offset_1st = LITO_MAX_OFFSET(yr),
    #                          thresholds = LITO_1ST_THRESH(yr),
    #                          tapers = LITO_1ST_TAPER(yr), 
    #                          refundable = FALSE),
    #        LMITO = set_offset(offset_1st = LMITO_1ST_OFFSET(yr),
    #                           thresholds = LMITO_THRESHS(yr), 
    #                           tapers = LMITO_TAPERS(yr), 
    #                           refundable = FALSE))
  }
  # stopifnot(is.list(offsets),
  #           identical(unique(lengths(offsets)), 4L))
  # lapply(offsets, function(offset) {
  #   if (!haveNames(offset, req_names <- c("offset_1st", "thresholds", "tapers", "refundable"))) {
  #     stop("`offset` lacks required names: ", toString(req_names), ".")
  #   }
  # })
  
  
  # do_income_tax2(ic_taxable_income_loss = rN(ic_taxable_income_loss), 
  #                yr = yr,
  #                c_age_30_june = rN(c_age_30_june),
  #                rebateIncome = rebateIncome,
  #                ds_pers_super_cont = rN(ds_pers_super_cont),
  #                is_net_rent = rN(is_net_rent),
  #                it_property_loss = rN(it_property_loss),
  #                it_rept_empl_super_cont = rN(it_rept_empl_super_cont),
  #                it_rept_fringe_benefit = rN(it_rept_fringe_benefit),
  #                it_invest_loss = rN(it_invest_loss),
  #                sc_empl_cont = rN(sc_empl_cont),
  #                spc_rebate_income = rN(spc_rebate_income),
  #                isn_sbi_net = rN(isn_sbi_net),
  #                is_married = is_married,
  #                n_dependants = rN(n_dependants),
  #                ordinary_tax_thresholds = ordinary_tax_thresholds %||% ORD_TAX_BRACK(yr),
  #                ordinary_tax_rates = ordinary_tax_rates %||% ORD_TAX_RATES(yr),
  #                temp_levy_brack = temp_levy_brack %||% LEVY_BRACK(yr),
  #                temp_levy_rates = temp_levy_rates %||% LEVY_RATES(yr),
  #                offsets = offsets,
  #                medicare_levy_taper = medicare_levy_taper %||% ML_TAPER(yr), 
  #                medicare_levy_rate = medicare_levy_rate   %||% ML_RATE(yr),
  #                medicare_levy_lower_threshold = medicare_levy_lower_threshold %||% ML_LWR_THRESHOLD_SINGLE(yr),
  #                medicare_levy_lower_sapto_threshold = medicare_levy_lower_sapto_threshold %||% ML_LWR_THRESHOLD_SINGLE_SAPTO(yr),
  #                medicare_levy_lower_family_threshold = medicare_levy_lower_family_threshold %||% ML_LWR_THRESHOLD_FAMILY(yr),
  #                medicare_levy_lower_family_sapto_threshold = medicare_levy_lower_family_sapto_threshold %||% ML_LWR_THRESHOLD_FAMILY_SAPTO(yr),
  #                medicare_levy_lower_up_for_each_child = medicare_levy_lower_up_for_each_child %||% ML_LWR_THR_UP_PER_CHILD(yr),
  #                
  #                sapto_max_offset = sapto_max_offset %||% SAPTO_MAX_SINGLE(yr),
  #                sapto_max_offset_married = sapto_max_offset_married %||% SAPTO_MAX_MARRIED(yr),
  #                sapto_lower_threshold = sapto_lower_threshold %||% SAPTO_LWR_SINGLE(yr),
  #                sapto_lower_threshold_married = sapto_lower_threshold_married %||% SAPTO_LWR_MARRIED(yr))
  Yr <- yr
  
  rebateIncome <- 
    .subset2(.dots.ATO, "ic_rebate_income")
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
            nThread,
            PACKAGE = "grattanDev")
  }
   
  .Call("Cincome_tax",
        Yr,
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




