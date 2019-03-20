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
#' income_tax(50e3, "2013-14")
#' 
#' ## Calculate tax for each lodger in the 2013-14 sample file.
#' ## Essentially, this is the only use-case for `income_tax`
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
                       n_dependants = 0L,
                       .dots.ATO = NULL,
                       return.mode = c("numeric", "integer"),
                       allow.forecasts = FALSE,
                       .debug = FALSE) {
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
  
 
  
  if (is.null(age) &&
      length(fy.year) == 1L &&
      fy.year %chin% c("2012-13", "2013-14", 
                       "2014-15", "2015-16",
                       "2016-17", "2017-18",
                       "2018-19", "2019-20")) {
    out <- income_tax_cpp(income,
                          fy.year = fy.year,
                          .dots.ATO = .dots.ATO,
                          n_dependants = n_dependants,
                          .debug = .debug)
  } else {
    # Absolutely necessary
    .dots.ATO.copy <- copy(.dots.ATO)
    
    out <- rolling_income_tax(income = income,
                              fy.year = fy.year,
                              age = age, 
                              family_status = family_status,
                              n_dependants = n_dependants, 
                              .dots.ATO = .dots.ATO.copy,
                              allow.forecasts = allow.forecasts, 
                              .debug = .debug)
  }
  return.mode <- match.arg(return.mode)
  
  switch(return.mode, 
         "numeric" = {
           return(out)
         }, 
         "integer" = {
           return(as.integer(floor(out)))
         })
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
      0.02 * pmaxIPnum0(income - 180e3) *
      {.subset2(input, "fy_year") %chin% c("2014-15", "2015-16", "2016-17")}
  } else {
    temp_budget_repair_levy. <- 0
  }
  
  if (any("2011-12" %fin% .subset2(input, "fy_year"))) {
    flood_levy. <- 
      0.005 *
      {.subset2(input, "fy_year") == "2011-12"} * 
      {pmaxIPnum0(income - 50e3) + pmaxIPnum0(income - 100e3)}
  } else {
    flood_levy. <- 0
  }
  
  # http://classic.austlii.edu.au/au/legis/cth/consol_act/itaa1997240/s4.10.html
  S4.10_basic_income_tax_liability <- pmaxIPnum0(base_tax. - lito. - sapto.)
  
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
      pmaxIPnum0(S4.10_basic_income_tax_liability - sbto.) +
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
  pmaxIPnum0(S4.10_basic_income_tax_liability - sbto.) +
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
  S4.10_basic_income_tax_liability <- pmaxIPnum0(base_tax. - lito. - sapto.)
  
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




