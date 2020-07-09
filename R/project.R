#' Simple projections of the annual 2\% samples of Australian Taxation Office tax returns.
#' 
#' @param sample_file A \code{data.table} matching a 2\% sample file from the ATO.
#' See package \code{taxstats} for an example.
#' @param h An integer. How many years should the sample file be projected?
#' @param fy.year.of.sample.file The financial year of \code{sample_file}. If \code{NULL}, the default, the number is inferred from the 
#' number of rows of \code{sample_file} to be one of \code{2012-13}, \code{2013-14}, \code{2014-15}, \code{2015-16}, or \code{2016-17}.
#' @param WEIGHT The sample weight for the sample file. (So a 2\% file has \code{WEIGHT} = 50.)
#' @param excl_vars A character vector of column names in \code{sample_file} that should not be inflated. Columns not present in the 2013-14 sample file are not inflated and nor are the columns \code{Ind}, \code{Gender}, \code{age_range}, \code{Occ_code}, \code{Partner_status}, \code{Region}, \code{Lodgment_method}, and \code{PHI_Ind}.
#' @param forecast.dots A list containing parameters to be passed to \code{generic_inflator}.
#' @param wage.series See \code{\link{wage_inflator}}. Note that the \code{Sw_amt}
#'  will uprated by \code{\link{differentially_uprate_wage}} (if requested).
#' @param lf.series See \code{\link{lf_inflator_fy}}.
#' @param use_age_pop_forecast Should the inflation of the number of taxpayers be 
#' moderated by the number of resident persons born in a certain year? If \code{TRUE},
#' younger ages will grow at a slightly higher rate beyond 2018 than older ages.
#' @param .recalculate.inflators (logical, default: \code{NA}). 
#' Should \code{generic_inflator()} or \code{CG_inflator} be called to project the other variables? Adds time.
#' Default \code{NA} means \code{TRUE} if the pre-calculated inflators are available,
#' \code{FALSE} otherwise.
#' 
#' @param .copyDT (logical, default: \code{TRUE}) Should a \code{copy()} of \code{sample_file} be made?
#'  If set to \code{FALSE}, will update \code{sample_file} in place, which may
#'  be necessary when memory is constrained, but is dangerous as it modifies the
#'  original data and its projection. (So if you run the same code twice you 
#'  may end up with a projection \code{2h} years ahead, not \code{h} years.)
#' @param check_fy_sample_file (logical, default: \code{TRUE}) 
#' Should \code{fy.year.of.sample.file} be checked against \code{sample_file}?
#' By default, \code{TRUE}, an error is raised if the base is not 2012-13, 2013-14, 2014-15, 2015-16, 2016-17,
#' or 2017-18,
#'  and a warning is raised if the 
#' number of rows in \code{sample_file} is different to the known number of rows in the sample files. 
#' @param differentially_uprate_Sw (logical, default: \code{NA}) 
#' Should the salary and wage column (\code{Sw_amt}) be differentially uprated 
#' using (\code{\link{differentially_uprate_wage}})? Default of \code{NA} means
#' use differential uprating is used when \code{fy.year.of.sample.file <= "2016-17"}.
#' It is known that the Treasury stopped using differential uprating by 2019.
#' 
#' Selecting \code{TRUE} for \code{fy.year.of.sample.file > "2016-17"} is an 
#' error as the precalculated values are not available.
#' 
#' @param r_super_balance The factor to inflate super balances by (annualized).
#' Set to \code{1.05} for backwards compatibility. The annual superannuation 
#' bulletin of June 2019 from APRA reported 7.3\% growth of funds with more than
#' fund members over the previous 5 years and 7.9\% growth over the 
#' previous ten years.
#' 
#' 
#' @return A sample file with the same number of rows as \code{sample_file} but 
#' with inflated values as a forecast for the sample file in \code{to_fy}. 
#' If \code{WEIGHT} is not already a column of \code{sample_file}, it will be added and its sum
#' will be the predicted number of taxpayers in \code{to_fy}.
#' 
#' @details 
#' 
#' Currently components of taxable income are individually inflated 
#' based on their historical trends in the ATO sample files, with the 
#' exception of:
#' \describe{
#' \item{inflated using \code{\link{differentially_uprate_wage}}.}{\code{Sw_amt}}
#' \item{inflated using \code{\link{wage_inflator}}}{\code{Alow_ben_amt}, \code{ETP_txbl_amt}, \code{Rptbl_Empr_spr_cont_amt}, \code{Non_emp_spr_amt}, \code{MCS_Emplr_Contr}, \code{MCS_Prsnl_Contr}, \code{MCS_Othr_Contr}}
#' \item{inflated using \code{\link{cpi_inflator}}}{\code{WRE_car_amt}, \code{WRE_trvl_amt}, \code{WRE_uniform_amt}, \code{WRE_self_amt}, \code{WRE_other_amt}}
#' \item{inflated by \code{\link{lf_inflator_fy}}}{\code{WEIGHT}}
#' \item{inflated by \code{\link{CG_inflator}}}{\code{Net_CG_amt}, \code{Tot_CY_CG_amt}}
#' }
#' 
#' Superannuation balances are inflated by a fixed rate of 5\% p.a.
#' 
#' We recommend you use \code{sample_file_1213} over \code{sample_file_1314},
#' unless you need the superannuation variables, 
#' as the latter suggests lower-than-recorded tax collections. 
#' However, more recent data is of course preferable.
#' @examples 
#' # install_taxstats()
#' if (requireNamespace("taxstats", quietly = TRUE) &&
#'     requireNamespace("data.table", quietly = TRUE)) {
#'   library(taxstats)
#'   library(data.table)
#'   sample_file <- copy(sample_file_1314)
#'   sample_file_1617 <- project(sample_file,
#'                               h = 3L, # to "2016-17"
#'                               fy.year.of.sample.file = "2013-14")  
#' }
#' @export

project <- function(sample_file, 
                    h = 0L, 
                    fy.year.of.sample.file = NULL, 
                    WEIGHT = 50L, 
                    excl_vars = NULL, 
                    forecast.dots = list(estimator = "mean", pred_interval = 80), 
                    wage.series = NULL,
                    lf.series = NULL,
                    use_age_pop_forecast = FALSE,
                    .recalculate.inflators = NA, 
                    .copyDT = TRUE,
                    check_fy_sample_file = TRUE,
                    differentially_uprate_Sw = NA,
                    r_super_balance = 1.05) {
  if (length(h) != 1L) {
    stop("`h` had length-", length(h), ", ", 
         "but must be a length-1 positive integer.")
  }
  
  if (!is.integer(h)) {
    htxt <- deparse(substitute(h))
    if (is.double(h)) {
      if (grepl("^[0-9]+$", htxt)) {
        hadvised <- paste0(htxt, "L")
        stop("`h = ", htxt, "` was type double, but must be type integer. ", 
             "(Did you mean `h = ", hadvised, "`?)\n\n",
             "Change h to a length-one integer. ",
             "For example, to project two years ahead, use `h = 2L`, not `h = 2`.")
      } else {
        stop("`h = ", htxt, "` was type double, but must be type integer. ", 
             "Change h to a length-one integer. ",
             "For example, to project two years ahead, use `h = 2L`, not `h = 2`.")
      }
    } else {
      stop("`h = ", htxt, "` was type ", typeof(h), ", but must be type integer. ", 
           "Change h to a length-one nonnegative integer.")
    }
  }
  if (anyNA(h)) {
    stop("`h = NA`. This is not permitted. ", 
         "Change h to a length-one nonnegative integer.")
  }
  if (h < 0L) {
    stop("`h = ", h, "`, but must be a nonnegative integer. ",
         "Change h to a nonnegative integer.")
  }
  
  if (!is.data.table(sample_file)) {
    if (is.data.frame(sample_file)) {
      sample_file <- as.data.table(sample_file)
    } else {
      stop("`sample_file` was of class ", class(sample_file)[1L], ", but must be a data.table. ", 
           "Ensure `sample_file`` is a data.frame with the same structure as the ATO's sample files.")
    }
  }
  
  if (.copyDT) {
    sample_file <- copy(sample_file)
  }
  
  
  if (check_fy_sample_file) {
    # It's been a common error of mine to switch sample files
    # without updating the fy.year.of.sample.file
    if (is.null(fy.year.of.sample.file)) {
      fy.year.of.sample.file <-
        match(nrow(sample_file), c(254318L, 258774L, 263339L, 269639L,  277202L, 284925L))
      if (is.na(fy.year.of.sample.file)) {
        stop("`fy.year.of.sample.file` was not provided, and its value could not be ",
             "inferred from nrow(sample_file) = ", nrow(sample_file), ". Either use ", 
             "a 2% sample file of the years 2012-13, 2013-14, or 2014-15 or ", 
             "supply `fy.year.of.sample.file` manually.")
      }
      fy.year.of.sample.file <- 
        c("2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")[fy.year.of.sample.file]
    }
    
    
    switch(fy.year.of.sample.file, 
           "2012-13" = {
             if (nrow(sample_file) != 254318) {
               warning("nrow(sample_file) != 254318. Should you choose a different fy.year.of.sample.file?")
             }
           },
           "2013-14" = {
             if (nrow(sample_file) != 258774) {
               warning("nrow(sample_file) != 254318. Should you choose a different fy.year.of.sample.file?")
             }
           },
           "2014-15" = {
             if (nrow(sample_file) != 263339) {
               warning("nrow(sample_file) != 263339. Should you choose a different fy.year.of.sample.file?")
             }
           },
           "2015-16" = {
             if (nrow(sample_file) != 269639) {
               warning("nrow(sample_file) != 269639. Should you choose a different fy.year.of.sample.file?")
             }
           },
           "2016-17" = {
             if (nrow(sample_file) != 277202) {
               warning("nrow(sample_file) != 277202. Should you choose a different fy.year.of.sample.file?")
             }
           },
           "2017-18" = {
             if (nrow(sample_file) != 284925) {
               warning("nrow(sample_file) != 284925. Should you choose a different fy.year.of.sample.file?")
             }
           },
           stop("`fy.year.of.sample.file` must be '2012-13', '2013-14', '2014-15', '2015-16', '2016-17',
                or '2017-18'."))
  }
  
  if (h == 0) {
    return(sample_file)
  }
  
  if ("WEIGHT" %notin% names(sample_file)) {
    sample_file[, "WEIGHT" := list(WEIGHT)]
  }
  
  if (fy.year.of.sample.file == "2015-16") {
    if ("Med_Exp_TO_amt" %chin% names(sample_file)) {
      
    } else {
      sample_file[, Med_Exp_TO_amt := 0]
    }
  }
  
  
  # NSE e.g inflators[h == h]
  H <- h
  current.fy <- fy.year.of.sample.file
  
  to.fy <- yr2fy(fy2yr(current.fy) + h)
  
  if (is.null(wage.series)){
    wage.inflator <- wage_inflator(1, from_fy = current.fy, to_fy = to.fy)
  } else {
    wage.inflator <- wage_inflator(1, from_fy = current.fy, to_fy = to.fy, 
                                   forecast.series = "custom", wage.series = wage.series)
  }
  
  if (is.null(lf.series)) {
    lf.inflator <- lf_inflator_fy(from_fy = current.fy, to_fy = to.fy)
  } else {
    lf.inflator <- lf_inflator_fy(from_fy = current.fy, to_fy = to.fy, 
                                  forecast.series = "custom", 
                                  lf.series = lf.series)
  }
  
  cpi.inflator <- cpi_inflator(1, from_fy = current.fy, to_fy = to.fy)
  
  if (!is.logical(.recalculate.inflators)) {
    stop("`.recalculate.inflators` was type ", typeof(.recalculate.inflators), ", ",
         "but must be TRUE, FALSE, or NA.")
  }
  if (length(.recalculate.inflators) != 1L) {
    stop("`length(.recalculate.inflators) = ", length(.recalculate.inflators), ", 
         but must be length-one.")
  }
  
  if (is.na(.recalculate.inflators)) {
    .recalculate.inflators <- current.fy >= "2017-18"
  }
  
  if (.recalculate.inflators) {
    CG.inflator <- CG_inflator(1, from_fy = current.fy, to_fy = to.fy)
  } else {
    if (current.fy %notin% c("2012-13", "2013-14", 
                             "2014-15", "2015-16", "2016-17")) {
      stop("Precalculated inflators only available when projecting from ",
           "2012-13, 2013-14, 2014-15, 2015-16, or 2016-17.")
    } else {
      cg_inflators <- 
        switch(current.fy, 
               "2012-13" = cg_inflators_1213, 
               "2013-14" = cg_inflators_1314,
               "2014-15" = cg_inflators_1415,
               "2015-16" = cg_inflators_1516,
               "2016-17" = cg_inflators_1617)
      stopifnot("forecast.series" %in% names(cg_inflators))
      forecast.series <- NULL 
      CG.inflator <- 
        cg_inflators[.(to.fy, forecast.dots$estimator)] %>%
        .subset2("cg_inflator")
    } 
  }
  
  col.names <- names(sample_file)
  
  # Differential uprating not available for years outside:
  if (is.na(differentially_uprate_Sw)) {
    differentially_uprate_Sw <- fy.year.of.sample.file <= "2016-17"
  }
  
  diff.uprate.wagey.cols <- 
    if (differentially_uprate_Sw) {
      "Sw_amt"
    } else {
      character(0L)
    }
  
  wagey.cols <- c(if (!differentially_uprate_Sw) "Sw_amt",
                  "Alow_ben_amt",
                  "ETP_txbl_amt",
                  "Rptbl_Empr_spr_cont_amt", 
                  "Non_emp_spr_amt", 
                  "MCS_Emplr_Contr", 
                  "MCS_Prsnl_Contr", 
                  "MCS_Othr_Contr")
  
  
  
  super.bal.col <- c("MCS_Ttl_Acnt_Bal")
  
  lfy.cols <- c("WEIGHT")
  
  cpiy.cols <- c(grep("WRE", col.names, value = TRUE), # work-related expenses
                 "Cost_tax_affairs_amt",
                 "Other_Ded_amt")
  
  derived.cols <- c("Net_rent_amt",
                    "Net_PP_BI_amt",
                    "Net_NPP_BI_amt",
                    "Tot_inc_amt",
                    "Tot_ded_amt",
                    "Taxable_Income")
  
  CGTy.cols <- c("Net_CG_amt", "Tot_CY_CG_amt")
  
  # names(taxstats::sample_file_1314)
  alien.cols <- 
    col.names[!col.names %chin% c("Ind", "Gender", "age_range", "Occ_code", "Partner_status", 
                                  "Region", "Lodgment_method", "PHI_Ind", "Sw_amt", "Alow_ben_amt", 
                                  "ETP_txbl_amt", "Grs_int_amt", "Aust_govt_pnsn_allw_amt", "Unfranked_Div_amt", 
                                  "Frk_Div_amt", "Dividends_franking_cr_amt", "Net_rent_amt", "Gross_rent_amt", 
                                  "Other_rent_ded_amt", "Rent_int_ded_amt", "Rent_cap_wks_amt", 
                                  "Net_farm_management_amt", "Net_PP_BI_amt", "Net_NPP_BI_amt", 
                                  "Total_PP_BI_amt", "Total_NPP_BI_amt", "Total_PP_BE_amt", "Total_NPP_BE_amt", 
                                  "Net_CG_amt", "Tot_CY_CG_amt", "Net_PT_PP_dsn", "Net_PT_NPP_dsn", 
                                  "Taxed_othr_pnsn_amt", "Untaxed_othr_pnsn_amt", "Other_foreign_inc_amt", 
                                  "Other_inc_amt", "Tot_inc_amt", "WRE_car_amt", "WRE_trvl_amt", 
                                  "WRE_uniform_amt", "WRE_self_amt", "WRE_other_amt", "Div_Ded_amt", 
                                  "Intrst_Ded_amt", "Gift_amt", "Non_emp_spr_amt", "Cost_tax_affairs_amt", 
                                  "Other_Ded_amt", "Tot_ded_amt", "PP_loss_claimed", "NPP_loss_claimed", 
                                  "Rep_frng_ben_amt", "Med_Exp_TO_amt", "Asbl_forgn_source_incm_amt", 
                                  "Spouse_adjusted_taxable_inc", "Net_fincl_invstmt_lss_amt", "Rptbl_Empr_spr_cont_amt", 
                                  "Cr_PAYG_ITI_amt", "TFN_amts_wheld_gr_intst_amt", "TFN_amts_wheld_divs_amt", 
                                  "Hrs_to_prepare_BPI_cnt", "Taxable_Income", "Help_debt", "MCS_Emplr_Contr", 
                                  "MCS_Prsnl_Contr", "MCS_Othr_Contr", "MCS_Ttl_Acnt_Bal",
                                  "WEIGHT")]
  Not.Inflated <- c("Ind", 
                    "Gender",
                    "age_range", 
                    "Occ_code", 
                    "Partner_status", 
                    "Region", 
                    "Lodgment_method", 
                    "PHI_Ind",
                    derived.cols,
                    alien.cols)
  
  
  Not.Inflated <- c(Not.Inflated, excl_vars)
  
  generic.cols <- 
    col.names[col.names %notchin% c(diff.uprate.wagey.cols, 
                                    wagey.cols,
                                    super.bal.col,
                                    lfy.cols,
                                    cpiy.cols,
                                    derived.cols,
                                    Not.Inflated)]
  
  if (.recalculate.inflators) {
    generic.inflators <- 
      generic_inflator(vars = generic.cols,
                       h = h,
                       fy.year.of.sample.file = fy.year.of.sample.file, 
                       estimator = forecast.dots$estimator,
                       pred_interval = forecast.dots$pred_interval)
  } else {
    generic.inflators <- 
      switch(current.fy, 
             "2012-13" = generic_inflators_1213, 
             "2013-14" = generic_inflators_1314, 
             "2014-15" = generic_inflators_1415, 
             "2015-16" = generic_inflators_1516, 
             "2016-17" = generic_inflators_1617, 
             stop("Precalculated inflators only available when projecting from ",
                  "2012-13, 2013-14, 2014-15, 2015-16, and 2016-17."))
  }
  
  ## Inflate:
  
  # Function to determine which inflator to use, given the column name
  inflator_switch <- function(nom) {
    if (nom %chin% diff.uprate.wagey.cols) {
      return("differential")
    }
    if (nom %chin% wagey.cols) {
      return("wage")
    }
    if (nom %chin% cpiy.cols) {
      return("cpi")
    }
    if (nom %chin% lfy.cols) {
      return("labour-force")
    }
    if (nom %chin% CGTy.cols) {
      return("cgt")
    }
    if (nom %chin% generic.cols) {
      return("generic")
    }
    if (nom %chin% super.bal.col) {
      return("super")
    }
    stop("Internal error: inflator-switch unmatched. ", # nocov
         "`nom = ", nom, "`") # nocov
  }
  
  if (use_age_pop_forecast) {
    Population <- NULL
    pop_inflator_by_age <-
      g_pop_forecasts_by_age_range[CJ(age_range = 0:11,
                                      fy = c(current.fy, to.fy)),
                                   on = c("age_range", "fy")] %>%
      .[, .(pop_forecast_r = last(Population) / first(Population)),
        keyby = "age_range"]
    sample_file <- sample_file[pop_inflator_by_age, on = "age_range"]
    # Will expand to nrow
    lf.inflator <- {lf.inflator + .subset2(sample_file, "pop_forecast_r")} / 2
  }
  
  if (!is.numeric(r_super_balance)) {
    stop("`r_super_balance` was type ", typeof(r_super_balance), " but must be
         numeric.")
  }
  if (length(r_super_balance) != 1L && length(r_super_balance) != nrow(sample_file)) {
    stop("`length(r_super_balance) = ", length(r_super_balance), "`, but must be 
         length-one or have the same length as the number of rows of `sample_file = ",
         nrow(sample_file), "`.")
  }

  for (j in col.names) {
    if (j %chin% Not.Inflated) {
      next
    }
    v <- .subset2(sample_file, j)
    v_new <- 
      switch(inflator_switch(j),
             "differential" = {
               if (is.null(wage.series)) {
                 differentially_uprate_wage(v, 
                                            from_fy = current.fy,
                                            to_fy = to.fy)
               } else {
                 differentially_uprate_wage(v, 
                                            from_fy = current.fy,
                                            to_fy = to.fy,
                                            forecast.series = "custom",
                                            wage.series = wage.series)
               }
             },
             "wage" = {
               wage.inflator * v
             },
             "cpi" = {
               cpi.inflator * v
             },
             "labour-force" = {
               lf.inflator * v
             },
             "cgt" = {
               CG.inflator * v
             },
             "generic" = {
               if (.recalculate.inflators) {
                 generic.inflators[variable == j]$inflator * v
               } else {
                 generic.inflators[.(H, j), inflator] * v
               }
             },
             "super" = {
               {r_super_balance ^ h} * v
             },
             # nocov start
             stop("Internal error: switch(inflator_switch()) not matched\n\t", 
                  "inflator_switch(j) = ", "inflator_switch(", j, ") = ", 
                  inflator_switch(j)))
    # nocov end
    set(sample_file, 
        j = j,
        value = v_new)
  }
  
  sample_file %>%
    .[, Net_rent_amt := Gross_rent_amt - Other_rent_ded_amt - Rent_int_ded_amt - Rent_cap_wks_amt] %>%
    .[, Net_PP_BI_amt := Total_PP_BI_amt - Total_PP_BE_amt] %>%
    .[, Net_NPP_BI_amt := Total_NPP_BI_amt - Total_NPP_BE_amt] %>%
    .[, Tot_inc_amt := .add(Sw_amt,
                            Alow_ben_amt,
                            ETP_txbl_amt,
                            Grs_int_amt,
                            Aust_govt_pnsn_allw_amt,
                            Unfranked_Div_amt,
                            Frk_Div_amt,
                            Dividends_franking_cr_amt,
                            Net_rent_amt,
                            Net_farm_management_amt,
                            Net_PP_BI_amt,  ## Need to check exactly how this maps.
                            Net_NPP_BI_amt,
                            Net_CG_amt,  ## We cannot express this cleanly in terms of Tot_CG
                            Net_PT_PP_dsn,
                            Net_PT_NPP_dsn,
                            Taxed_othr_pnsn_amt,
                            Untaxed_othr_pnsn_amt,
                            Other_foreign_inc_amt,
                            Other_inc_amt)] %>%
    .[, Tot_ded_amt := .add(WRE_car_amt,
                            WRE_trvl_amt,
                            WRE_uniform_amt,
                            WRE_self_amt,
                            WRE_other_amt,
                            Div_Ded_amt,
                            Intrst_Ded_amt,
                            Gift_amt,
                            Non_emp_spr_amt,
                            Cost_tax_affairs_amt,
                            Other_Ded_amt)] %>%
    .[, Taxable_Income := pmaxC(Tot_inc_amt - Tot_ded_amt - PP_loss_claimed - NPP_loss_claimed, 0)] %>%
    .[]
}

