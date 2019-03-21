.onLoad <- function(libname = find.package("grattan"), pkgname = "grattan") {
  
  op <- options()
  the_tempdir <- tempdir()
  
  opgrattan <- list(
    "grattan.verbose" = FALSE,
    "grattan.assume1901_2100" = TRUE,
    "grattan.taxstats.lib" = {
      if (any(grepl("taxstatslib", list.dirs(path = the_tempdir, 
                                             recursive = FALSE, 
                                             full.names = FALSE)))) {
        lib <- file.path(the_tempdir, 
                         grep("taxstatslib", 
                              list.dirs(path = the_tempdir,
                                        recursive = FALSE,
                                        full.names = FALSE),
                              value = TRUE,
                              fixed = TRUE)[1])
      } else {
        lib <- tempfile("taxstatslib", tmpdir = the_tempdir)
      }
      lib
    }
  )
  toset <- !(names(opgrattan) %in% names(op))
  if (any(toset)) options(opgrattan[toset])
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      # sample file names from taxstats
      c("Ind", "Gender", "age_range", "Occ_code", "Partner_status", 
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
    # we use the magrittr pipe
    ".",
    
    # to return
    "out",
    
    # generic.inflators
    "variable", 
    
    # CGT inflator
    "marginal_rate_first", 
    "marginal_rate_last", 
    "CGT_discount_for_individuals_and_trusts_millions", 
    "to_cg", 
    "from_cg", 
    "n_CG_to",
    "n_CG_from",
    "revenue_foregone",
    "mean_wmrL",
    "zero_discount_Net_CG_total",
    
    
    # Taxstats Table 1
    "Selected_items", 
    "fy_year",
    "Count", 
    "Sum"
    
    , 
    
    # dput(unique(c(names(grattan:::medicare_tbl), names(grattan:::sapto_tbl), names(grattan:::cgt_expenditures))))
    c("sato", "pto", "sapto", "family_status", "lower_threshold", 
      "family_income", 
      "upper_threshold", "taper", "rate", "lower_family_threshold", 
      "upper_family_threshold", "lower_up_for_each_child", "family_status_index", 
      "max_offset", "taper_rate", "source", "FY", "CGT_discount_for_individuals_and_trusts_millions", 
      "URL", "Projected"), 
    # lito_tbl
    "max_lito", "min_bracket", "lito_taper"
      )
    )
  
  tryCatch({
    f_mtimes <- 
      # Display last file changed when on local machine only
      if (identical(Sys.info()[["user"]], "hughp")) {
        if (file.exists("R/grattan-package.R")) {
          c(vapply(dir(path = "R", full.names = TRUE), file.mtime, double(1)),
            vapply(dir(path = "tests/testthat", full.names = TRUE), file.mtime, double(1)))
        } else if (file.exists(file.path(find.package("grattan"), "NAMESPACE"))) {
          sapply(file.path(find.package("grattan"), "NAMESPACE"), file.mtime)
        }
      }
    if (length(f_mtimes)) {
      class(f_mtimes) <- "POSIXct"
      the_filemtime <- f_mtimes[which.max(f_mtimes)]
      ago <- difftime(Sys.time(), the_filemtime)
      file_name <- names(the_filemtime)
      if (is.character(basename(file_name))) {
        gessage("Last change: ", basename(file_name), " at ", strftime(the_filemtime),
                " (", floor(ago), " ", attr(ago, "units"), " ago).")
      }
    }
  }, 
  error = function(e) NULL)
  
  
  invisible(NULL)
}

gessage <- function(...) {
  if (identical(Sys.info()[["user"]], "hughp") &&
      identical(.Platform$GUI, "RStudio") &&
      !isNamespaceLoaded("pkgdown") &&
      file.exists("~/grattan_1.4.0.2.tar.gz")) {
    packageStartupMessage(...)
  } else {
    NULL
  }
}

.onUnload <- function (libpath) {
  library.dynam.unload("grattan", libpath)
}
