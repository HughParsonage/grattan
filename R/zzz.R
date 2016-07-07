.onLoad <- function(libname = find.package("grattan"), pkgname = "grattan"){
  op <- options()
  op.grattan <- list(
    grattan.bigplot = TRUE
  )
  toset <- !(names(op.grattan) %in% names(op))
  if(any(toset)) options(op.grattan[toset])
  
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
    
    ))
  invisible()
}
