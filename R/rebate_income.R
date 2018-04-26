#' Rebate income
#' 
#' @param Taxable_Income the taxable income
#' @param Rptbl_Empr_spr_cont_amt The reportable employer superannuation contributions amount
#' @param All_deductible_super_contr deductible personal superannuation contributions
#' @param Net_fincl_invstmt_lss_amt Net financial investment loss
#' @param Net_rent_amt (for Rental deductions)
#' @param Rep_frng_ben_amt Reportable fringe-benefits
#' @source \url{https://www.ato.gov.au/Individuals/Tax-return/2015/Tax-return/Tax-offset-questions-T1-T2/Rebate-income-2015/}

rebate_income <- function(Taxable_Income, 
                          Rptbl_Empr_spr_cont_amt = 0,
                          All_deductible_super_contr = 0,
                          Net_fincl_invstmt_lss_amt = 0, 
                          Net_rent_amt = 0, 
                          Rep_frng_ben_amt = 0) {
  Taxable_Income +
    Rptbl_Empr_spr_cont_amt +
    All_deductible_super_contr +
    Net_fincl_invstmt_lss_amt -
    pminC(Net_rent_amt, 0) +
    floor(Rep_frng_ben_amt * 0.51)
}
