#' Rebate income
#'
#' @param Taxable_Income The taxable income.
#' @param Rptbl_Empr_spr_cont_amt Reportable employer super contributions, excluding compulsory SG.
#' @param All_deductible_super_contr Deductible personal super contributions.
#' @param Net_fincl_invstmt_lss_amt Net financial investment loss.
#' @param Net_rent_amt Signed net rental income; a loss is negative.
#' @param Rep_frng_ben_amt Aggregate reportable fringe benefits. For modern years
#' the aggregate alone is treated as exempt-employer benefits.
#' @param exempt_fringe_benefits,nonexempt_fringe_benefits Separate IT1 amounts by
#' employer exemption status. The 2016-17 worksheet adjusts their sum. From
#' 2017-18 the adjustment applies only to exempt-employer benefits.
#' @param assessable_fhss Assessable first home super saver released amount from 2018-19.
#' @param fy.year Financial year, defaulting to the current financial year.
#' @source ITAA 1936 s6 and the ATO annual rebate-income worksheets.

rebate_income <- function(Taxable_Income,
                          Rptbl_Empr_spr_cont_amt = 0,
                          All_deductible_super_contr = 0,
                          Net_fincl_invstmt_lss_amt = 0,
                          Net_rent_amt = 0,
                          Rep_frng_ben_amt = 0,
                          fy.year = NULL, exempt_fringe_benefits = NULL,
                          nonexempt_fringe_benefits = NULL, assessable_fhss = NULL) {
  year <- fy2yr(validate_fys_permitted(fy.year %||% date2fy(Sys.Date())))
  .Call("Crebate_income", .rN(Taxable_Income, integer(length(Taxable_Income))),
        Rptbl_Empr_spr_cont_amt, All_deductible_super_contr,
        Net_fincl_invstmt_lss_amt, Net_rent_amt, Rep_frng_ben_amt,
        exempt_fringe_benefits, nonexempt_fringe_benefits, assessable_fhss, year, 1L, PACKAGE = "grattan")
}
