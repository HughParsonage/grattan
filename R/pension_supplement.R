#' Pension Supplement
#' 
#' Supplementary payment for those receiving Age Pension, Carer Payment, Wife Pension, Widow B Pension, Bereavement Allowance, Disability Support Pension (except if under 21 and no kids)
#' Can also receive if over age pension age and  receiving ABSTUDY, Austudy, Parenting Payment, Partner Allowance, Special Benefit, or, Widow Allowance

pension_supplement <- function(has_partner = FALSE,
                               n_dependants = 0){
  max_rate_March_2016 <-
    if_else(has_partner)
}