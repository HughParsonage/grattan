#' Pension Supplement
#' 
#' Gets added to max rate of payment before income reduction tests are applied
#' Supplementary payment for those receiving Age Pension, Carer Payment, Wife Pension, Widow B Pension, Bereavement Allowance, Disability Support Pension (except if under 21 and no kids)
#' Can also receive if over age pension age and  receiving ABSTUDY, Austudy, Parenting Payment, Partner Allowance, Special Benefit, or, Widow Allowance
#' Can still receive the basic amount if single, under age pension age, and receive the Parenting Payment
#' @export

#note: adjusted using 364/14 = 26

pension_supplement <- function(has_partner = FALSE,
                               age = 70,
                               parenting_payment = FALSE,
                               Date = NULL,
                               fy.year = NULL,
                               age_pension_age_requirement = FALSE,
                               per = 'fortnight',
                               overseas_absence = FALSE){
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      Date <- "2016/03/01"
      warning('`Date` and `fy.year` not set, so using `Date` = "2016/03/01" and `fy.year` = "2015-16"')
    } else {
      Date <- fy2date(fy.year)
    }
  } else {
    if (!is.null(fy.year)) {
      warning("`fy.year` and `Date` both used. Ignoring `fy.year`.")
    }
  }
  
  age_pension_age <-#increase:https://www.humanservices.gov.au/individuals/services/centrelink/age-pension/eligibility-payment-rates/age-rules
                      #women increase before 2014 (not implemented): https://www.humanservices.gov.au/sites/default/files/documents/co029-0907.pdf
    if_else(Date < "2017-07-01",
            65,
            if_else(Date < "2019-01-01",
                    65.5,
                    if_else(Date < "2021-07-01",
                            66,
                            if_else(Date < "2023-07-01",
                                    66.5,
                                    67))))
  
  eligible <- if_else(age_pension_age_requirement,
                      age > age_pension_age,
                      TRUE)
  
  max_rate_March_2016 <-
    if_else(has_partner,
            49,
            65)
  #not exactly sure how min rate works. Think it is the minimum payment one can receive before the individual becomes ineligible due to income reduction tests and therefore isn't used explicitly
  # min_rate_March_2016 <-
  #   if_else(has_partner,
  #           52.60,
  #           34.90)
  basic_rate_March_2016 <-
    if_else(has_partner,
            18.7,
            22.70)
  
  output <- if_else(eligible,
                    if_else((!has_partner & parenting_payment) | overseas_absence,
                            basic_rate_March_2016,
                            max_rate_March_2016),
                    0)
  output
}
