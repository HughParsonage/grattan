#' Pension Supplement
#' 
#' The Pension Supplement gets added to the max rate of payment before income reduction tests are applied. Note that if the individual is part of a couple, the rate indicates the payment amount per person, not for the couple.
#' Can be claimed by those receiving Age Pension, Carer Payment, Wife Pension, Widow B Pension, Bereavement Allowance, or Disability Support Pension (except if under 21 and have no children).
#' Can also be claimed if over age pension age and are receiving ABSTUDY, Austudy, Parenting Payment, Partner Allowance, Special Benefit, or Widow Allowance.
#' Can still claim the basic amount if single, under age pension age, and receive the Parenting Payment.
#'
#' @param has_partner Does the individual have a partner?
#' @param age The individual's age. Default is 18 years.
#' @param n_dependants How many dependant children does the individual have?
#' @param parenting_payment Is the individual receiving parenting payment?
#' @param Date Date. Default is "2016/03/01" if fy.year is not present.
#' @param fy.year Financial year. Default is "2015-16" if Date is not present.
#' @param age_pension_age_requirement Does the payment for which the pension supplement is being added to require the individual to be over age pension age to receive? i.e. one of ABSTUDY, Austudy, Parenting Payment, Partner Allowance, Special Benefit, Widow Allowance. 
#' @param disability_support_pension Is the individual receiving the disability support pension?
#' @param per How often the payment will be made. Default is fortnightly. 
#' @param overseas_absence Will the individual be living outside of Australia for more than 6 weeks of the year?
#' @param seperated_couple Is the individual part of an illness separated couple, respite care couple, or partner imprisoned?
#' 
#' @author Matthew Katzen
#' @export

#note: annual = rate multiplied by 364/14 (= 26)

pension_supplement <- function(has_partner = FALSE,
                               age = 70,
                               n_dependents = 0,
                               parenting_payment = FALSE,
                               Date = NULL,
                               fy.year = NULL,
                               age_pension_age_requirement = FALSE,
                               disability_support_pension = FALSE,
                               per = 'fortnight',
                               overseas_absence = FALSE,
                               seperated_couple = FALSE){
  
  if(!(per %in% c('fortnight', 'annual'))){
    stop("per can only take values `fortnight` or `annual`")
  }
  
  if(any(!has_partner & seperated_couple)) {
    stop("incompatible values of `has_partner` and `partner_seperated`")
  }
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      Date <- as.Date("2016/03/01")
      fy.year <- "2015-16"
      warning('`Date` and `fy.year` not set, so using `Date` = "2016/03/01" and `fy.year` = "2015-16"')
    } else {
      Date <- fy2date(fy.year)
      warning('`Date` not set. Using date as defined in fy2date()')
    }
  } else {
    if (is.null(fy.year)) {
      fy.year <- date2fy(Date)
    } else {
      warning("`fy.year` and `Date` both used. Ignoring `fy.year`.")
    }
  }
  
  input <- data.table(do.call(cbind.data.frame, mget(ls()))) #convert arguments to data table
  
  
  input[ ,age_pension_age :=  #increase:https://www.humanservices.gov.au/individuals/services/centrelink/age-pension/eligibility-payment-rates/age-rules
                              #women increase before 2014 (not implemented): https://www.humanservices.gov.au/sites/default/files/documents/co029-0907.pdf
    if_else(Date < "2017-07-01",
            65,
            if_else(Date < "2019-01-01",
                    65.5,
                    if_else(Date < "2021-07-01",
                            66,
                            if_else(Date < "2023-07-01",
                                    66.5,
                                    67))))]

  input[, eligible := if_else(age_pension_age_requirement,
                              age >= age_pension_age,
                              if_else(disability_support_pension,
                                      !(age < 21 & n_dependents == 0),#ineligble if under 21 and have no kids
                                      TRUE))]

  input[ ,max_rate_March_2016 :=
    if_else(has_partner & !seperated_couple,
            49,
            65)]

  input[, basic_rate_March_2016 :=
    if_else(has_partner,
            18.7,
            22.70)]

  output <- input[, if_else(eligible,
                            if_else((!has_partner & parenting_payment) | overseas_absence,
                                    if_else(per == 'fortnight',
                                            basic_rate_March_2016,
                                            basic_rate_March_2016 * 26),
                                    if_else(per == 'fortnight',
                                            max_rate_March_2016,
                                            max_rate_March_2016 * 26)),
                            0)]
  output
}
