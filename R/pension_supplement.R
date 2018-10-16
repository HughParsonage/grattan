#' Pension Supplement
#' 
#' The Pension Supplement gets added to the max rate of payment before income reduction tests are applied. Note that if the individual is part of a couple, the rate indicates the payment amount per person, not for the couple.
#' Can be claimed by those receiving Age Pension, Carer Payment, Wife Pension, Widow B Pension, Bereavement Allowance, or Disability Support Pension (except if under 21 and have no children).
#' Can also be claimed if over age pension age and are receiving ABSTUDY, Austudy, Parenting Payment, Partner Allowance, Special Benefit, or Widow Allowance.
#' Can still claim the basic amount if single, under age pension age, and receive the Parenting Payment.
#'
#' @param has_partner Does the individual have a partner?
#' @param age The individual's age. Default is 70 years.
#' @param n_dependants How many dependant children does the individual have?
#' @param parenting_payment Is the individual receiving parenting payment?
#' @param Date Date. Default is "2016/03/01" if fy.year is not present.
#' @param fy.year Financial year. Default is "2015-16" if Date is not present.
#' @param qualifying_payment What is the payment that the supplement is being applied to? 
#' @param per How often the payment will be made. Default is to return the annual
#'  payment, with a message.
#' @param overseas_absence Will the individual be living outside of Australia 
#' for more than 6 weeks of the upcoming year?
#' @param seperated_couple Is the individual part of an illness separated couple,
#'  respite care couple, or partner imprisoned?
#' 
#' @author Matthew Katzen
#' @export

#note: annual = rate multiplied by 364/14 (= 26)

pension_supplement <- function(has_partner = FALSE,
                               age = 70,
                               n_dependants = 0,
                               parenting_payment = FALSE,
                               Date = NULL,
                               fy.year = NULL,
                               qualifying_payment = 'age_pension',
                               per = c("year", "fortnight", "quarter"),
                               overseas_absence = FALSE,
                               seperated_couple = FALSE){
  
  
  
  if (any(!has_partner & seperated_couple)) {
    stop("incompatible values of `has_partner` and `partner_seperated`")
  }
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      Date <- as.Date("2016/03/01")
      fy.year <- "2015-16"
      message('`Date` and `fy.year` not set, so using `Date` = "2016/03/01" and `fy.year` = "2015-16"')
    } else {
      Date <- fy2date(fy.year)
    }
  } else {
    if (is.null(fy.year)) {
      fy.year <- date2fy(Date)
    } 
  }
  
  # Convert date to DATE format
  Date <- as.Date(Date)
  
  # Convert arguments to data table
  ls_np <- ls()[ls() != "per"]
  input <- data.table(do.call(cbind.data.frame, mget(ls_np))) 
  
  eligible <- NULL
  input[, eligible := if_else(qualifying_payment %in% c('abstudy', 'austudy', 'parenting_payment', 'partner_allowance', 'special_benefit', 'widow_allowance'),
                              age >= age_pension_age(Date), # must be over age pension age if receiving one of above payments
                              if_else(qualifying_payment == 'disability_support_pension',
                                      !(age < 21 & n_dependants == 0), # ineligble if under 21 and have no kids
                                      qualifying_payment %in% c('age_pension', 'carer_payment', 'wife_pension', 'widow_b_pension', 'bereavement_allowance')))] # eligible for these payments
        
  max_rate_March_2016 <- NULL
  input[, max_rate_March_2016 :=
          if_else(has_partner & !seperated_couple,
                  49,
                  65)]
  
  basic_rate_March_2016 <- NULL
  input[, basic_rate_March_2016 := if_else(has_partner, 18.7, 22.70)]
  
  res <- 
    input[, if_else(eligible,
                    if_else((!has_partner & parenting_payment) | overseas_absence,
                            basic_rate_March_2016 * 26,
                            max_rate_March_2016 * 26),
                    0)]
  res / validate_per(per, missing(per))
}
