#' Youth allowance
#' 
#' @param ordinary_income Fortnightly individual income. Defualt is zero.
#' @param fy.year Financial year. Default is current financial year.
#' @param age The individual's age. Default is 18 years.
#' @param eligible_if_over22 To be eligible for Youth Allowance while over 22, recipients must either commence full-time study or an Australian apprenticeship having been in receipt of an income support payment for at least 6 out of the last 9 months since turning 22, or study an approved course in English where English is not their first language.
#' @param is_single Is the individual single?
#' @param living_at_home Does the individual live at home with their parents?
#' @param has_children Does the individual have any children?
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal carer, either of large family or foster child/ren, or who is a home or distance educator of child/ren?
#' @param is_student Is the individual a student? Note that apprenctices are considered students.
#' @param per How often the payment will be made. Default is fortnightly. At present payments can only be fortnightly.
#' @param taper1 The amount at which the payment is reduced for each dollar earned between the lower and upper bounds.
#' @param taper2 The amount at which the payment is reduced for each dollar earned above the upper bound.
#' @param FT_YA_student_lower Student and apprentice lower bound for which reduction in payment occurs at rate taper1
#' @param FT_YA_student_upper Student and apprentice upper bound for which reduction in payment occurs at rate taper1. Student and apprentice lower bound for which reduction in payment occurs at rate taper2.
#' @param FT_YA_jobseeker_lower Jobseeker lower bound for which reduction in payment occurs at rate taper1
#' @param FT_YA_jobseeker_upper Jobseeker upper bound for which reduction in payment occurs at rate taper1. Student and apprentice lower bound for which reduction in payment occurs at rate taper2.
#' @param excess_partner_income_mu The amount at which the payment is reduced for each dollar earned by the individual's partner.
#' @export youth_allowance 



youth_allowance <- function(ordinary_income = 0,
                            fy.year = yr2fy(year(Sys.Date())),
                            age = 18,
                            eligible_if_over22 = FALSE,
                            has_partner = FALSE,
                            living_at_home = FALSE,
                            n_dependants = 0L,
                            isjspceoalfofcoahodeoc = FALSE,
                            is_student = TRUE,
                            per = "fortnight",
                            taper1 = 0.5,
                            taper2 = 0.6,
                            FT_YA_student_lower = 437,
                            FT_YA_student_upper = 524,
                            FT_YA_jobseeker_lower = 143,
                            FT_YA_jobseeker_upper = 250,
                            excess_partner_income_mu = 0.6) {
  stopifnot(per == "fortnight")
  
  #max rate data: http://guides.dss.gov.au/guide-social-security-law/5/1/1/20
  #note: identical for student and jobseeker
  max_rate_March_2018 <-
    if_else(ordinary_income >= Inf,
            NA_real_,
            if_else(and(age>=22, eligible_if_over22),
                    if_else(!has_partner,
                            if_else(living_at_home,
                                    360.20,
                                    541.70),
                            if_else(n_dependants > 0L,
                                    NA_real_,
                                    489.60)),
                    if_else(!has_partner,
                            if_else(isjspceoalfofcoahodeoc,
                                    762.40,
                                    if_else(n_dependants > 0L,
                                            584.20,
                                            if_else(living_at_home,
                                                    if_else(age %between% c(16,17),
                                                            244.10,
                                                            293.60),
                                                    445.80
                                                    ))),
                            ifelse(n_dependants > 0L,
                                   489.60,
                                   445.80))))
  
  if (anyNA(max_rate_March_2018)) {
    stop("Unknown recipient status for youth_allowance() at index(es) ",
         paste(which(is.na(max_rate_March_2018)), collapse=", "),
         " . Check arguments.")
  }
  
  
  #max income data: https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
  max_income <-
    if_else(is_student,
            #student
            if_else(n_dependants > 0L,
                    if_else(!has_partner,
                            1440.50,
                            1280.34),
                    if_else(living_at_home,
                            if_else(age<18,
                                    864.84,
                                    948.50),
                            1206.17)),
            #jobseeker
            if_else(isjspceoalfofcoahodeoc,
                    1435.17,      
                    if_else(n_dependants > 0L,
                            if_else(!has_partner,
                                    1149.84,
                                    989.67),
                            if_else(living_at_home,
                                    if_else(age<18,
                                            574.18,
                                            657.84),
                                    989.67))))
            


  # Benefit test
  # http://guides.dss.gov.au/guide-social-security-law/4/2/2
  # https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
  
  
  #ordinary_income_free_area <- 
  #  cpi_inflator(100, from_fy = "2014-15", to_fy = fy.year)
  
  #ordinary_income_excess <- pmaxC(ordinary_income - ordinary_income_free_area)
  
  ###TESTS needed: https://www.humanservices.gov.au/individuals/services/centrelink/youth-allowance-students-and-australian-apprentices/how-much-you-can-get/income-and-assets-test#personalassets
    #personal income test: function ya_income_reduction()
      #https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
  
    #personal assets test: INCOMPLETE
      #http://guides.dss.gov.au/guide-social-security-law/4/2/8/30 The personal assets test for independent YA recipients is the same as the benefits assets tes
      #http://guides.dss.gov.au/guide-social-security-law/4/2/3
      #https://www.legislation.gov.au/Details/C2018C00167
  
    #partner income test: function ya_partner_reduction() INCOMPLETE
      #http://guides.dss.gov.au/guide-social-security-law/4/2/2
      #example: http://guides.dss.gov.au/guide-social-security-law/5/5/3/30
  
    #parental income test: function ya_parental_income_reduction() INCOMPLETE
      #https://www.humanservices.gov.au/individuals/services/centrelink/youth-allowance-students-and-australian-apprentices/how-much-you-can-get/income-and-assets-test#parentalincome
      #http://guides.dss.gov.au/guide-social-security-law/4/2/8/05                          
      #family tax benefits: http://guides.dss.gov.au/family-assistance-guide/3/1/1/20   
      #https://www.humanservices.gov.au/individuals/enablers/how-much-ftb-part-you-can-get/30611
  
                                       
  payment <- if_else(ordinary_income > max_income,
                     0,
                     max_rate_March_2018 - ya_income_reduction(ordinary_income, 
                                                          max_income,
                                                          is_student,
                                                          FT_YA_student_lower,
                                                          FT_YA_student_upper,
                                                          FT_YA_jobseeker_lower,
                                                          FT_YA_jobseeker_upper,
                                                          taper1,
                                                          taper2))
                     
                   
  payment
  
}

ya_parental_income_reduction <- function() {}

ya_partner_reduction <- function(ordinary_income,
                                 partner_income = 0,
                                 income_free_area,
                                 partner_benefit,
                                 partner_pension,
                                 partner_dependent_ya) {
  #social security benifit?
  
  
}


ya_income_reduction <- function(ordinary_income,
                             max_income,
                             is_student,
                             max_rate_March_2018,
                             taper1 = 0.5,
                             taper2 = 0.6,
                             FT_YA_student_lower = 437,
                             FT_YA_student_upper = 524,
                             FT_YA_jobseeker_lower = 143,
                             FT_YA_jobseeker_upper = 250) {
  if_else(is_student,
          #student
          if_else(ordinary_income<FT_YA_student_lower,
                  0,
                  if_else(ordinary_income < FT_YA_student_upper,
                          taper1 * (ordinary_income - FT_YA_student_lower),
                          if_else(ordinary_income < max_income,
                                  taper1 * (ordinary_income - FT_YA_student_lower) +
                                    (taper2 - taper1) * (ordinary_income - FT_YA_student_upper),
                                  #reduce by entire income i.e. dont get YA
                                  max_rate_March_2018))),
          #jobseeker
          if_else(ordinary_income<FT_YA_jobseeker_lower,
                  0,
                  if_else(ordinary_income < FT_YA_jobseeker_upper,
                          taper1 * (ordinary_income - FT_YA_jobseeker_lower),
                          if_else(ordinary_income < max_income,
                                  taper1 * (ordinary_income - FT_YA_jobseeker_lower) +
                                    (taper2 - taper1) * (ordinary_income - FT_YA_jobseeker_upper),
                                  #reduce by entire income i.e. dont get YA
                                  max_rate_March_2018))))
}
