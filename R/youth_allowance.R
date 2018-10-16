#' Youth allowance
#' 
#' @param fortnightly_income,annual_income Individual's income. Default is zero. 
#' You may provided both; providing both when the ratio is not 26 is an error.
#' @param fy.year Financial year. Default is current financial year.
#' @param include_ES (logical, default: \code{TRUE}) If \code{FALSE} do not
#'  include the energy supplement.
#' @param age The individual's age. Default is 18 years. If type double will be
#' coerced to integer via truncation (i.e. 17.9 becomes 17).
#' @param eligible_if_over22 To be eligible for Youth Allowance while over 22, 
#' recipients must either commence full-time study or an Australian 
#' apprenticeship having been in receipt of an income support payment for at 
#' least 6 out of the last 9 months since turning 22, or study an approved 
#' course in English where English is not their first language.
#' @param has_partner Does the individual have a partner?
#' @param lives_at_home Does the individual live at home with their parents?
#' @param n_dependants How many dependant children does the individual have?
#' @param isjspceoalfofcoahodeoc Is the recipient a single job seeker principal
#'  carer, either of large family or foster child/ren, or who is a home or 
#'  distance educator of child/ren?
#' @param is_student Is the individual a student? Note that apprentices are 
#' considered students.
#' @param per How often the payment will be made. Default is fortnightly. At 
#' present payments can only be fortnightly.
#' @param max_rate If not \code{NULL}, a length-1 double representing the 
#' maximum \emph{fortnightly} rate for youth allowance.
#' @param es If not \code{NULL}, a length-1 double as the energy supplement.
#' @param taper1 The amount at which the payment is reduced for each dollar 
#' earned between the lower and upper bounds.
#' @param taper2 The amount at which the payment is reduced for each dollar
#'  earned above the upper bound.
#' @param FT_YA_student_lower Student and apprentice lower bound for which 
#' reduction in payment occurs at rate \code{taper1}.
#' @param FT_YA_student_upper Student and apprentice upper bound for which 
#' reduction in payment occurs at rate \code{taper1}. Student and apprentice
#'  lower bound for which reduction in payment occurs at rate taper2.
#' @param FT_YA_jobseeker_lower Jobseeker lower bound for which reduction in
#'  payment occurs at rate taper1
#' @param FT_YA_jobseeker_upper Jobseeker upper bound for which reduction in 
#' payment occurs at rate taper1. Student and apprentice lower bound for which 
#' reduction in payment occurs at rate \code{taper2}.
#' @param partner_fortnightly_income The partner's fortnightly income (or zero 
#' if no partner).
#' @param partner_is_pensioner (logical, default: \code{FALSE}) Is the 
#' individual's partner in receipt of a \emph{pension} (or benefit)? 
#' @param partner_taper The amount by which the payment is reduced for each 
#' dollar earned by the individual's partner. (See \url{http://guides.dss.gov.au/guide-social-security-law/4/2/8/40}.)
#' @export youth_allowance 


youth_allowance <- function(fortnightly_income = 0,
                            annual_income = 0,
                            fy.year = NULL,
                            include_ES = TRUE,
                            age = 18L,
                            eligible_if_over22 = FALSE,
                            has_partner = FALSE,
                            lives_at_home = FALSE,
                            n_dependants = 0L,
                            isjspceoalfofcoahodeoc = FALSE,
                            is_student = TRUE,
                            per = c("fortnight", "year"),
                            
                            max_rate = NULL,
                            es = NULL,
                            taper1 = NULL,
                            taper2 = NULL,
                            FT_YA_student_lower = NULL,
                            FT_YA_student_upper = NULL,
                            FT_YA_jobseeker_lower = NULL,
                            FT_YA_jobseeker_upper = NULL,
                            partner_fortnightly_income = 0,
                            partner_is_pensioner = FALSE,
                            partner_taper = 0.6) {
  
  args <- ls(sorted = FALSE)  # sorted = FALSE => in order of args
  
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(fortnightly_income,
                                        annual_income,
                                        age,
                                        eligible_if_over22,
                                        has_partner,
                                        lives_at_home,
                                        n_dependants,
                                        isjspceoalfofcoahodeoc,
                                        is_student,
                                        partner_fortnightly_income,
                                        partner_is_pensioner)
  for (arg in args) {
    if (anyNA(get(arg, inherits = FALSE))) {
      stop("`", arg, "` contains missing values. Impute these values.")
    }
  }
  
  
  
  if (missing(annual_income)) {
    ordinary_income <- fortnightly_income
  } else {
    if (missing(fortnightly_income)) {
      ordinary_income <- annual_income / 26
    } else {
      if (max(abs(fortnightly_income - annual_income / 26)) > .Machine$double.eps^0.5) {
        i <- which.max(abs(fortnightly_income - annual_income / 26))
        stop("`fortnightly_income` and `annual_income` both provided but ",
             "don't agree. \n\n\t", 
             "i \tfortnightly_income\tannual_income\n\t", 
             "--\t------------------\t-------------\n\t", 
             i,
             "\t", formatC(fortnightly_income[i], width = 18),
             "\t", formatC(annual_income[i], width = 13), "\n\n",
             "Either provide one (and not the other) or ensure that `annual_income = 26 * fortnightly_income`.")
      }
      ordinary_income <- fortnightly_income
    }
  }
  
  if (is.null(fy.year)) {
    fy.year <- date2fy(Sys.Date())
    message('`fy.year` not set, so using `fy.year = "', fy.year, '".')
  } else {
    if (length(fy.year) != 1L && length(fy.year) != max.length) {
      if (max.length == 1L) {
        max.length <- length(fy.year)
      } else {
        the_lengths <-
          vapply(autonamed_list(fortnightly_income,
                                annual_income,
                                age,
                                eligible_if_over22,
                                has_partner,
                                lives_at_home,
                                n_dependants,
                                isjspceoalfofcoahodeoc,
                                is_student),
                 length, 
                 0L)
        
        stop("`fy.year` had length ", length(fy.year),
             ", yet `",
             names(the_lengths)[which.max(the_lengths)],
             "` had length ", max.length, ". ",
             "The only permitted lengths are 1 or the maximum length of the inputs.")
      }
    }
    
    fy.year <- validate_fys_permitted(fy.year)
  }
  
  
  if (length(fy.year) == 1L) {
    rates <- youth_annual_rates[.(fy.year)]
    tests <- youth_income_tests[.(c(FALSE, TRUE), fy.year)]
  } else {
    if (!is.null(max_rate)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `max_rate` is not NULL. ",
           "If using `max_rate` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    if (!is.null(taper1)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `taper1` is not NULL. ",
           "If using `taper1` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    if (!is.null(taper2)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `taper2` is not NULL. ",
           "If using `taper2` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    if (!is.null(FT_YA_student_lower)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `FT_YA_student_lower` is not NULL. ",
           "If using `FY_YA_student_lower` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    if (!is.null(FT_YA_student_upper)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `FT_YA_student_upper` is not NULL. ",
           "If using `FT_YA_student_upper` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    if (!is.null(FT_YA_jobseeker_lower)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `FT_YA_jobseeker_lower` is not NULL. ",
           "If using `FT_YA_jobseeker_lower` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    if (!is.null(FT_YA_jobseeker_upper)) {
      stop("`fy.year` has length ", length(fy.year),
           " but `FT_YA_jobseeker_upper` is not NULL. ",
           "If using `FT_YA_jobseeker_upper` to manually ", 
           "set the threshold, `fy.year` must be a single ", 
           "financial year.")
    }
    
    rates <- youth_annual_rates
    tests <- youth_income_tests
  }
  
  if (!is.numeric(partner_fortnightly_income)) {
    stop("`partner_fortnightly_income` was class ", 
         class(partner_fortnightly_income), 
         ", but must be a numeric vector. ",
         "Ensure `partner_fortnightly_income` is a numeric vector of length-1 ",
         "or the length of the longest input.")
  }
  
  
  if (any(partner_fortnightly_income[!has_partner] != 0)) {
    first_bad <- which.max(and(partner_fortnightly_income > 0, !has_partner))
    stop("`partner_fortnightly_income` was greater than zero at position ",
         first_bad, " yet ",
         "`has_partner[", first_bad, "]` is FALSE. ", 
         "Ensure the entries of `partner_fortnightly_income` and `has_partner` ", 
         "agree in this respect.")
  }
  
  
  income <- NULL
  isStudent <- NULL
  fy_year <- NULL
  hasDependant <- NULL
  hasPartner <- NULL
  LivesAtHome <- NULL
  Age16or17 <- NULL
  Age <- NULL
  partnerIsPensioner <- NULL
  partnerIncome <- NULL
  isParentingPayment <- NULL
  
  MBR <- ES <- NULL
  
  input <- data.table(income = as.double(ordinary_income),
                      isStudent = is_student,
                      fy_year = fy.year,
                      hasDependant = n_dependants > 0L,
                      hasPartner = has_partner,
                      LivesAtHome = lives_at_home,
                      Age16or17 = and(or(as.integer(age) == 16L,
                                         as.integer(age) == 17L),
                                      and(n_dependants == 0L,
                                          has_partner & lives_at_home)),
                      Age = age,  # necessary for partner calculations
                      partnerIsPensioner = partner_is_pensioner,
                      partnerIncome = partner_fortnightly_income,
                      isParentingPayment = isjspceoalfofcoahodeoc)
  
  
  # ## 4.2.8.40 
  # The partner income test applies where an independent YA recipient is 
  # a member of a couple (1.1.M.120). A recipient's rate of YA is reduced 
  # by 60 cents for each dollar of their partner's ordinary income (1.1.O.30)
  # that exceeds the partner income free area. The partner income free 
  # area is the same as for the benefits income test.
  #
  # Exception: If the recipient's partner is receiving a pension, both
  #   partners income is added together. Each partner is then assessed 
  #   on half the combined income.
  # 
  # source: http://guides.dss.gov.au/guide-social-security-law/4/2/8/40
  input[, income := if_else(hasPartner & partnerIsPensioner,
                            (income + partnerIncome) / 2,
                            income)]
  
  
  on.exit(options(datatable.auto.index = getOption("datatable.auto.index")))
  options(datatable.auto.index = FALSE)
  
  ok <- NULL
  IncomeThreshold_1 <- NULL
  IncomeThreshold_2 <- NULL
  taper_1 <- NULL
  taper_2 <- NULL
  
  tests_rates <-
    input %>%
    .[rates, 
      on = c("fy_year",
             "hasDependant",
             "hasPartner",
             "LivesAtHome",
             "Age16or17"),
      nomatch=0L] %>%
    .[tests, 
      on = c("isStudent",
             "fy_year"),
      nomatch=0L] %>%
    .[, out := 0] %>%
    .[, ok := (eligible_if_over22 | age <= 22)] %>%
    setindexv("ok") %>%
    .[]
  
  
  if (any(isjspceoalfofcoahodeoc)) {
    i.MBR <- NULL
    tests_rates <-
      parenting_payment_by_fy[tests_rates,
                              on = c("isParentingPayment", "fy_year"),
                              nomatch = 0L] %>%
      .[, MBR := coalesce(MBR, i.MBR)] %>%
      .[, i.MBR := NULL]
  }
  
  
  MaxRate <- NULL
  if (include_ES) {
    tests_rates[, "MaxRate" := MBR + ES] 
  } else {
    tests_rates[, "MaxRate" := MBR + 0] 
  }
  if (!is.null(max_rate)) {
    if (include_ES) {
      if (!is.null(es)) {
        set(tests_rates, j = "MaxRate", value = max_rate + es)
      } else {
        tests_rates[, MaxRate := max_rate + ES]
      }
    } else {
      set(tests_rates, j = "MaxRate", value = max_rate)
    }
  }
  
  if (!is.null(taper1)) {
    set(tests_rates, j = "taper_1", value = taper1)
  }
  if (!is.null(taper2)) {
    set(tests_rates, j = "taper_2", value = taper2)
  }
  
  if (!is.null(FT_YA_student_lower) && !is.null(FT_YA_jobseeker_lower)) {
    tests_rates[, IncomeThreshold_1 := if_else(isStudent, 
                                                FT_YA_student_lower,
                                                FT_YA_jobseeker_lower)]
  } else if (!is.null(FT_YA_student_lower)) {
    tests_rates[(isStudent), IncomeThreshold_1 := FT_YA_student_lower]
  } else if (!is.null(FT_YA_jobseeker_lower)) {
    tests_rates[(!isStudent), IncomeThreshold_1 := FT_YA_jobseeker_lower]
  }
  
  if (!is.null(FT_YA_student_upper) && !is.null(FT_YA_jobseeker_upper)) {
    tests_rates[, IncomeThreshold_2 := if_else(isStudent, 
                                                FT_YA_student_upper,
                                                FT_YA_jobseeker_upper)]
  } else if (!is.null(FT_YA_student_upper)) {
    tests_rates[(isStudent), IncomeThreshold_2 := FT_YA_student_upper]
  } else if (!is.null(FT_YA_jobseeker_upper)) {
    tests_rates[(!isStudent), IncomeThreshold_2 := FT_YA_jobseeker_upper]
  }
  
  incom2 <- NULL
  
  tests_rates %>%
    # incom2:
    #   the top threshold or the income, whichever is the smallest
    .[, incom2 := pminV(income, IncomeThreshold_2)] %>%
    .[(ok), out := MaxRate] %>%
    .[(ok), out := out - taper_1 * pmax0(incom2 - IncomeThreshold_1)] %>%
    .[(ok), out := out - taper_2 * pmax0(income - IncomeThreshold_2)]
  
  if (any(partner_is_pensioner)) {
    pifabfsa <- partner_income_free_area_by_fy_student_age
    partner_income_free_area <- NULL
    
    tests_rates <-
      # ## 4.2.8.40 
      # The partner income test applies where an independent YA recipient is 
      # a member of a couple (1.1.M.120). A recipient's rate of YA is reduced 
      # by 60 cents for each dollar of their partner's ordinary income (1.1.O.30)
      # that exceeds the partner income free area. The partner income free 
      # area is the same as for the benefits income test.
      pifabfsa[tests_rates,
               on = c("fy_year",
                      "partnerIsPensioner",
                      "Age"),
               roll = TRUE,
               nomatch = 0L,
               mult = "first"] %>%
      .[hasPartner & !partnerIsPensioner,
        out := out - partner_taper * pmax0(partner_fortnightly_income - partner_income_free_area)]
  }
  
  
  pmax0(.subset2(tests_rates, "out")) * 26 / validate_per(per, missing(per))
}


# .youth_allowance <- function(fortnightly_income = 0,
#                             annual_income = 0,
#                             fy.year = date2fy(Sys.Date()),
#                             age = 18,
#                             eligible_if_over22 = FALSE,
#                             has_partner = FALSE,
#                             living_at_home = FALSE,
#                             n_dependants = 0L,
#                             isjspceoalfofcoahodeoc = FALSE,
#                             is_student = TRUE,
#                             per = "fortnight",
#                             taper1 = 0.5,
#                             taper2 = 0.6,
#                             FT_YA_student_lower = 437,
#                             FT_YA_student_upper = 524,
#                             FT_YA_jobseeker_lower = 143,
#                             FT_YA_jobseeker_upper = 250,
#                             excess_partner_income_mu = 0.6) {
#   
#   input <- data.table(do.call(cbind.data.frame, mget(ls())))
#   
#   # max rate data: http://guides.dss.gov.au/guide-social-security-law/5/1/1/20
#   # note: identical for student and jobseeker
#   max_rate_March_2018 <-
#     input[, if_else(and(age >= 22, eligible_if_over22),
#                     if_else(!has_partner,
#                             if_else(living_at_home,
#                                     360.20,
#                                     541.70),
#                             if_else(n_dependants > 0L,
#                                     NA_real_,
#                                     489.60)),
#                     if_else(!has_partner,
#                             if_else(isjspceoalfofcoahodeoc,
#                                     762.40,
#                                     if_else(n_dependants > 0L,
#                                             584.20,
#                                             if_else(living_at_home,
#                                                     if_else(age %between% c(16, 17),
#                                                             244.10,
#                                                             293.60),
#                                                     445.80))),
#                             if_else(n_dependants > 0L,
#                                     489.60,
#                                     445.80)))]
#   
#   if (anyNA(max_rate_March_2018)) {
#     stop("Unknown recipient status for youth_allowance() at index(es) ",
#          paste(which(is.na(max_rate_March_2018)), collapse=", "),
#          " . Check arguments.")
#   }
#   
#   
#   # max income data: https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
#   max_income_March_2018 <-
#     input[, if_else(is_student,
#                     # student
#                     if_else(n_dependants > 0L,
#                             if_else(has_partner,
#                                     1280.34,
#                                     1440.50),
#                             if_else(living_at_home,
#                                     if_else(age < 18,
#                                             864.84,
#                                             948.50),
#                                     1206.17)),
#                     # jobseeker
#                     if_else(isjspceoalfofcoahodeoc,
#                             1435.17,      
#                             if_else(n_dependants > 0L,
#                                     if_else(has_partner,
#                                             989.67,
#                                             1149.84),
#                                     if_else(living_at_home,
#                                             if_else(age < 18,
#                                                     574.18,
#                                                     657.84),
#                                             989.67))))]
#   
#   max_income <- 
#     switch(fy.year,
#            "2017-18" = max_income_March_2018 <- 143,
#            .NotYetImplemented())
#   max_rate <- 
#     switch(fy.year,
#            "2017-18" = max_rate_March_2018 <- 293.60,
#            .NotYetImplemented())
#   
#   max_income_March_2016 <- 143
#   max_rate_March_2016 <- 244
#             
#   if (missing(annual_income)) {
#     ordinary_income <- fortnightly_income
#   } else {
#     if (missing(fortnightly_income)) {
#       ordinary_income <- annual_income / 26
#     } else {
#       if (max(abs(fortnightly_income - annual_income / 26)) > .Machine$double.eps^0.5) {
#         i <- which.max(abs(fortnightly_income - annual_income / 26))
#         stop("`fortnightly_income` and `annual_income` both provided but ",
#              "don't agree. \n\n\t", 
#              "i \tfortnightly_income\tannual_income\n\t", 
#              "--\t------------------\t-------------\n\t", 
#              i,
#              "\t", formatC(fortnightly_income[i], width = 18),
#              "\t", formatC(annual_income[i], width = 13), "\n")
#       }
#       ordinary_income <- fortnightly_income
#     }
#   }
#   
# 
#   # Benefit test
#   # http://guides.dss.gov.au/guide-social-security-law/4/2/2
#   # https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
#   
#   # ordinary_income_excess <- pmaxC(ordinary_income - ordinary_income_free_area)
#   
#   ### TESTS needed: https://www.humanservices.gov.au/individuals/services/centrelink/youth-allowance-students-and-australian-apprentices/how-much-you-can-get/income-and-assets-test# personalassets
#     # personal income test: 
#       # https://www.humanservices.gov.au/individuals/enablers/personal-income-test-austudy-and-youth-allowance/30411
#   
#   personal_income_reduction <-
#     if_else(is_student,
#             # student
#             if_else(ordinary_income < FT_YA_student_lower,
#                     0,
#                     if_else(ordinary_income < FT_YA_student_upper,
#                             taper1 * (ordinary_income - FT_YA_student_lower),
#                             if_else(ordinary_income < max_income,
#                                     taper1 * (ordinary_income - FT_YA_student_lower) +
#                                       (taper2 - taper1) * (ordinary_income - FT_YA_student_upper),
#                                     # reduce by entire income i.e. dont get YA
#                                     max_rate_March_2018))),
#             # jobseeker
#             if_else(ordinary_income < FT_YA_jobseeker_lower,
#                     0,
#                     if_else(ordinary_income < FT_YA_jobseeker_upper,
#                             taper1 * (ordinary_income - FT_YA_jobseeker_lower),
#                             if_else(ordinary_income < max_income,
#                                     taper1 * (ordinary_income - FT_YA_jobseeker_lower) +
#                                       (taper2 - taper1) * (ordinary_income - FT_YA_jobseeker_upper),
#                                     # reduce by entire income i.e. dont get YA
#                                     max_rate_March_2018))))
#     # personal assets test: 
#       # http://guides.dss.gov.au/guide-social-security-law/4/2/8/30 
#       # ## The personal assets test for independent YA recipients 
#       # ##  is the same as the benefits assets tes
#       # http://guides.dss.gov.au/guide-social-security-law/4/2/3
#       # https://www.legislation.gov.au/Details/C2018C00167
#   
#     personal_asset_reduction <-
#       input[, if_else(has_partner,
#                       if_else(homeowner,
#                               if_else(assets_value > 286500,
#                                       max_rate_March_2016,
#                                       0),
#                               if_else(assets_value > 433000,
#                                       max_rate_March_2016,
#                                       0)),
#                       if_else(homeowner,
#                               if_else(assets_value > 202000,
#                                       max_rate_March_2016,
#                                       0),
#                               if_else(assets_value > 348500,
#                                       max_rate_March_2016,
#                                       0)))]
#   
#     # partner income test
#       # http://guides.dss.gov.au/guide-social-security-law/4/2/2
#       # http://guides.dss.gov.au/guide-social-security-law/4/2/8/40
#       # example: http://guides.dss.gov.au/guide-social-security-law/5/5/3/30
#     partner_income_reduction <- 
#       # https://web.archive.org/web/20160812171654/http://guides.dss.gov.au/guide-social-security-law/5/5/3/30
#       if_else(has_partner & (partner_income > max_income_March_2018) & (age >= 22),
#               0.6 * (partner_income - round((1/0.6) * (max_rate_March_2018 - (upper - lower) * 0.5 + 252 * 0.6))),
#               0)
#   
#     
#   # parental income test:  INCOMPLETE
#     # https://www.humanservices.gov.au/individuals/services/centrelink/youth-allowance-students-and-australian-apprentices/how-much-you-can-get/income-and-assets-test# parentalincome
#     # http://guides.dss.gov.au/guide-social-security-law/4/2/8/05                          
#     
#   MITRA <-
#       233.94 - 57.68 #ftba max rate - basic rate
#     
#   PITR <- # http://guides.dss.gov.au/guide-social-security-law/4/2/8/10
#       input[, if_else(parents_income > 51027,
#                     (parents_income - 51027) / 130,
#                     0)]# NOTE: steps 6-10 not applied as require info on other children who receive payments
#   
#   MITR <-  
#     # MIFA historical rates: http://guides.dss.gov.au/family-assistance-guide/3/6/1# table7
#     if_else(n_siblings_on_ya == 0 & !ftb_children,
#             1620.60,
#             if_else(ftb_children,
#                     540.20,
#                     (1620.60 + n_siblings_on_ya * 540.20) / (1 + n_siblings_on_ya)))
#     # need maintenance income
#     
#   parental_income_reduction <-
#     if_else(PITR > MITRA,
#             PITR,
#             0)
#   ans <- 
#     max_rate_March_2016 - 
#     personal_income_reduction - 
#     parental_income_reduction - 
#     asset_reduction - 
#     partner_income_reduction
#   
#   pmax0(ans) / validate_per(per, missing(per))
# }

