context("newstart allowance")

test_that("Correct values, no income", {
  #values retrieved for singles: http://guides.dss.gov.au/guide-social-security-law/5/2/1/20
  #values retrieved for couples: http://guides.dss.gov.au/guide-social-security-law/5/2/1/30
  #additional source from wayback machine which contains isjspceoalfofcoahodeoc value: https://web.archive.org/web/20160502192819/http://guides.dss.gov.au/guide-social-security-law/5/1/8/20
  #using 20/03/2016 values, note that rate is indexed in March and September each year.
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22),
               263.80 * 2)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  n_dependants = 1L),
               285.40 * 2)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 60,
                                  nine_months = TRUE),
               285.40 * 2)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  has_partner = TRUE),
               238.20 * 2)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  has_partner = TRUE,
                                  n_dependants = 1L),
               238.20 * 2)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  isjspceoalfofcoahodeoc = TRUE),
               737.10 * 2)
})

#examples for singles taken from http://guides.dss.gov.au/guide-social-security-law/5/5/2
  #subtracted extra benefits (RA, ES, ...)
test_that("Correct values, income", {
  expect_equal(newstart_allowance(ordinary_income = 300,
                                  age = 22),
               425) 
  expect_equal(newstart_allowance(ordinary_income = 300,
                                  age = 22,
                                  has_partner = FALSE,
                                  n_dependants = 1L),
               492.4)#note: single parent uses different income reduction function
  #examples for partners http://guides.dss.gov.au/guide-social-security-law/5/5/3
  expect_equal(newstart_allowance(ordinary_income = 290,
                                  age = 22,
                                  has_partner = TRUE,
                                  partner_income = 160,
                                  partner_eligible = TRUE,
                                  n_dependants = 1L),
               379.8)#example: /10
  expect_equal(newstart_allowance(ordinary_income = 140,
                                  age = 22,
                                  has_partner = TRUE,
                                  partner_income = 965,
                                  partner_eligible = FALSE,
                                  n_dependants = 1L),
               476.4-18-0.6)#example: /30
  #example /50?
  
})
