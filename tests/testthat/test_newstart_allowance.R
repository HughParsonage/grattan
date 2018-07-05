context("newstart allowance")

test_that("Warnings", {
  expect_warning(newstart_allowance(),
                 '`fy.year` not set, so defaulting to fy.year = "2015-16"')
})

test_that("Correct values, no income", {
  #values retrieved for singles: http://guides.dss.gov.au/guide-social-security-law/5/2/1/20
  #values retrieved for couples: http://guides.dss.gov.au/guide-social-security-law/5/2/1/30
  #additional source from wayback machine which contains isjspceoalfofcoahodeoc value: https://web.archive.org/web/20160502192819/http://guides.dss.gov.au/guide-social-security-law/5/1/8/20
  #using 20/03/2016 values, note that rate is indexed in March and September each year.
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22),
               527.6)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  n_dependants = 1L),
               570.8)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 60,
                                  nine_months = TRUE),
               570.8)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  has_partner = TRUE),
               476.4)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  has_partner = TRUE,
                                  n_dependants = 1L),
               476.4)
  expect_equal(newstart_allowance(ordinary_income = 0,
                                  age = 22,
                                  isjspceoalfofcoahodeoc = TRUE),
               737.10)
})

#examples for singles taken from http://guides.dss.gov.au/guide-social-security-law/5/5/2
  #subtracted extra benefits (RA, ES, ...)
test_that("Correct values, income", {
  expect_equal(newstart_allowance(ordinary_income = 300,
                                  age = 22),
               423.8) 
  expect_equal(newstart_allowance(ordinary_income = 300,
                                  age = 22,
                                  has_partner = FALSE,
                                  n_dependants = 1L,
                                  principal_carer = TRUE),
               491.6)
  #examples for partners http://guides.dss.gov.au/guide-social-security-law/5/5/3
    #2015-16 values taken from wayback machine
  expect_equal(newstart_allowance(ordinary_income = 290,
                                  age = 22,
                                  has_partner = TRUE,
                                  partner_income = 160,
                                  n_dependants = 1L),
               378.6)#example: /10
  expect_equal(newstart_allowance(ordinary_income = 140,
                                  age = 22,
                                  has_partner = TRUE,
                                  partner_income = 950,
                                  n_dependants = 1L),
               440)#example: /30 except es removed
  #example /50?
  
})

test_that("Multiple people", {
  expect_equal(newstart_allowance(ordinary_income = c(0,0), n_dependants = c(0,1),
                                  age = 22),
               c(527.6, 570.8))
})

test_that("Elligibility and asset threshhold", {
  expect_equal(newstart_allowance(age = 20),
               0)
  expect_equal(newstart_allowance(age = 65),
               0)
  expect_equal(newstart_allowance(assets_value = 348000), 
               527.6)
  expect_equal(newstart_allowance(assets_value = 348501),
               0)
})
