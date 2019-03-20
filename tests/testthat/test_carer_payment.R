context("Carer Payment")

test_that("Messages", {
  expect_message(carer_payment(),
                 regexp = '`Date` and `fy.year` not set, so using `Date = ',
                 fixed = TRUE)
})

test_that("Eligibility", {
  expect_equal(carer_payment(care_receiver_asset_value = 10000000),
               0)
  #high adat
  expect_equal(carer_payment(Date = "2018-03-20", 
                             high_adat = TRUE, 
                             care_receiver_annual_income = 50000, 
                             partner_annual_income = 100000),
               0)
  expect_equal(carer_payment(Date = "2018-03-20", 
                             high_adat = TRUE, 
                             care_receiver_annual_income = 50000, 
                             partner_annual_income = 50000,
                             children_annual_income = 50000),
               age_pension(Date = "2018-03-20"))
  #high_adat and receving other payment
  expect_equal(carer_payment(Date = "2018-03-20", 
                             high_adat = TRUE, 
                             care_receiver_annual_income = 50000, 
                             partner_annual_income = 100000,
                             receiving_other_payment = TRUE),
               age_pension(Date = "2018-03-20"))
  expect_equal(carer_payment(Date = "2018-03-20", 
                             high_adat = TRUE, 
                             care_receiver_asset_value = 500000, 
                             partner_asset_value = 500000,
                             receiving_other_payment = TRUE),
               age_pension(Date = "2018-03-20"))
  #child not at home - income
  expect_equal(carer_payment(Date = "2018-03-20", 
                             dclad_eligible = TRUE,
                             living_at_home = FALSE,
                             care_receiver_annual_income = 50000,
                             parents_annual_income = 100000),
               age_pension(Date = "2018-03-20"))
  expect_equal(carer_payment(Date = "2018-03-20", 
                             dclad_eligible = TRUE,
                             living_at_home = TRUE,
                             care_receiver_annual_income = 50000,
                             parents_annual_income = 100000),
               0)
  #child not at home - assets
  expect_equal(carer_payment(Date = "2018-03-20", 
                             dclad_eligible = TRUE,
                             living_at_home = FALSE,
                             care_receiver_asset_value = 500000,
                             parents_asset_value = 500000),
               age_pension(Date = "2018-03-20"))
  expect_equal(carer_payment(Date = "2018-03-20", 
                             dclad_eligible = TRUE,
                             living_at_home = TRUE,
                             care_receiver_asset_value = 500000,
                             parents_asset_value = 500000),
               0)
})

test_that("Matches age pension payment", {
  expect_equal(carer_payment(),
               age_pension())
})

test_that("fy.year supplied", {
  expect_equal(carer_payment(fy.year = "2015-16"),
               20664.8)
})

