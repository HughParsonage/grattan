context("Carer Payment")

test_that("Eligibility", {
  expect_equal(carer_payment(dclad_eligible = FALSE, low_adat = FALSE),
               0)
  expect_equal(carer_payment(care_receiver_asset_value = 10000000),
               0)
  expect_equal(carer_payment(high_adat = TRUE, care_receiver_annual_income = 100000, partner_annual_income = 100000),
               0)
})

test_that("Matches age pension payment", {
  expect_equal(carer_payment(),
               age_pension())
  expect_equal(carer_payment(carer_fortnightly_income = 1000, 
                             carer_is_home_owner = TRUE, 
                             carer_has_partner = TRUE, 
                             carer_n_dependants = 5, 
                             carer_partner_annual_income = 1000), 
               age_pension(ordinary_income = 1000, 
                           is_home_owner = TRUE, 
                           has_partner = TRUE, 
                           n_dependants = 5, 
                           partner_annual_income = 1000))
  
})

