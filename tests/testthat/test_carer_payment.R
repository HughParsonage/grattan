context("Carer Payment")

test_that("Eligibility", {
  expect_equal(carer_payment(dclad_eligible = FALSE, adat_eligible = FALSE),
               0)
  expect_equal(carer_payment(care_receiver_asset_value = 10000000),
               0)
  expect_equal(carer_payment(high_adat = TRUE, care_receiver_annual_income = 100000, partner_income = 100000),
               0)
})

