context("Model rent assistance")

test_that("Values", {
  sample<- CJ(rent = 1:500, n_dependants = 0, has_partner = FALSE, is_homeowner = FALSE, lives_in_sharehouse = FALSE)
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.8, return. = "new_ra")),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.7, return. = "new_ra")))
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 400, Min_rent = 0, return. = "new_ra")),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 400, Min_rent = 100, return. = "new_ra")))
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 400, Min_rent = 100, return. = "new_ra")),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100, return. = "new_ra")))
})

test_that("Errors", {
  sample<- CJ(gsdfgfdg = 1:500, n_dependants = 0, has_partner = FALSE, is_homeowner = FALSE, lives_in_sharehouse = FALSE)
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19"),
               regexp = "`sample_file` lacked the following required columns:\n\trent.\n")
})