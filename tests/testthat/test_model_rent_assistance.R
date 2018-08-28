context("Model rent assistance")

test_that("Values", {
  sample <- CJ(rent = 1:500, n_dependants = 0, has_partner = FALSE, is_homeowner = FALSE, lives_in_sharehouse = FALSE)
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.8)),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.7)))
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 0)),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100)))
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100)),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 200, Min_rent = 100)))
})

test_that("Errors", {
  sample <- CJ(gsdfgfdg = 1:500, n_dependants = 0, has_partner = FALSE, is_homeowner = FALSE, lives_in_sharehouse = FALSE)
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19"),
               regexp = "`sample_file` lacked the following required columns:\n\trent.\n")
  expect_error(model_rent_assistance(sample),
               rgexp = "one of `baseline_fy` or `baseline_Date` was not provided")
  expect_error(model_rent_assistance(300, baseline_fy = "2018-19"),
               reg_exp = "is.data.frame(sample_file) is not TRUE")
})