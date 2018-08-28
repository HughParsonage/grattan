context("Model rent assistance")

test_that("Values", {
  sample <- CJ(rent = 1:500, n_dependants = 0, has_partner = FALSE, is_homeowner = FALSE, lives_in_sharehouse = FALSE)
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.8, Max_rate = 300, Min_rent = 0)),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.7, Max_rate = 300, Min_rent = 0)))
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 0)),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100)))
  expect_gt(sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100)),
            sum(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 200, Min_rent = 100)))
})

test_that("Errors", {
  sample <- CJ(gsdfgfdg = 1:500, n_dependants = 0, has_partner = FALSE, is_homeowner = FALSE, lives_in_sharehouse = FALSE)
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", baseline_Date = "2018-01-01", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100),
               reg_exp = "only one of either `baseline_fy` or `baseline_Date` can be provided.")
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", Max_rate = 300, Min_rent = 100),
               reg_exp = "need all of `.Prop_rent_paid_by_RA`, `Max_rate` and `Min_rent`.")
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = "0.75", Max_rate = 300, Min_rent = 100),
               reg_exp = "`.Prop_rent_paid_by_RA` was type character, but must be numeric.")
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = "300", Min_rent = 100),
               reg_exp = "`Max_rate` was type character, but must be numeric.")  
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = "100"),
               reg_exp = "`Min_rent` was type character, but must be numeric.")  
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100, calc_baseline_ra = "FALSE"),
               reg_exp = "`calc_baseline_ra` was type character, but must be logical.")
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100, return. = "singlefile"),
               reg_exp = "`return. was not one of `new_ra`, `sample_file`, `sample_file.int`")
  expect_error(model_rent_assistance(sample, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100),
               reg_exp = "`sample_file` lacked the following required columns:\n\trent.\n")
  expect_error(model_rent_assistance(300, baseline_fy = "2018-19", .Prop_rent_paid_by_RA = 0.75, Max_rate = 300, Min_rent = 100),
               reg_exp = "is.data.frame(sample_file) is not TRUE")
})