context("Model rent assistance")

test_that("Values", {
  library(data.table)
  sample <- CJ(rent = 1:500,
               n_dependants = 0,
               has_partner = FALSE,
               is_homeowner = FALSE,
               lives_in_sharehouse = FALSE)
  m1819_3 <- function(p = 0.75, maxr = 300, minr = 0, ret = "new_ra") {
    res <- model_rent_assistance(sample,
                                 baseline_fy = "2018-19",
                                 .Prop_rent_paid_by_RA = p,
                                 Max_rate = maxr,
                                 Min_rent = minr,
                                 return. = ret)
    if (ret == "new_ra") {
      sum(res) 
    } else {
      res
    }
  }
  
  expect_gt(m1819_3(p = 0.8),
            m1819_3(p = 0.7))
  
  expect_gt(m1819_3(),
            m1819_3(minr = 100))
  
  expect_gt(m1819_3(maxr = 300),
            m1819_3(maxr = 200))
  
  expect_true(is.data.table(m1819_3(ret = "sample_file")))
  expect_true(is.data.table(m1819_3(ret = "sample_file.int")))
  expect_true(is.integer(.subset2(m1819_3(ret = "sample_file.int"),
                                  "baseline_ra")))
})

test_that("Errors", {
  library(data.table)
  sample <- CJ(gsdfgfdg = 1:500,
               n_dependants = 0,
               has_partner = FALSE,
               is_homeowner = FALSE,
               lives_in_sharehouse = FALSE)
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     baseline_Date = "2018-01-01",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "`baseline_fy` is NULL, yet `baseline_Date` is also NULL.")
  expect_error(model_rent_assistance(sample,
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "Neither `baseline_fy` nor `baseline_Date` was provided.")
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "need all of `.Prop_rent_paid_by_RA`, `Max_rate` and `Min_rent`.")
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = "0.75",
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "`.Prop_rent_paid_by_RA` was type character, but must be numeric.")
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = "300",
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "`Max_rate` was type character, but must be numeric.")  
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = "100",
                                     return. = "new_ra"),
               
               regexp = "`Min_rent` was type character, but must be numeric.")  
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     calc_baseline_ra = "FALSE",
                                     return. = "new_ra"),
               
               regexp = "`calc_baseline_ra` was type character but must be logical.")
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "singlefile"),
               
               regexp = "should be one of")
  expect_error(model_rent_assistance(sample,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "`sample_file` lacked the following required columns:\n\trent.\n")
  expect_error(model_rent_assistance(300,
                                     baseline_fy = "2018-19",
                                     .Prop_rent_paid_by_RA = 0.75,
                                     Max_rate = 300,
                                     Min_rent = 100,
                                     return. = "new_ra"),
               
               regexp = "`sample_file` was a numeric, but must be a data.frame")
})
