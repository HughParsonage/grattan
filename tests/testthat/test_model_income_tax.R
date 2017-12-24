context("model_income_tax")

test_that("Error handling", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  s_no_ti <- 
    sample_file_1314_copy %>%
    copy %>%
    .[, Taxable_Income := NULL]
  
  expect_error(model_income_tax(s_no_ti, "2013-14"),
               regexp = "does not contain a column.*Taxable_Income")
  
  s_no_age <-
    sample_file_1314_copy %>%
    copy %>%
    .[, age_range := NULL]
  
  expect_warning(model_income_tax(s_no_age, "2013-14"), 
                 regexp = "Assuming everyone is ineligible for SAPTO.")
  
  
})

test_that("La plus ca meme la plus ca meme", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(sample_file_1314))
  
  new_tax <- model_income_tax(sample_file_1314_copy,
                              baseline_fy = "2013-14",
                              ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                              ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45))
  
  expect_equal(new_tax, original)
  
  
  
  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(sample_file_1314))
  
  new_tax <- model_income_tax(sample_file_1314_copy,
                              baseline_fy = "2013-14",
                              medicare_levy_rate = 0.015)
  
  expect_equal(new_tax, original)
})

test_that("Increase in a rate results in more tax", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(sample_file_1314))
  
  new_tax <- model_income_tax(sample_file_1314_copy,
                              baseline_fy = "2013-14",
                              ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                              ordinary_tax_rates = c(0, 0.19, 0.325, 0.37,
                                                     0.46))
  
  expect_true(all(new_tax >= original))
  
  unchanged_indices <- which(sample_file_1314_copy$Taxable_Income < 180e3)
  expect_false(any(new_tax[unchanged_indices] > original[unchanged_indices] + 1))
})

test_that("Medicare options", {
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  original <- income_tax(sample_file_1314$Taxable_Income,
                         fy.year = "2013-14",
                         .dots.ATO = copy(sample_file_1314))
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_upper_threshold = 40349",
                 fixed = TRUE)
  
  sample_file_1314_if_2pc_ML <-
    sample_file_1314 %>%
    copy %>%
    .[, old_tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = .)] %>%
    .[, new_tax := model_income_tax(sample_file_1314_copy,
                                    baseline_fy = "2013-14",
                                    medicare_levy_upper_threshold = 40349,
                                    medicare_levy_rate = 0.02)] %>%
    .[]
  
  min_unchanged <- 
    sample_file_1314_if_2pc_ML[new_tax != old_tax,
                               .(min_TI = min(Taxable_Income))] %>%
    .subset2("min_TI")
  
  expect_equal(min_unchanged, 24168)
  
  
  first_above100k <- 
    sample_file_1314_if_2pc_ML[Taxable_Income >= 100e3] %>%
    .[order(Taxable_Income), .(difference = first(new_tax - old_tax),
                               first_abv = first(Taxable_Income))]
  
  expect_equal(first_above100k$difference, 0.005 * first_above100k$first)
  
  shift_thresholds <- 
    sample_file_1314_copy %>%
    copy %>%
    .[, old_tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = .)] %>%
    .[, new_tax := model_income_tax(.,
                                    baseline_fy = "2013-14",
                                    medicare_levy_lower_threshold = 21542,
                                    medicare_levy_upper_threshold = 25343)] %>%
    .[]
  
})

test_that("Elasticity of taxable income", {
  s12131314 <- 
    copy(sample_files_all) %>%
    setkey(Ind) %>%
    .[()]
  
  no_elasticity <- 
    model_income_tax(copy(s12131314),
                     "2016-17",
                     medicare_levy_rate = 0.025, 
                     medicare_levy_upper_threshold = 44984, 
                     return. = "sample_file") %>%
    .[, .(Ind,
          Taxable_Income,
          old_tax = income_tax(Taxable_Income, "2016-17", .dots.ATO = .),
          new_tax)]
  
  elasticity_0.5 <-
    model_income_tax(copy(s12131314),
                     "2016-17",
                     elasticity_of_taxable_income = 0.5,
                     medicare_levy_rate = 0.025,
                     medicare_levy_upper_threshold = 44984, 
                     return = "sample_file")
  elasticity_0.5 <-
    elasticity_0.5[, .(Ind,
                       Taxable_Income_e.5 = Taxable_Income,
                       old_tax = income_tax(Taxable_Income, "2016-17", .dots.ATO = copy(elasticity_0.5)),
                       new_taxable_income_e.5 = new_taxable_income,
                       new_tax_e.5 = new_tax)]
  
  elasticity_1.0 <-
    model_income_tax(copy(s12131314),
                     "2016-17",
                     elasticity_of_taxable_income = 1,
                     medicare_levy_rate = 0.025,
                     medicare_levy_upper_threshold = 44984, 
                     return. = "sample_file")
  elasticity_1.0 <- 
    elasticity_1.0[, .(Ind,
                       Taxable_Income_e1 = Taxable_Income,
                       old_tax_e1 = income_tax(Taxable_Income, "2016-17", .dots.ATO = copy(elasticity_1.0)),
                       new_taxable_income_e1 = new_taxable_income,
                       new_tax_e1 = new_tax)]
  
  
  no_elasticity[elasticity_0.5[elasticity_1.0, on = "Ind"], on = "Ind"]
  
})



