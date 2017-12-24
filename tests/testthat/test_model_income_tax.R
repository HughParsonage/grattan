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
               regexp = "Taxable_Income")
  
  
})

test_that("La plus ca meme la plus ca meme: ordinary tax", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(sample_file_1314))
  
  new_tax <- model_income_tax(sample_file_1314_copy,
                              baseline_fy = "2013-14",
                              ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                              ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45))
  
  expect_equal(new_tax, original)
})
  
test_that("La plus ca meme la plus ca meme: medicare levy", {
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  
  original <- 
    income_tax(sample_file_1314_copy$Taxable_Income,
               fy.year = "2013-14",
               .dots.ATO = copy(sample_file_1314_copy))
  
  sample_file_manual_ML <-
    model_income_tax(copy(sample_file_1314_copy),
                     baseline_fy = "2013-14",
                     medicare_levy_rate = 0.015, 
                     return = "sample_file")
  
  expect_true(is.double(new_tax2))
  expect_equal(new_tax2, original)
  
  expect_equal(sample_file_manual_ML[["new_tax"]], original)
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
                 regexp = "medicare_levy_upper_threshold = 25678",
                 fixed = TRUE)
  
  sample_file_1314_if_2pc_ML <-
    sample_file_1314 %>%
    copy %>%
    .[, old_tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = .)] %>%
    .[, new_tax := model_income_tax(sample_file_1314_copy,
                                    baseline_fy = "2013-14",
                                    medicare_levy_upper_threshold = 25678,
                                    medicare_levy_upper_sapto_threshold = 40349,
                                    medicare_levy_upper_family_threshold = 42959,
                                    medicare_levy_upper_family_sapto_threshold = 57500,
                                    medicare_levy_rate = 0.02)] %>%
    .[]
  
  min_unchanged <- 
    sample_file_1314_if_2pc_ML %>%
    .[Spouse_adjusted_taxable_inc == 0] %>%
    .[age_range > 1] %>%
    .[new_tax != old_tax,
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


test_that("Medicare families", {
  s1617 <- project(sample_file_1314, h = 3L)
  
  for (j in seq_along(s1617)) {
    if (is.double(s1617[[j]])) {
      set(s1617, j = j, value = as.integer(s1617[[j]]))
    }
  }
  
  s1617_orig <- 
    copy(s1617) %>%
    .[, orig_tax := income_tax(Taxable_Income, "2016-17", .dots.ATO = s1617)]
  
  expect_warning(model_income_tax(s1617, 
                                  "2016-17",
                                  medicare_levy_lower_family_threshold = 35000, 
                                  return = "sample_file"),
                 regexp = "`medicare_levy_upper_family_threshold` was not specified",
                 fixed = TRUE)
  
  s1617_modelled <-
    model_income_tax(s1617, 
                     "2016-17",
                     medicare_levy_lower_family_threshold = 35000,
                     medicare_levy_upper_family_threshold = 43750)
  single_idx <- which(s1617$Spouse_adjusted_taxable_inc == 0)
  expect_equal(s1617_orig$orig_tax[single_idx],
               s1617_modelled[single_idx])
  
})

test_that("Elasticity of taxable income", {
  s12131314 <- 
    copy(sample_file_1213) %>%
    .[Ind %% 3 == 0]
  
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


