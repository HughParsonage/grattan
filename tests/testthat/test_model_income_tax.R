context("model_income_tax")

test_that("Error handling", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  library(hutils)
  sample_file_1314_copy <- copy(sample_file_1314)
  s_no_ti <- 
    sample_file_1314_copy %>%
    copy %>%
    .[, Taxable_Income := NULL]
  
  expect_error(model_income_tax(s_no_ti, "2013-14"),
               regexp = "Taxable_Income")
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                sapto_eligible = "wrong"),
               regexp = "sapto_eligible.*not a logical vector")
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                sapto_eligible = c(FALSE, TRUE)),
               regexp = "sapto_eligible. was length 2")
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                ordinary_tax_thresholds = c(0, 18200, 30e3),
                                ordinary_tax_rates = c(0, 0.19)),
               regexp = "ordinary_tax_thresholds.*different lengths")
  
  expect_warning(model_income_tax(drop_col(copy(sample_file_1314_copy), "age_range"),
                                  baseline_fy = "2013-14",
                                  ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                                  ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45)),
                 regexp = "Assuming everyone is ineligible for SAPTO.")
  
  
})

test_that("La plus ca meme la plus ca meme: ordinary tax", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(sample_file_1314))
  
  new_tax <- model_income_tax(sample_file_1314_copy,
                              baseline_fy = "2013-14",
                              ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                              ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45)) %>%
    .subset2("new_tax")
  
  expect_equal(new_tax, original)
})

test_that("La plus ca meme la plus ca meme: la deluge", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1112_copy <- copy(sample_file_1112)
  original <-
    income_tax(sample_file_1112_copy$Taxable_Income,
               "2011-12",
               .dots.ATO = copy(sample_file_1112_copy))
  
  new_tax <-
    model_income_tax(sample_file_1112_copy,
                     baseline_fy = "2011-12",
                     ordinary_tax_thresholds = c(0, 6000, 35e3, 80e3, 180e3),
                     ordinary_tax_rates = c(0, 0.15, 0.30, 0.37, 0.45)) %>%
    .subset2("new_tax")
  
  expect_equal(new_tax, original)
})
  
test_that("La plus ca meme la plus ca meme: medicare levy", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  
  original <- 
    income_tax(sample_file_1314_copy$Taxable_Income,
               fy.year = "2013-14",
               .dots.ATO = copy(sample_file_1314_copy))
  
  new_tax2 <-
    model_income_tax(copy(sample_file_1314_copy),
                     baseline_fy = "2013-14",
                     medicare_levy_rate = 0.015, 
                     return. = "tax")
  
  expect_equal(new_tax2, original)
})

test_that("Increase in a rate results in more tax", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(sample_file_1314))
  original <- round(original)
  
  new_tax <-
    model_income_tax(sample_file_1314_copy,
                     baseline_fy = "2013-14",
                     ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                     ordinary_tax_rates = c(0, 0.19, 0.325, 0.37,
                                            0.46)) %>%
    .subset2("new_tax") %>%
    round
  
  expect_true(all(new_tax >= original))
  
  unchanged_indices <- which(sample_file_1314_copy$Taxable_Income < 180e3)
  expect_false(any(new_tax[unchanged_indices] > original[unchanged_indices] + 1))
})

test_that("Medicare warnings", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1314_copy <- copy(sample_file_1314)
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_upper_threshold = 25678",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 27168,
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_lower_threshold = 21734",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_taper = 0.06",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_taper = 0.06), 
                 regexp = "medicare_levy_rate = 0.02",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_taper = 0.06,
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_upper_sapto_threshold = 48419",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_taper = 0.06,
                                  medicare_levy_rate = 0.02,
                                  medicare_levy_upper_sapto_threshold = 48417), 
                 regexp = "medicare_levy_upper_family_threshold = 51551",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  medicare_levy_upper_family_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 48417)), 
                 regexp = "but its default values would be inconsistent with the parameters that were specified",
                 fixed = TRUE)
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  medicare_levy_upper_family_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 48417)), 
                 regexp = "(First and last five shown.)",
                 fixed = TRUE)
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                
                                # In 2013-14, the rate was 0.015
                                medicare_levy_upper_threshold = 30e3,
                                medicare_levy_lower_threshold = 20e3,
                                medicare_levy_taper = 0.06,
                                medicare_levy_rate = 0.02,
                                medicare_levy_upper_sapto_threshold = 48419,
                                medicare_levy_lower_family_threshold = 48417,
                                medicare_levy_upper_family_threshold = 51551), 
                 regexp = "`medicare_levy_upper_family_threshold` and `medicare_levy_lower_family_threshold` were both supplied",
                 fixed = TRUE)
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                
                                # In 2013-14, the rate was 0.015
                                medicare_levy_upper_threshold = 30e3,
                                medicare_levy_lower_threshold = 20e3,
                                medicare_levy_taper = 0.06,
                                medicare_levy_rate = 0.02,
                                medicare_levy_upper_sapto_threshold = 48419,
                                medicare_levy_lower_family_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 30e3),
                                medicare_levy_upper_family_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]] + 100, 40e3)), 
               regexp = "`medicare_levy_upper_family_threshold` and `medicare_levy_lower_family_threshold` were both supplied.*first 6 shown",
               fixed = FALSE)
  
  expect_error(model_income_tax(sample_file_1314_copy,baseline_fy = "2013-14",
                                medicare_levy_lower_family_sapto_threshold = 30e3,
                                medicare_levy_upper_family_sapto_threshold = 35e3),
               regexp = "`medicare_levy_upper_family_sapto_threshold` and `medicare_levy_lower_family_sapto_threshold` were both supplied",
               fixed = TRUE)
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                medicare_levy_lower_family_sapto_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 30e3),
                                medicare_levy_upper_family_sapto_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]] + 100, 40e3)),
               regexp = "`medicare_levy_upper_family_sapto_threshold` and `medicare_levy_lower_family_sapto_threshold` were both supplied.*first 6 shown",
               fixed = FALSE)
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                medicare_levy_upper_sapto_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 30e3),
                                medicare_levy_lower_sapto_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]] + 100, 40e3)),
               regexp = "`medicare_levy_upper_sapto_threshold` and `medicare_levy_lower_sapto_threshold` were both supplied.*first 6 shown",
               fixed = FALSE)
  
  modeled_other <-
    model_income_tax(sample_file_1314_copy,
                     baseline_fy = "2013-14",
                     
                     # In 2013-14, the rate was 0.015
                     medicare_levy_upper_threshold = 30e3,
                     medicare_levy_lower_threshold = 20e3,
                     medicare_levy_taper = 0.06,
                     medicare_levy_rate = 0.02,
                     medicare_levy_upper_sapto_threshold = 48417,
                     medicare_levy_upper_family_threshold = 51551,
                     medicare_levy_upper_family_sapto_threshold = 69000) %>%
    .[, old_tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = sample_file_1314_copy)] %>%
    .[]
  
  # No error
  expect_true(is.data.table(modeled_other))
  
  
  
})

test_that("Medicare options", {
  skip_if_not_installed("taxstats")
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
                                    medicare_levy_rate = 0.02, 
                                    return. = "tax")] %>%
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
                                    medicare_levy_upper_threshold = 25343,
                                    return. = "tax")] %>%
    .[]
  
  expect_warning(model_income_tax(sample_file_1314_copy, 
                                  medicare_levy_upper_family_sapto_threshold = 40e3 + 1,
                                  baseline_fy = "2013-14"),
                 regexp = "medicare_levy_lower_family_sapto_threshold = (33999|34000)")
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                medicare_levy_lower_family_threshold = 30e3,
                                medicare_levy_upper_family_threshold = 40e3,
                                medicare_levy_lower_threshold = 20e3,
                                medicare_levy_upper_threshold = 20833,
                                medicare_levy_upper_sapto_threshold = 33624,
                                medicare_levy_lower_sapto_threshold = 38399,
                                medicare_levy_taper = 0.5,
                                medicare_levy_rate = 0.02),
               regexp = "`medicare_levy_upper_sapto_threshold` and `medicare_levy_lower_sapto_threshold` were both supplied, but imply a Medicare taper rate of",
               fixed = TRUE)
  
})


test_that("Medicare families", {
  skip_on_cran()
  skip_if_not_installed("taxstats")
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
                     medicare_levy_upper_family_threshold = 43750,
                     return. = "tax")
  single_idx <- which(s1617$Spouse_adjusted_taxable_inc == 0)
  expect_equal(s1617_orig$orig_tax[single_idx],
               s1617_modelled[single_idx])
  
})

test_that("SAPTO modelled", {
  skip_on_cran()
  skip_if_not_installed("taxstats", minimum_version = "0.0.5")
  library(taxstats)
  sample_file_1415_copy <- copy(sample_file_1415_synth)
  
  expect_warning({
    model_income_tax(sample_file_1415_copy,
                     baseline_fy = "2014-15",
                     
                     # In 2013-14, the rate was 0.015
                     medicare_levy_upper_threshold = 30e3,
                     medicare_levy_lower_threshold = 20e3,
                     sapto_max_offset = 4460,
                     medicare_levy_taper = 0.06,
                     medicare_levy_rate = 0.02,
                     medicare_levy_upper_sapto_threshold = 48417,
                     medicare_levy_upper_family_threshold = 51551,
                     medicare_levy_upper_family_sapto_threshold = 69000)
  },
  regexp = "medicare_levy_lower_sapto_threshold = 32277",
  fixed = TRUE)
  
  
  expect_error({
    model_income_tax(sample_file_1415_copy,
                     baseline_fy = "2014-15",
                     
                     # In 2013-14, the rate was 0.015
                     medicare_levy_upper_threshold = 30e3,
                     medicare_levy_lower_threshold = 20e3,
                     sapto_max_offset = 4460,
                     medicare_levy_taper = 0.06,
                     medicare_levy_rate = 0.02,
                     medicare_levy_upper_sapto_threshold = 50e3,
                     medicare_levy_lower_sapto_threshold = 32277,
                     medicare_levy_upper_family_threshold = 51551,
                     medicare_levy_upper_family_sapto_threshold = 69000)
  }, 
  regexp = "Medicare levy parameter mismatch could not be safely resolved.*medicare_levy_upper_sapto_threshold")
  
  result <- 
    model_income_tax(sample_file_1415_copy,
                     baseline_fy = "2014-15",
                     
                     # In 2013-14, the rate was 0.015
                     medicare_levy_upper_threshold = 30e3,
                     medicare_levy_lower_threshold = 20e3,
                     sapto_max_offset = 4460,
                     medicare_levy_taper = 0.06,
                     medicare_levy_rate = 0.02,
                     medicare_levy_upper_sapto_threshold = 48417,
                     medicare_levy_lower_sapto_threshold = 32277,
                     medicare_levy_upper_family_threshold = 51551,
                     medicare_levy_upper_family_sapto_threshold = 69000)

  
  sapto_only_psnn <-
    copy(sample_file_1415_synth) %>%
    project_to(to_fy = "2016-17") %>%
    .[, saptoEligible := and(age_range <= 1L, Aust_govt_pnsn_allw_amt > 1)] %>%
    model_income_tax(baseline_fy = "2016-17",
                     sapto_eligible = .$saptoEligible) %>%
    .[, revenue := new_tax - baseline_tax] %>%
    .[]
  
  revenue_sapto_pension <- 
    sum(sapto_only_psnn$revenue * sapto_only_psnn$WEIGHT) / 1e6
  
  expect_gt(revenue_sapto_pension, 200)
  expect_lt(revenue_sapto_pension, 400)
})

test_that("LITO", {
  skip_if_not_installed("taxstats")
  library(taxstats)
  sample_file_1213_copy <- copy(sample_file_1213)
  
  old_taxes <-
    copy(sample_file_1213_copy) %>%
    .[, old_tax := income_tax(Taxable_Income, "2012-13", .dots.ATO = sample_file_1213_copy)] %>%
    .subset2("old_tax") %>%
    round
  
  new_taxes <- 
    model_income_tax(sample_file_1213_copy, 
                     baseline_fy = "2012-13",
                     lito_max_offset = 500,
                     return. = "tax") %>%
    round
  
  expect_true(all(new_taxes <= old_taxes))
})


test_that("Elasticity of taxable income", {
  skip_on_cran()
  skip_if_not_installed("taxstats")
  library(taxstats)
  s12131314 <- 
    copy(sample_file_1213) %>%
    .[Ind %% 3 == 0] %>%
    setkey(Ind)
  
  no_elasticity <- 
    model_income_tax(copy(s12131314),
                     "2016-17",
                     medicare_levy_rate = 0.025, 
                     medicare_levy_lower_threshold = 22499,
                     medicare_levy_upper_threshold = 30e3, 
                     medicare_levy_upper_sapto_threshold = 44984,
                     medicare_levy_upper_family_threshold = 48001,
                     medicare_levy_upper_family_sapto_threshold = 62621,
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
                     medicare_levy_upper_threshold = 30e3, 
                     medicare_levy_lower_threshold = 22499,
                     medicare_levy_upper_sapto_threshold = 44984,
                     medicare_levy_upper_family_threshold = 48001,
                     medicare_levy_upper_family_sapto_threshold = 62621,
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
                     medicare_levy_upper_threshold = 30e3, 
                     medicare_levy_lower_threshold = 22499,
                     medicare_levy_upper_sapto_threshold = 44984,
                     medicare_levy_upper_family_threshold = 48001,
                     medicare_levy_upper_family_sapto_threshold = 62621,
                     return. = "sample_file")
  elasticity_1.0 <- 
    elasticity_1.0[, .(Ind,
                       Taxable_Income_e1 = Taxable_Income,
                       old_tax_e1 = income_tax(Taxable_Income, "2016-17",
                                               .dots.ATO = copy(elasticity_1.0)),
                       new_taxable_income_e1 = new_taxable_income,
                       new_tax_e1 = new_tax)]
  
  result <- 
    no_elasticity[elasticity_0.5[elasticity_1.0, on = "Ind"], on = "Ind"]
  
  result[, old_private_income := Taxable_Income - old_tax]
  result[, private_income_zero := Taxable_Income - new_tax]
  result[, private_income_half := new_taxable_income_e.5 - new_tax_e.5]
  result[, private_income_full := new_taxable_income_e1 - new_tax_e1]
  
  expect_gt(nrow(result), 0)
  
})


test_that("Elasticity 0 vs 1", {
  skip_if_not_installed("taxstats", minimum_version = "0.0.5")
  library(taxstats)
  s1415 <- 
    copy(sample_file_1415_synth) %>%
    .[, old_tax := income_tax(Taxable_Income, "2014-15", .dots.ATO = .)] %>%
    unique(by = "Ind")
  
  s1415E0 <-
    model_income_tax(copy(s1415),
                     baseline_fy = "2014-15",
                     ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.50)) %>%
    .[, .(Ind,
          Taxable_Income_E0 = Taxable_Income,
          new_tax_E0 = new_tax)]
  
  s1415E1 <-
    model_income_tax(copy(s1415),
                     baseline_fy = "2014-15",
                     ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.50),
                     elasticity_of_taxable_income = 1) %>%
    .[, .(Ind,
          Taxable_Income_E1 = Taxable_Income,
          new_taxable_income_E1 = new_taxable_income,
          new_tax_E1 = new_tax)]
  
  result <- 
    s1415E0[s1415E1, on = "Ind"] %>%
    setcolorder(sort(names(.))) %>%
    .[s1415[, .(Ind, old_ti = Taxable_Income, old_tax)], on = "Ind"] %>%
    setkey(old_ti) %>%
    .[old_ti > 190e3] %>%
    .[, old_private_income := old_ti - old_tax] %>%
    .[, new_private_income0 := Taxable_Income_E0 - new_tax_E0] %>%
    .[, new_private_income1 := new_taxable_income_E1 - new_tax_E1] %>%
    .[, lapply(.SD, round)] %>%
    .[]
  
  expect_true(all(result$new_tax_E0 > result$new_tax_e1))
})




