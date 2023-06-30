context("model_income_tax")

test_that("Error handling", {
  skip_on_cran()
  skip_without_sample_files()
  library(hutils)
  sample_file_1314 <- .sample_file_1314()
  sample_file_1314_copy <- copy(.sample_file_1314())
  s_no_ti <- 
    sample_file_1314_copy %>%
    copy %>%
    .[, Taxable_Income := NULL]
  
  expect_error(model_income_tax(s_no_ti, "2013-14"),
               regexp = "Taxable_Income")
  
  expect_error(model_income_tax(sample_file_1314_copy,
                                baseline_fy = "2013-14",
                                ordinary_tax_thresholds = c(0, 18200, 30e3),
                                ordinary_tax_rates = c(0, 0.19)),
               regexp = "ordinary_tax_thresholds.* lengths")
  
  

  original <- income_tax(sample_file_1314$Taxable_Income, "2013-14", .dots.ATO = copy(.sample_file_1314()))
  
  new_tax <- model_income_tax(sample_file_1314_copy,
                              baseline_fy = "2013-14",
                              ordinary_tax_thresholds = c(0, 18200, 37e3, 80e3, 180e3),
                              ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45)) %>%
    .subset2("new_tax")
  
  expect_equal(new_tax, original)
})

test_that("La plus ca meme la plus ca meme: la deluge", {
  skip_on_cran()
  skip_on_circleci(2)

  sample_file_1112 <- .sample_file_("1112")
  sample_file_1112_copy <- copy(sample_file_1112)
  original <-
    income_tax(sample_file_1112_copy$Taxable_Income,
               "2011-12",
               .dots.ATO = copy(sample_file_1112_copy))
  
  new_tax <-
    model_income_tax(sample_file_1112_copy,
                     baseline_fy = "2011-12",
                     ordinary_tax_thresholds = c(0, 6000, 37e3, 80e3, 180e3),
                     ordinary_tax_rates = c(0, 0.15, 0.30, 0.37, 0.45)) %>%
    .subset2("new_tax")
  
  expect_equal(new_tax, original)
})

test_that("La plus ca meme la plus ca meme: medicare levy", {
  skip_on_cran()
  skip_on_circleci(2)

  sample_file_1314_copy <- copy(.sample_file_1314())
  
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

test_that("La plus ca meme la plus ca meme: LITO", {
  skip_on_cran()
  skip_on_circleci(2)
  
  sample_file_1314_copy <- copy(.sample_file_1314())
  
  original <- 
    income_tax(sample_file_1314_copy$Taxable_Income,
               fy.year = "2013-14",
               .dots.ATO = copy(sample_file_1314_copy))
  
  new_tax2 <-
    model_income_tax(copy(sample_file_1314_copy),
                     baseline_fy = "2013-14",
                     lito_taper = 0.015, 
                     return. = "tax")
  
  expect_equal(new_tax2, original)
})

test_that("La plus ca meme la plus ca meme: SBTO doesn't interfere with SBTO", {
  skip_on_cran()
  skip_on_circleci(2)
  library(magrittr)
  library(data.table)
  sample_file_1314_copy <- copy(.sample_file_1314())
  sample_file_81913 <- 
    sample_file_1314_copy %>%
    .[Ind == 81913] %T>%
    .[, stopifnot(Taxable_Income > 180e3, Net_NPP_BI_amt > 1000)] %>%
    model_income_tax("2016-17")
  
  expect_equal(sample_file_81913[["baseline_tax"]],
               sample_file_81913[["new_tax"]],
               tol = 1, scale = 1)
})

test_that("Increase in a rate results in more tax", {
  skip_on_cran()
  skip_on_circleci(2)
  
  sample_file_1314_copy <- copy(.sample_file_1314())
  original <- income_tax(.sample_file_1314()$Taxable_Income, "2013-14", .dots.ATO = copy(.sample_file_1314()))
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
  skip_on_cran()
  skip_on_circleci(2)
  sample_file_1314_copy <- copy(.sample_file_1314())
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_upper_threshold .* 2567[6789]")
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 27168,
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_lower_threshold .* 217")
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_taper = 0.06")
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_taper = 0.06), 
                 regexp = "medicare_levy_rate = 0.02")
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_taper = 0.06,
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_upper_sapto_threshold = 4841[789]")
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_upper_threshold = 30e3,
                                  medicare_levy_lower_threshold = 20e3,
                                  medicare_levy_taper = 0.06,
                                  medicare_levy_rate = 0.02,
                                  medicare_levy_upper_sapto_threshold = 48417), 
                 regexp = "medicare_levy_upper_family_threshold = 5155[012]")
  
  # expect_warning(model_income_tax(sample_file_1314_copy,
  #                                 baseline_fy = "2013-14",
  #                                 medicare_levy_upper_family_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 48417)), 
  #                regexp = "but its default values would be inconsistent with the parameters that were specified",
  #                fixed = TRUE)
  # 
  # expect_warning(model_income_tax(sample_file_1314_copy,
  #                                 baseline_fy = "2013-14",
  #                                 medicare_levy_upper_family_threshold = pmaxC(sample_file_1314_copy[["Taxable_Income"]], 48417)), 
  #                regexp = "(First and last five shown.)",
  #                fixed = TRUE)
  
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
               regexp = "`medicare_levy_(upper|lower)_family_threshold.* and `medicare_levy_(lower|upper)_family_threshold.* were both supplied")
  

  
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
  skip_on_cran()
  skip_on_circleci(2)
  
  sample_file_1314_copy <- copy(.sample_file_1314())
  original <- income_tax(sample_file_1314_copy$Taxable_Income,
                         fy.year = "2013-14",
                         .dots.ATO = copy(sample_file_1314_copy))
  
  expect_warning(model_income_tax(sample_file_1314_copy,
                                  baseline_fy = "2013-14",
                                  
                                  # In 2013-14, the rate was 0.015
                                  medicare_levy_rate = 0.02), 
                 regexp = "medicare_levy_upper_threshold = 25677")
  
  sample_file_1314_if_2pc_ML <-
    sample_file_1314_copy %>%
    copy %>%
    .[, old_tax := income_tax(Taxable_Income, "2013-14", .dots.ATO = .)] %>%
    .[, new_tax := model_income_tax(sample_file_1314_copy,
                                    baseline_fy = "2013-14",
                                    medicare_levy_upper_threshold = 25678,
                                    medicare_levy_upper_sapto_threshold = 40348,
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
               regexp = "`medicare_levy_lower_sapto_threshold.*` and `medicare_levy_upper_sapto_threshold.*` were both supplied, but imply a Medicare taper rate of")
  
})


test_that("Medicare families", {
  skip_on_cran()
  skip_on_circleci(2)
  s1617 <- project(.sample_file_1314(), h = 3L)
  
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
                     # ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.47), # temp budget repair levy
                     medicare_levy_lower_family_threshold = 35000,
                     medicare_levy_upper_family_threshold = 43750,
                     return. = "tax")
  single_idx <- which(s1617$Spouse_adjusted_taxable_inc == 0 & !s1617$Partner_status)
  expect_equal(s1617_orig$orig_tax[single_idx],
               s1617_modelled[single_idx], 
               tol = 2, scale = 1)
  
})

test_that("SAPTO modelled", {
  skip_on_cran()
  skip_on_circleci(2)
  sample_file_1415_copy <- copy(.sample_file_1415())
  
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
                     medicare_levy_upper_sapto_threshold = 48415,
                     medicare_levy_lower_sapto_threshold = 32277,
                     medicare_levy_lower_family_threshold = 34367,
                     medicare_levy_upper_family_threshold = 51551,
                     medicare_levy_upper_family_sapto_threshold = 69000)
  
  
  sapto_only_psnn <-
    copy(.sample_file_1415()) %>%
    project_to(to_fy = "2016-17") %>%
    .[, saptoEligible := and(age_range <= 1L, Aust_govt_pnsn_allw_amt > 1)] %>%
    # The partner contribution makes the extra revenue much higher
    # HP 2022-05-27
    .[, Partner_status := FALSE] %>%
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
  skip_on_cran()
  skip_on_circleci(2)
  sample_file_1213_copy <- copy(.sample_file_("1213"))
  
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
  skip_on_circleci(2)
  
  s12131314 <- 
    copy(.sample_file_("1213")) %>%
    .[Ind %% 3 == 0] %>%
    setkey(Ind)
  
  no_elasticity <- 
    model_income_tax(copy(s12131314),
                     "2016-17",
                     medicare_levy_rate = 0.025, 
                     medicare_levy_lower_threshold = 22499,
                     medicare_levy_upper_threshold = 30e3, 
                     medicare_levy_upper_sapto_threshold = 44984,
                     medicare_levy_lower_sapto_threshold = 33737,
                     medicare_levy_upper_family_threshold = 48001,
                     medicare_levy_lower_family_threshold = 36000,
                     medicare_levy_upper_family_sapto_threshold = 62621,
                     medicare_levy_lower_family_sapto_threshold = 46965,
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
                     medicare_levy_lower_sapto_threshold = 33737,
                     medicare_levy_lower_family_threshold = 36000,
                     medicare_levy_upper_sapto_threshold = 44984,
                     medicare_levy_lower_family_sapto_threshold = 46965,
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
                     medicare_levy_lower_sapto_threshold = 33737,
                     medicare_levy_upper_sapto_threshold = 44984,
                     medicare_levy_lower_family_threshold = 36000,
                     medicare_levy_upper_family_threshold = 48001,
                     medicare_levy_lower_family_sapto_threshold = 46965,
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
  skip_on_circleci(2)
  s1415 <- 
    copy(.sample_file_("1415_synth")) %>%
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

test_that("Lamington", {
  skip_on_cran()
  skip_if_not_installed("fst", minimum_version = "0.8.4")
  skip_on_circleci(2)
  temp.fst <- "~/SampleFile1819/sample_file_1819.fst"
  library(data.table)
  library(hutils)
  library(magrittr)
  if (!exists("sample_file_1819") || !is.data.table(sample_file_1819)) {
    if (!file.exists("~/SampleFile1819/sample_file_1819.fst")) {
      temp.fst <- tempfile(fileext = ".fst")
      download.file('https://github.com/HughParsonage/SampleFile1819/raw/master/sample_file_1819.fst',
                    mode = "wb", 
                    destfile = temp.fst,
                    quiet = TRUE)
    }
    sample_file_1819 <- fst::read_fst(temp.fst, as.data.table = TRUE)
  }
  
  skip_if_not(exists("sample_file_1819"))
  if (!"Ind" %in% names(sample_file_1819)) {
    sample_file_1819[, Ind := .I]
  }
  
  
  s1819_Budget2018_lamington <- 
    model_income_tax(sample_file_1819,
                     baseline_fy = "2017-18",
                     ordinary_tax_rates = c(0, 0.19, 0.325, 0.37, 0.45),
                     ordinary_tax_thresholds = c(0, 18200, 37e3, 90e3, 180e3),
                     Budget2018_lamington = TRUE) %>%
    .[, .(Taxable_Income, new_tax, baseline_tax)] %>%
    .[, delta := as.integer(new_tax) - as.integer(baseline_tax)] %>%
    setkey(Taxable_Income) %>%
    .[]
  # https://budget.gov.au/2018-19/content/incometax.html
  expect_equal(coalesce(as.double(s1819_Budget2018_lamington[.(c(23000, 37000, 50e3, 91e3, 200e3)), mult="first"][["delta"]]),
                        c(-200, -200, -530, -650, -135)),
               c(-200, -200, -530, -650, -135))
  
})

test_that("Clear columns", {
  skip_on_cran()
  skip_on_circleci(2)
  s1314 <- as.data.table(.sample_file_1314())
  s1314[, new_tax := 1][, baseline_tax := 2]
  res <- model_income_tax(s1314, "2013-14", clear_tax_cols = TRUE)
  expect_true(res[, max(new_tax)] > 1)
})

test_that("sample_file.int", {
  skip_on_cran()
  skip_on_circleci(2)
  s1314 <- as.data.table(.sample_file_1314())
  res <- model_income_tax(s1314, "2013-14", return. = "sample_file.int")
  expect_identical(res[["new_tax"]], res[["baseline_tax"]])
})




test_that("Keyed data.table", {
  skip_on_cran()
  skip_on_circleci(2)
  s1314 <- as.data.table(.sample_file_1314())
  base <- model_income_tax(s1314, "2013-14", sbto_discount = 0.1)
  setkey(s1314, Taxable_Income)
  new <- model_income_tax(s1314, "2013-14", sbto_discount = 0.1)
  setkey(new, NULL)
  expect_identical(base[order(Taxable_Income)], new)
})

test_that("Budget2018", {
  skip_on_cran()
  
  skip_on_circleci(2)
  
  library(hutilscpp)
  s1314 <- as.data.table(.sample_file_1314())
  sWatr <- model_income_tax(s1314,
                            baseline_fy =  "2013-14",
                            Budget2018_watr = TRUE,
                            return. = "sample_file.int")
  out_10k <- 
    sWatr[, .(delta = mean(new_tax - baseline_tax)),
          keyby = .(Income = round(Taxable_Income, -4))]
  expect_lt(abs_diff(out_10k[.(70e3), delta], -928), 2.5)
  expect_equal(out_10k[.(140e3), delta], 0)
  
  
  sL2223 <- model_income_tax(s1314,
                             baseline_fy =  "2013-14",
                             Budget2018_lito_202223 = TRUE,
                             return. = "sample_file.int")
  sL2223[, delta := new_tax - baseline_tax]
  expect_equal(sL2223[, min(delta)], -200)
  
  sL2223_10k <- 
    sL2223[, .(delta = mean(new_tax - baseline_tax)),
           keyby = .(Income = round(Taxable_Income, -4))]
  expect_equal(sL2223_10k[delta < 0, Income], (2:4) * 10e3)
  
})



test_that("CGT discount", {
  skip_on_cran()
  
  
  skip_on_circleci(2)
  library(data.table)
  s12131314 <- copy(.sample_file_("1213"))
  baseline <- model_income_tax(s12131314,
                               "2013-14",
                               ordinary_tax_thresholds = c(0, 20e3, 37e3, 80e3, 180e3))
  
  if (getRversion() < "3.5.0") {
    # sum doesn't coerce to double to avoid overflow
    baseline[, new_tax := as.double(new_tax)]
    baseline[, baseline_tax := as.double(baseline_tax)]
  }
  
  # Surprisingly didn't fail on recent versions of R
  expect_lt(baseline[, sum(new_tax)], 
            baseline[, sum(baseline_tax)])
  
  la_meme <-
    model_income_tax(s12131314,
                     "2013-14",
                     ordinary_tax_thresholds = c(0, 20e3, 37e3, 80e3, 180e3),
                     cgt_discount_rate = 0.5)
  
  expect_equal(baseline[["new_tax"]],
               model_income_tax(s12131314,
                                "2013-14",
                                ordinary_tax_thresholds = c(0, 20e3, 37e3, 80e3, 180e3),
                                cgt_discount_rate = 0.5,
                                return. = "tax"))
  # TES 2015-16: 5160 for full discount
  s1516_orig <- copy(.sample_file_1516())
  s1516 <- model_income_tax(copy(s1516_orig)[, Partner_status := 0L], 
                            baseline_fy = "2015-16",
                            cgt_discount_rate = 0.0)
  s1516[, WEIGHT := 50L]
  # was $6.0919 billion in grattan pre 2022-06-05
  # probably due to Partner_status being treated as single for ML etc
  expect_lte(abs(revenue_foregone(s1516) - 6150e6) / 6150e6, 0.05)
  
  
  expect_lt(baseline[, sum(new_tax)],
            sum(model_income_tax(s12131314,
                                 "2013-14",
                                 ordinary_tax_thresholds = c(0, 20e3, 37e3, 80e3, 180e3),
                                 cgt_discount_rate = 0.4,
                                 return. = "tax")))
  s1516_totally_discounted <- 
    model_income_tax(copy(s1516_orig),
                     baseline_fy = "2015-16",
                     cgt_discount_rate = 1.0)
  expect_false(anyNA(s1516_totally_discounted[["new_tax"]]),
               label = "High CGT discount should not cause NAs in new_tax.")
  expect_gte(s1516_totally_discounted[, min(Taxable_Income, na.rm = TRUE)], 0,
             label = "High CGT discount should not introduce negative Taxable_Incomes.")
  
  prev_revenue_foregone <- revenue_foregone(s1516)
  
  # No discount if no capital gains
  s1516[, Net_CG_amt := 0L]
  s1516 <- model_income_tax(s1516, 
                            baseline_fy = "2015-16",
                            cgt_discount_rate = 0.0)
  
  expect_lt(revenue_foregone(s1516), prev_revenue_foregone)
  
  
})


test_that("CGT (errors)", {
  skip_on_cran()
  skip_on_circleci(2)
  expect_error(model_income_tax(.sample_file_("1213"), "2013-14", 
                                cgt_discount_rate = rep(0, 5)),
               regexp = "length(cgt_discount_rate) = 5",
               fixed = TRUE)
  expect_error(model_income_tax(.sample_file_("1213"), "2013-14", 
                                cgt_discount_rate = "x"),
               regexp = "`cgt_discount_rate` was type character",
               fixed = TRUE)
})

test_that("SAPTO parameters should not go out of range (causing NAs)", {
  skip_on_cran()
  skip_on_circleci(2)
  library(data.table)
  library(hutils)
  s1314 <- as.data.table(.sample_file_1314())
  m1314_27000 <- model_income_tax(s1314, "2016-17", sapto_lower_threshold = 27000)
  expect_false(anyNA(m1314_27000[["new_tax"]]))
})

test_that("Issue #176", {
  skip_on_circleci(3)
  # Just no error
  expect_silent(model_income_tax(copy(.sample_file_1314()), 
                                 "2016-17", 
                                 medicare_levy_lower_family_sapto_threshold = 42000,
                                 medicare_levy_upper_family_sapto_threshold = 52500))
  expect_true(TRUE)
})

test_that("SAPTO modelling done for Age of entitlement report", {
  skip_on_cran()
  skip_on_circleci(2)
  library(data.table)
  library(hutils)
  library(hutilscpp)
  s1718_AgeOfEntitlement <-
    project(.sample_file_1314(), 
            h = 4L) %>%
    model_income_tax("2017-18", 
                     sapto_lower_threshold = 27e3,
                     sapto_lower_threshold_married = 42e3,
                     sapto_max_offset = 1160,
                     sapto_max_offset_married = 390,
                     medicare_levy_lower_sapto_threshold = 27000,
                     medicare_levy_upper_sapto_threshold = 33750,
                     medicare_levy_upper_family_threshold = 46361,
                     medicare_levy_lower_family_sapto_threshold = 42000,
                     medicare_levy_upper_family_sapto_threshold = 52500)
  expect_lte(abs(revenue_foregone(s1718_AgeOfEntitlement) - 400e6), 50e6)
})



