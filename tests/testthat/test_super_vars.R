context("Superannuation variables")

library(taxstats)
library(magrittr)


test_that("Div293 tax is bounded by cap @ 25k", {
  cap1 <- 25e3
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, cap = cap1, age_based_cap = FALSE)
  expect_true(all(new_sample_file$div293_tax <= cap1 * 0.15 + .Machine$double.eps ^ 0.5))
})
test_that("Div293 tax is bounded by cap @ 20k", {
  cap1 <- 20e3
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, cap = cap1, age_based_cap = FALSE)
  expect_true(all(new_sample_file$div293_tax <= cap1 * 0.15 + .Machine$double.eps ^ 0.5))
})
test_that("Div293 tax is bounded by cap @ 30k", {
  cap1 <- 30e3
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, cap = cap1, age_based_cap = FALSE)
  expect_true(all(new_sample_file$div293_tax <= cap1 * 0.15 + .Machine$double.eps ^ 0.5), info = as.character(paste0("cap1 = ", cap1)))
})

test_that("Div293 tax is bounded by an arbitrary cap", {
  caps <- sort(abs(rcauchy(2, location = 30e3, scale = 20e3)))
  cap1 <- caps[1]
  cap2 <- caps[2]
  
  age_based_cap <- sample(c(TRUE, FALSE), size = 1)
  div293_threshold <- abs(rcauchy(1, 300e3, 100e3))
  cap2_age <- sample(25:65, size = 1)
  
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, 
                                                 cap = cap1, cap2 = cap2, age_based_cap = age_based_cap, 
                                                 div293_threshold = div293_threshold, cap2_age = cap2_age)
  expect_true(all(new_sample_file$div293_tax <= new_sample_file$concessional_cap * 0.15 + .Machine$double.eps ^ 0.5), info = as.character(paste0("cap1 = ", cap1)))
})

# Adjusted Taxable Income (for surcharge purposes < 300e3)
test_that("Surchargeable income and low tax contributions less than 300,000 implies no Div293 tax", {
  caps <- sort(abs(rcauchy(2, location = 30e3, scale = 20e3)))
  cap1 <- caps[1]
  cap2 <- caps[2]
  
  age_based_cap <- sample(c(TRUE, FALSE), size = 1)
  div293_threshold <- abs(rcauchy(1, 300e3, 100e3))
  cap2_age <- sample(25:65, size = 1)
  
  new_sample_file <- apply_super_caps_and_div293(sample_file_1314, 
                                                 cap = cap1, cap2 = cap2, age_based_cap = age_based_cap, 
                                                 div293_threshold = div293_threshold, cap2_age = cap2_age)
  
  # cap1 = 82481.0762025714
  # cap2 = 12349.9120592203
  # age_based_cap = TRUE
  # div293_threshold = 177262.550400898
  # cap2_age = 65
  
  expect_true(all(new_sample_file[surchargeable_income_div293 + low_tax_contributions_div293 <= div293_threshold][["div293_tax"]] <= .Machine$double.eps ^ 0.5), 
              info = as.character(paste0("cap1 = ", cap1, "\n", 
                                         "cap2 = ", cap2, "\n",
                                         "age_based_cap = ", age_based_cap, "\n", 
                                         "div293_threshold = ", div293_threshold, "\n", 
                                         "cap2_age = ", cap2_age)))
})

test_that("Counts for Div 293 at 250e3 not at odds with PBO", {
  sample_file_1718 <- 
    sample_file_1314 %>%
    project_to(to_fy = "2017-18", fy.year.of.sample.file = "2013-14")
  
  n_adversely_affected_201718 <- 
    n_affected_from_new_cap_and_div293(.sample.file = sample_file_1718, 
                                       # PBO issued estimate in 2015-16
                                       fy.year = "2015-16", 
                                       new_cap = 30e3, new_cap2 = 35e3, new_age_based_cap = TRUE, 
                                       new_cap2_age = 49,
                                       new_ecc = FALSE, 
                                       new_div293_threshold = 250e3, 
                                       
                                       use_other_contr = FALSE,
                                       prv_cap = 30000, prv_cap2 = 35000, prv_age_based_cap = TRUE,
                                       prv_cap2_age = 49, 
                                       prv_ecc = FALSE, 
                                       prv_div293_threshold = 300e3)
  # https://www.facebook.com/LDP.australia/photos/pcb.10152975190247672/10152975189552672/?type=3&theater
  # PBO has number affected by a change to 250e3 as
  # 2017-18   2018-19   2019-20
  # 110,000   130,000   150,000
  expect_true(between(n_adversely_affected_201718, 70e3, 130e3))
})

context("Reweighting and imputation successfully reconcile aggregates")

test_that("Imputed, reweighted sample file agrees with aggregates by no less than 1%", {
  library("taxstats")
  library("data.table")
  library("dplyr")
  library("dtplyr")
  library("magrittr")
  
  funds <- 
    funds_table1_201314 %>%
    filter(Selected_items == "Assessable contributions") %>%
    select(fy_year, Assessable_contributions_funds = Sum) %>%
    setkey(fy_year)
  
  smsfs <- 
    funds_table2_smsf_201314 %>%
    filter(Selected_items == "Assessable contributions") %>%
    select(fy_year, Assessable_contributions_smsfs = Sum) %>%
    setkey(fy_year)
  
  ato_aggregate_contributions <- 
    smsfs[funds] %>%
    mutate(total_contributions = Assessable_contributions_smsfs + Assessable_contributions_funds)
    
  
  # Now test imputation using defaults.
  imputed_concessional_contributions <- 
    sample_file_1314 %>%
    mutate(WEIGHT = 50) %>%
    apply_super_caps_and_div293(reweight_late_lodgers = TRUE, impute_zero_concess_contr = TRUE) %$%
    sum(concessional_contributions * WEIGHT)
  
  percentage_difference <- 
    100 * abs(imputed_concessional_contributions / ato_aggregate_contributions[fy_year == "2013-14"][["total_contributions"]] - 1)
  
  expect_lt(percentage_difference, 1)
})

test_that("Error handling", {
  sample_file <- sample_file_1314 %>% head(.) %>% as.data.frame(.)
  expect_error(apply_super_caps_and_div293(sample_file), regexp = "data.table")
  
  sample_file_dt <- sample_file_1314 %>% copy %>% head %>% mutate(concessional_cap = 25e3)
  expect_warning(apply_super_caps_and_div293(sample_file_dt))
  
  expect_warning(apply_super_caps_and_div293(sample_file_dt, colname_new_Taxable_Income = "Taxable_Income"), 
                 regexp = "Dropping Taxable.Income")
  
  expect_error(apply_super_caps_and_div293(sample_file_1213), regexp = "does not have the variables needed")
  
  expect_warning(apply_super_caps_and_div293(sample_file_dt, colname_div293_tax = "Sw_amt"))
  
  sample_file_old <- sample_file_dt %>% copy %>% select(-Rptbl_Empr_spr_cont_amt)
  
  expect_error(apply_super_caps_and_div293(sample_file_old), regexp = "required to impute") 
  
  expect_warning(apply_super_caps_and_div293(sample_file_dt, reweight_late_lodgers = TRUE), regexp = "WEIGHT")
  
})


test_that("Corner cases", {
  n_low_age <- 
    sample_file_1314 %>%
    apply_super_caps_and_div293(cap2_age = 19) %$%
    sum(concessional_cap == max(concessional_cap))
    
  n_high_age <- 
    sample_file_1314 %>%
    apply_super_caps_and_div293(cap2_age = 68) %$%
    sum(concessional_cap == max(concessional_cap))
  
  expect_gte(n_low_age, n_high_age)
  
  expect_false("div293_income" %in% names(apply_super_caps_and_div293(sample_file_1314, drop_helpers = TRUE)))
  
  low_tax_contributions_no_Other_contr <- 
    sample_file_1314 %>%
    apply_super_caps_and_div293 %$%
    sum(low_tax_contributions_div293)
  
  low_tax_contributions_with_Other_contr <- 
    sample_file_1314 %>%
    apply_super_caps_and_div293(use_other_contr = TRUE) %$%
    sum(low_tax_contributions_div293)
  
  expect_gte(low_tax_contributions_with_Other_contr, low_tax_contributions_no_Other_contr)
})




