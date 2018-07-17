context("Age pension age")

test_that("Error handling", {
  expect_error(age_pension_age(sex = NULL), 
               "must be a character vector.")
  expect_message(age_pension_age(), "`when` not set")
  expect_error(age_pension_age(when = c("2015-16", "2015-16"), 
                               sex = c("m", "f", "m")), 
               "`sex` must be length-one or have the same length as `when`.")
  expect_error(age_pension_age(sex = c("male", "baz")), 
               "position 2.*male.*female")
  expect_error(age_pension_age(when = c("14/14/2015")), 
               "during coercion")
})

test_that("Women before 1995", {
  # http://guides.dss.gov.au/guide-social-security-law/3/4/1/10
  library(data.table)
  women_before_52 <- 
    data.table(sex = "female", 
               date = c("1995-07-01", "2003-07-01", "2000-07-01", "2000-06-30", "1990-01-01"))
  res <- women_before_52[, age_pension_age(when = date, sex = sex)]
  expect_equal(res, c(60.5, 62.5, 61.5, 61.5, 60))
  
  res2 <- women_before_52[, age_pension_age(when = as.Date(date), sex = sex)]
  expect_identical(res, res2)
  
  res3 <- women_before_52[, age_pension_age(when = date2fy(as.Date(date)), sex = sex)]
  expect_identical(res, res3)
  
  
})

