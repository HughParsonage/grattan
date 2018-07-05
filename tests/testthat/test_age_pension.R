context("Age pension")

test_that("Maximum rates", {
  # http://guides.dss.gov.au/guide-social-security-law/5/2/2/10
  expect_equal(age_pension(fy.year = "2015-16", has_partner = FALSE), 20745.40)
  expect_equal(age_pension(fy.year = "2015-16", has_partner = TRUE), 15576.60)
  expect_equal(age_pension(Date = "2016-03-20", has_partner = FALSE), 20745.40)
  expect_equal(age_pension(Date = "2016-03-20", has_partner = TRUE), 15576.60)
})

test_that("Means testing", {
  skip("No")
})
