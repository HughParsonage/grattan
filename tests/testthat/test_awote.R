context("AWOTE")

test_that("Error handling", {
  expect_error(awote(isMale = "yes"))
  expect_error(awote(isAdult = "yes"))
  expect_error(awote(isOrdinary = "yes"))
  expect_error(awote(isAdult = NA))
  expect_error(awote(isOrdinary = NA))
})

test_that("Message", {
  expect_message(awote(), 
                 regexp = "both NULL so using")
})

test_that("AWOTE fy", {
  library(data.table)
  expect_true(between(awote(Date = fy2date("2013-14")),
                      awote(fy.year = "2012-13"),
                      awote(fy.year = "2014-15")))
  expect_equal(awote_fy("2014-15", isMale = NA, isAdult = TRUE, isOrdinary = TRUE),
               awote(fy.year = "2014-15"))
})

test_that("AWOTE unsorted, issue #204", {
  ufys <- yr2fy(2015:2014)
  # Reversing inputs same as referring outputs
  expect_equal(awote(fy.year = rev(ufys)),
               rev(awote(fy.year = ufys)))
  
  udates <- paste0(2015:2014, "-01-01")
  expect_equal(awote(Date = rev(udates)),
               rev(awote(Date = udates)))
  
  withr::with_seed(602, {
    library(data.table)
    # Create a combination of inputs, randomly order them
    # and ensure the collected inputs match the inputs alone
    
    DT <- CJ(fy.year = yr2fy(2013:2015),
             isMale = c(NA, TRUE, FALSE),
             isAdult = c(TRUE, FALSE), 
             isOrdinary = c(TRUE, FALSE))
    DT[, awote_ordered := awote(fy.year = fy.year, 
                                isMale = isMale,
                                isAdult = isAdult,
                                isOrdinary = isOrdinary)]
    myorder <- sample(1:nrow(DT))
    DT2 <- DT[myorder]
    DT2[, this_awote := awote(fy.year = fy.year, 
                              isMale = isMale, 
                              isAdult = isAdult,
                              isOrdinary = isOrdinary)]
    first_awote <- awote(fy.year = .subset2(DT2, "fy.year")[1],
                         isMale = .subset2(DT2, "isMale")[1],
                         isAdult = .subset2(DT2, "isAdult")[1],
                         isOrdinary = .subset2(DT2, "isOrdinary")[1])
    expect_equal(first_awote, DT2[["this_awote"]][1])
    expect_equal(.subset2(DT2, "this_awote"),
                 .subset2(DT2, "awote_ordered"))
  })
  
  
})

