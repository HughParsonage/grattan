# Independent full-year resident scales from ATO tables and Parliamentary Library
# Table 3, plus the enacted post-2024 rates. See inst/law-audit-sources.txt.
# Expected schedules are transcribed independently of the native C initializers.
test_that("resident schedules and separate levies match independent annual scales", {
  scales <- list()
  add <- function(years, thresholds, rates) {
    for (yr in years) {
      scales[[as.character(yr)]] <<- list(b = c(0, thresholds), r = c(0, rates))
    }
  }
  add(1984, c(4595,19500,35788), c(.30,.46,.60))
  add(1985, c(4595,12500,19500,28000,35000,35788), c(.2667,.30,.46,.4733,.5533,.60))
  add(1986, c(4595,12500,19500,28000,35000), c(.25,.30,.46,.48,.60))
  add(1987, c(4890,12500,12600,19500,28000,35000), c(.2442,.265,.2942,.4425,.4683,.5708))
  add(1988:1989, c(5100,12600,19500,35000), c(.24,.29,.40,.49))
  add(1990, c(5100,17650,20600,35000,50000), c(.21,.29,.39,.47,.48))
  add(1991, c(5250,17650,20600,20700,35000,36000,50000), c(.205,.245,.295,.385,.425,.465,.47))
  add(1992:1993, c(5400,20700,36000,50000), c(.20,.38,.46,.47))
  add(1994, c(5400,20700,36000,38000,50000), c(.20,.355,.385,.44125,.47))
  add(1995:2000, c(5400,20700,38000,50000), c(.20,.34,.43,.47))
  add(2001:2003, c(6000,20000,50000,60000), c(.17,.30,.42,.47))
  add(2004, c(6000,21600,52000,62500), c(.17,.30,.42,.47))
  add(2005, c(6000,21600,58000,70000), c(.17,.30,.42,.47))
  add(2006, c(6000,21600,63000,95000), c(.15,.30,.42,.47))
  add(2007, c(6000,25000,75000,150000), c(.15,.30,.40,.45))
  add(2008, c(6000,30000,75000,150000), c(.15,.30,.40,.45))
  add(2009, c(6000,34000,80000,180000), c(.15,.30,.40,.45))
  add(2010, c(6000,35000,80000,180000), c(.15,.30,.38,.45))
  add(2011:2012, c(6000,37000,80000,180000), c(.15,.30,.37,.45))
  add(2013:2016, c(18200,37000,80000,180000), c(.19,.325,.37,.45))
  add(2017:2018, c(18200,37000,87000,180000), c(.19,.325,.37,.45))
  add(2019:2020, c(18200,37000,90000,180000), c(.19,.325,.37,.45))
  add(2021:2024, c(18200,45000,120000,180000), c(.19,.325,.37,.45))
  add(2025:2026, c(18200,45000,135000,190000), c(.16,.30,.37,.45))
  add(2027, c(18200,45000,135000,190000), c(.15,.30,.37,.45))
  add(2028:2030, c(18200,45000,135000,190000), c(.14,.30,.37,.45))
  ordinary <- function(x, b, r) {
    vapply(x, function(z) sum(pmax(0, pmin(z, c(b[-1], Inf)) - b) * r), numeric(1))
  }
  legal_levies <- function(x, yr) {
    if (yr == 2012L) {
      .005 * pmax(0, pmin(x, 100000) - 50000) + .01 * pmax(0, x - 100000)
    }
    else if (yr %in% 2015:2017) {
      .02 * pmax(0, x - 180000)
    }
    else {
      rep(0, length(x))
    }
  }
  for (yr in 1984:2030) {
    s <- scales[[as.character(yr)]]
    incomes <- sort(unique(c(0, pmax(0, s$b - 1), s$b, s$b + 1,
                             49999, 50000, 50001, 99999, 100000, 100001,
                             179999, 180000, 180001, 200000)))
    system <- System(yr, fix = 2L)
    system$Medicare$rate <- 0
    system$Offsets <- list()
    expected <- ordinary(incomes, s$b, s$r) + legal_levies(incomes, yr)
    expect_equal(income_tax(incomes, yr2fy(yr), age = 42, System = system),
                 expected, info = paste("year ending", yr), tolerance = 1e-10)
  }
})
