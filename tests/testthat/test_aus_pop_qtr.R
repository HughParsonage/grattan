context("Aus pop qtr")

test_that("Returns correct values", {
  skip("Slight differences temporarily")
  expect_equal(aus_pop_qtr("2016-Q1"), 24122701)
  expect_gte(aus_pop_qtr("2018-Q1"), 24122701)
  expect_gte(aus_pop_qtr("2018-Q4"), 25e6)
  # by age
  
  expect_equivalent(grattan:::aust_pop_by_age_yearqtr, aus_pop_qtr_age(tbl = TRUE))
  expect_equal(aus_pop_qtr_age(age = 1)[1:139], c(226775L, 228838L, 230676L, 232589L, 233901L, 235023L, 236046L, 
                                           237097L, 238209L, 239285L, 240219L, 241246L, 241007L, 240648L, 
                                           240264L, 239865L, 240552L, 241269L, 241865L, 242597L, 242144L, 
                                           241668L, 241309L, 241123L, 242094L, 242863L, 243728L, 244292L, 
                                           245486L, 246449L, 247244L, 247817L, 248769L, 249652L, 250492L, 
                                           251302L, 252949L, 254438L, 256051L, 257523L, 258352L, 258949L, 
                                           259498L, 259878L, 259595L, 259090L, 258538L, 257688L, 257979L, 
                                           258087L, 258236L, 258426L, 258483L, 258180L, 257992L, 258057L, 
                                           258215L, 258393L, 258260L, 258399L, 257840L, 257119L, 256177L, 
                                           255188L, 255039L, 254899L, 254705L, 254452L, 253480L, 252544L, 
                                           251516L, 250579L, 250989L, 251449L, 251866L, 252358L, 252563L, 
                                           252868L, 253285L, 253737L, 253484L, 253209L, 252893L, 252634L, 
                                           252156L, 251629L, 251016L, 250356L, 250391L, 250477L, 250464L, 
                                           250613L, 251432L, 252241L, 253063L, 253896L, 255230L, 256526L, 
                                           257695L, 258624L, 261571L, 264477L, 267518L, 270527L, 274659L, 
                                           278285L, 281747L, 284625L, 286620L, 288335L, 290044L, 291342L, 
                                           292237L, 292749L, 293227L, 293410L, 294011L, 294311L, 294550L, 
                                           294616L, 294397L, 293969L, 293637L, 293110L, 296848L, 300623L, 
                                           304219L, 307681L, 308917L, 309894L, 310986L, 311858L, 310855L, 
                                           309617L, 308503L, 307228L, 307658L, 307941L, 308384L),
               tol = 5000,
               scale = 1)
  expect_equal(aus_pop_qtr_age(date = as.Date("2015-01-01")), 
               c(309617L, 312599L, 306541L, 303699L, 305330L, 303537L, 300446L, 
                 297943L, 293234L, 286061L, 282031L, 279984L, 281517L, 284300L, 
                 285830L, 287508L, 291708L, 299841L, 311240L, 318711L, 321363L, 
                 327558L, 338210L, 348732L, 350418L, 347623L, 348155L, 351251L, 
                 354769L, 354764L, 353178L, 347888L, 339953L, 330734L, 320947L, 
                 314057L, 310080L, 309487L, 311774L, 317426L, 324486L, 334968L, 
                 344388L, 337275L, 325377L, 316584L, 306372L, 302143L, 302689L, 
                 307728L, 313202L, 314544L, 313673L, 307614L, 299841L, 293950L, 
                 286990L, 281536L, 275374L, 266933L, 261028L, 254757L, 248233L, 
                 243826L, 237515L, 234429L, 239057L, 224090L, 200610L, 191115L, 
                 176760L, 165396L, 157278L, 147678L, 140354L, 132014L, 124122L, 
                 117245L, 109200L, 100965L, 94738L, 89194L, 85082L, 80922L, 74057L, 
                 66857L, 59752L, 52432L, 45257L, 38161L, 31289L, 25353L, 20199L, 
                 15161L, 10386L, 6943L, 5041L, 3669L, 2590L, 4130L), 
               tol = 10e3,
               scale = 1)
})

test_that("Multiple unordered", {
  expect_equal(aus_pop_qtr_age(date = as.Date(c("2015-12-01", "2013-12-01", "2014-12-01")), 
                               age = c(2, 1, 3)), 
               c(311984, 309894, 306541), 
               tol = 5000,
               scale = 1)
})

test_that("Rolls work as expected", {
  expect_equal(aus_pop_qtr_age(date = as.Date("2015-01-01")), 
               aus_pop_qtr_age(date = as.Date("2015-01-02"), roll = TRUE))
  
  
})

test_that("Error handling", {
  expect_error(aus_pop_qtr("2016 Q0"), "Entry 1 was not in the correct form")
  expect_error(aus_pop_qtr(c("2016 Q0", "2016-Q2", "19999")), "Entries 1 and 3 were not in the correct form")
  expect_error(aus_pop_qtr(c("2016 Q0", "2016-Q-", "19999")), "There were 2 other bad entries.")
  expect_error(aus_pop_qtr(c("2016 Q0", "2016 q1")), "Entries 1 and 2 were not in the correct form")
  
  expect_error(aus_pop_qtr_age(age = "45"))
  expect_error(aus_pop_qtr_age(age = 101))
  expect_error(aus_pop_qtr_age(age = -99))
  expect_warning(aus_pop_qtr_age(date = as.Date("2099-01-01"), age = 1, roll = TRUE, roll.beyond = FALSE))
  expect_warning(aus_pop_qtr("2050-Q1", allow.projections = FALSE),
                 regexp = "Using an earlier date than specified")
  expect_error(aus_pop_qtr_age(age = 50:52, date = rep(as.Date("2015-01-01"), 2)), 
               regexp = "`date` and `age` can only have different lengths when the smaller length is 1.", 
               fixed = TRUE)
  expect_error(aus_pop_qtr_age(age = 50:51, date = rep(as.Date("2015-01-01"), 3)), 
               regexp = "`date` and `age` can only have different lengths when the smaller length is 1.", 
               fixed = TRUE)
})
