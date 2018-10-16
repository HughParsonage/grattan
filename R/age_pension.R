#' Age pension
#' 
#' @param fortnightly_income,annual_income Income for means-testing purposes. Provide one but not both.
#' @param has_partner (logical, default: \code{FALSE}) Does the individual have a partner?
#' @param n_dependants How many dependants does the individual have? Default is zero.
#' @param partner_fortnightly_income,partner_annual_income The partner's income. The sum of this value and the indiviudal's income gives the income test.
#' @param partner_pensioner (logical, default: \code{TRUE}) Is the individual's partner also in receipt of the age pension?
#' @param Date,fy.year The financial year. Currently only 2015-16 is supported (the most recent survey of income and housing results).
#' @param assets_value Total value of household assets.
#' @param financial_assets Assets which earn incomes for which deeming rates apply.
#' @param is_home_owner (logical, default: \code{FALSE}) Does the individual own their own home? 
#' @param illness_separated_couple Is the couple separated by illness? (Affects the assets test.)
#' @param per Specifies the timeframe in which payments will be made. One of \code{"year"} and \code{"fortnight"}.
#' 
#' @details
#' Currently does not include the age pension supplement.
#' @return Returns the age pension payable for each individual defined by the 
#' arguments, assuming otherwise eligible.
#' 
#' 
#' 
#' @export age_pension
#' 


age_pension <- function(fortnightly_income = 0, 
                        annual_income = fortnightly_income * 26, 
                        has_partner = FALSE,
                        n_dependants = 0L,
                        partner_fortnightly_income = 0,
                        partner_annual_income = partner_fortnightly_income * 26,
                        partner_pensioner = has_partner,
                        Date = NULL,
                        fy.year = NULL,
                        assets_value = 0,
                        financial_assets = 0,
                        is_home_owner = FALSE,
                        illness_separated_couple = FALSE,
                        per = c("year", "fortnight")) {
  args <- ls(sorted = FALSE)  # sorted = FALSE => in order of args
  
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      Date <- .age_pension_today2qtr(Sys.Date())
      message('`Date` and `fy.year` not set, so using `Date = "', Date, '".')
    } else {
      Date <- fy2date(fy.year)
    }
  } else {
    if (!is.null(fy.year)) {
      warning("`fy.year` and `Date` both used. Ignoring `fy.year`.")
    }
  }
  
  for (i in args) {
    if (anyNA(get(i, inherits = FALSE))) {
      stop("`", i, "` contains NAs. Impute these values.")
    }
  }
  
  if (!missing(fortnightly_income)) {
    if (!missing(annual_income) &&
        !isTRUE(all.equal(annual_income,
                          fortnightly_income * 26))) {
      stop("`fortnightly_income` is provided, ", 
           "yet `annual_income` is not 26 times its values. ",
           "Provide one but not both.")
    }
  }
  
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(annual_income, 
                                        has_partner, 
                                        n_dependants,
                                        partner_annual_income,
                                        partner_pensioner,
                                        Date, 
                                        assets_value,
                                        financial_assets,
                                        is_home_owner)
  
  
  
  HH_Income <- 
    HasPartner <- 
    PartnerIncome <-
    Assets <- 
    HomeOwner <-
    IlnnessSeparated <- NULL
  
  input <- 
    data.table(HH_Income = annual_income + partner_annual_income, 
               HasPartner = has_partner, 
               n_dependants = n_dependants,
               PartnerPensioner = partner_pensioner,
               Date = as.Date(Date), 
               Assets = assets_value, 
               FinancialAssets = financial_assets,
               HomeOwner = is_home_owner,
               IllnessSeparated = illness_separated_couple)
  
  
  input[, "ordering" := .I]
  setkeyv(input, c("HasPartner", "Date"))
  
  max_rate <- NULL
  max_rates <-
    melt.data.table(Age_pension_base_rates_by_year,
                    id.vars = "Date", 
                    value.name = "max_rate", 
                    variable.factor = FALSE) %>%
    .[, HasPartner := variable == "Married rate"] %T>%
    # assertion that the names are correct
    .[, stopifnot(any(HasPartner) && !all(HasPartner))] %>%
    .[, Date := as.Date(Date)] %>%
    .[, variable := NULL] %>%
    setkeyv(c("HasPartner", "Date"))
  
  assets_test <- 
    Age_pension_assets_test_by_year %>%
    setkeyv(c("HasPartner", "IllnessSeparated", "HomeOwner", "Date"))
  
  stopifnot(hutils::has_unique_key(assets_test), 
            "assets_test" %in% names(assets_test))
  assets_excess <- NULL
  
  stopifnot("type" %in% names(Age_pension_permissible_income_by_Date))
  type <- NULL
  stopifnot("permissible_income" %in% names(Age_pension_permissible_income_by_Date))
  permissible_income <- NULL
  
  income_test <- 
    Age_pension_permissible_income_by_Date %>%
    .[, .(HasPartner = type == "Couple",
          Date = as.Date(Date),
          permissible_income = permissible_income)] %>%
    setkeyv(c("HasPartner", "Date"))
  
  A <- max_rates[input, on = c("HasPartner", "Date"), roll = Inf, nomatch=0L]
  B <- income_test[A, on = c("HasPartner", "Date"), roll = Inf, nomatch=0L]
  
  setkeyv(B, c("HasPartner", "IllnessSeparated", "HomeOwner", "Date"))
  C <- assets_test[B,
                   on = c("HasPartner", "IllnessSeparated", "HomeOwner", "Date"),
                   roll = Inf,
                   nomatch = 0L] 
  
  
  deeming_rate_above <- deemed_income <- FinancialAssets <- NULL
  
  # http://guides.dss.gov.au/guide-social-security-law/4/4/1/10
  deeming <- 
    Age_pension_deeming_rates_by_Date %>%
    # temp
    rbind(data.table(Date = as.Date("2017-07-01"), 
                     type = c("single", "couple", "nonpensioner couple"), 
                     threshold = c(50200, 83400, 41700),
                     deeming_rate_below = 0.0175,
                     deeming_rate_above = 0.0325)) %>%
    unique %>%
    .[, .(Date,
          HasPartner = type != "single",
          PartnerPensioner = type == "couple", 
          threshold, deeming_rate_below, deeming_rate_above)] %>%
    setkeyv(c("HasPartner", "PartnerPensioner", "Date")) %>%
    .[]
  
  D <- deeming[C, 
               on = c("HasPartner", "PartnerPensioner", "Date"), 
               roll = Inf, 
               nomatch = 0L]
  
  threshold <- deeming_rate_below <- NULL
  
  
  D[, deemed_income := deeming_rate_below * pminV(threshold, FinancialAssets)]
  D[FinancialAssets > threshold,
    deemed_income := deemed_income + deeming_rate_above * pmaxC(FinancialAssets - threshold, 0)]
  D[, HH_Income := HH_Income + deemed_income]
  
  # if (is_testing())print(A); print(B); print(C)
  age_pension_income <-
    age_pension_assets <- 
    assets_test <- 
    partner_test_reduction <-
    NULL
  
  out <- 
    D %>%
    .[, partner_test_reduction := (-1/2 * HasPartner + 1)] %>% 
    .[, age_pension_income := pminV(pmaxC(max_rate - partner_test_reduction * 0.5 * (HH_Income - (has_partner + 1) * permissible_income),
                                          0),
                                    max_rate)] %>%
    .[, assets_excess := pmaxC(Assets - assets_test, 0)] %>%
    .[, age_pension_assets := pminV(pmaxC(max_rate - partner_test_reduction * 19.5 * floor(assets_excess / 250), 0), max_rate)] %>%
    setorderv("ordering") %>%
    .[, pminV(age_pension_income, 
              age_pension_assets)]
  
  if (missing(per)) {
    message("`per` is missing. Using `per = ", per[1], "`. ", 
            "Set `per` explicitly to avoid this message.")
  }
  
  per <- per[1L]
  
  
  switch(per, 
         "fortnight" = out / 26,
         "year" = out,
         "annual" = out,
         stop('`per` must be one of "fortnight" or "annual".'))
  
  
}

.age_pension_today2qtr <- function(today) {
  the_year <- year(today)
  the_month <- month(today)
  the_day <- mday(today)
  if (OR(the_month < 3L,
         the_month == 3L && the_day < 20L)) {
    paste0(the_year - 1L, "-09-20")
  } else if (OR(the_month > 9L,
                the_month == 9L && the_day >= 20L)) {
    paste0(the_year, "-09-20")
  } else {
    paste0(the_year, "-03-20")
  }
}




