#' Age pension
#' 
#' @param ordinary_income,annual_income Income for means-testing purposes. Provide one but not both.
#' @param has_partner (logical, default: \code{FALSE}) Does the individual have a partner?
#' @param n_dependants How many dependants does sthe individual have? Default is zero.
#' @param partner_fortnightly_income,partner_annual_income The partner's income. The sum of this value and the indiviudal's income gives the income test.
#' @param Date,fy.year The financial year. Currently only 2015-16 is supported (the most recent survey of income and housing results).
#' @param assets_value Total value of household assets.
#' @param is_home_owner (logical, default: \code{FALSE}) Does the individual own their own home? 
#' @param illness_separated_couple Is the couple separated by illness? (Affects the assets test.)
#' 
#' 
#' @export age_pension
#' 


age_pension <- function(ordinary_income = 0, 
                        annual_income = ordinary_income * 26, 
                        has_partner = FALSE,
                        n_dependants = 0L,
                        partner_fortnightly_income = 0,
                        partner_annual_income = partner_fortnightly_income * 26,
                        Date = NULL,
                        fy.year = NULL,
                        assets_value = 0,
                        is_home_owner = FALSE,
                        illness_separated_couple = FALSE) {
  if (is.null(Date)) {
    if (is.null(fy.year)) {
      stop("`Date` and `fy.year` were both NULL, yet one is required.")
    } else {
      if (!identical(fy.year, "2015-16")) {
        .NotYetImplemented()
      }
      Date <- as.Date("2016-07-01")
    }
  }
  
  max.length <- 
    prohibit_vector_recycling.MAXLENGTH(annual_income, 
                                        has_partner, 
                                        n_dependants,
                                        partner_annual_income, 
                                        Date, 
                                        assets_value, 
                                        is_home_owner)
  
  Income <- 
    HasPartner <- 
    PartnerIncome <-
    Assets <- 
    HomeOwner <-
    IlnnessSeparated <- NULL
  
  input <- 
    data.table(Income = annual_income, 
               HasPartner = has_partner, 
               n_dependants = n_dependants,
               PartnerIncome = partner_annual_income, 
               Date = as.Date(Date), 
               Assets = assets_value, 
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
    setkeyv(c("HasPartner", "Date"))
  
  assets_test <- 
    Age_pension_assets_test_by_year %>%
    setkeyv(c("HasPartner", "IllnessSeparated", "HomeOwner", "Date"))
  
  stopifnot(hutils::has_unique_key(assets_test), 
            "assets_excess" %in% names(assets_test))
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
  C <- assets_test[B, on = c("HasPartner", "IllnessSeparated", "HomeOwner", "Date"), roll = Inf, nomatch=0L] 

  # if (is_testing())print(A); print(B); print(C)
  age_pension_income <-
    age_pension_assets <- 
    NULL
  
  C %>%
    .[, age_pension_income := pminV(pmaxC(max_rate - 0.5 * (Income - permissible_income),
                                          0),
                                    max_rate)] %>%
    .[, assets_excess := pmaxC(Assets - assets_test, 0)] %>%
    .[, age_pension_assets := pminV(pmaxC(max_rate - 19.5 * floor(assets_excess / 250), 0), max_rate)] %>%
    setorderv("ordering") %>%
    .[, pminV(age_pension_income, 
              age_pension_assets)]
  
  
}




