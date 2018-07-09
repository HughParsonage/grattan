#' Disability support pension
#' @description Identical to the \code{\link{age_pension}} except for those under 21.
#' 
#' @param fortnightly_income,annual_income Income for the means test
#' @param assets Assets test.
#' @param fy.year,Date Either the financial year and Date in which the pension is paid. Only `fy.year = "2015-16"` is implemented.
#' @param age Age of the individual, only relevant for those under 21.
#' @param has_partner (logical, default: \code{FALSE}) Is the individual a member of a couple?
#' @param n_dependants Integer number of dependent children.
#' @param lives_at_home (logical, default: \code{FALSE}) Does the individual live at home with their parents? Only relevant if \code{age < 21}.
#' @param independent (logical, default: \code{FALSE}) Is the person independent? Only relevant if \code{age < 21}.
#' @param per One of \code{"fortnight"}, \code{"year"} return either the fortnightly pension.


disability_pension <- function(fortnightly_income = 0,
                               annual_income = 26 * fortnightly_income,
                               assets = 0,
                               fy.year = NULL,
                               Date = NULL,
                               age = 21L,
                               has_partner = FALSE,
                               n_dependants = 0L,
                               lives_at_home = FALSE,
                               independent = FALSE,
                               per = c("fortnight", "year"),
                               ...) {
  stopifnot(identical(fy.year, "2015-16"))
  if (!missing(annual_income)) {
    if (missing(fortnightly_income)) {
      fortnightly_income <- annual_income / 26
    } else {
      if (!isTRUE(all.equal(annual_income, 26 * fortnightly_income))) {
        stop("Both `annual_income` and `fortnightly_income` ", 
             "were provided, but annual_income is not equal to 26 * fortnigthly_income", 
             "for all entries. ", 
             "Provide one or the other, or provide both but ensure that the ratio is 26.")
      }
    }
  }
  
  out <- 
    if_else(age >= 21L | n_dependants > 0L,
            age_pension(fortnightly_income = fortnightly_income, 
                        assets = assets, 
                        fy.year = "2015-16",
                        has_partner = has_partner,
                        n_dependants = n_dependants,
                        ...),
            # http://guides.dss.gov.au/guide-social-security-law/5/2/2/40
            # possibly more; see
            # https://www.humanservices.gov.au/individuals/services/centrelink/disability-support-pension/payments/payment-rates
            if_else(age >= 18L,
                    if_else(independent | has_partner,
                            433.20,
                            285.10),
                    if_else(lives_at_home, 
                            237.10,
                            285.10)))
  switch(match.arg(per), 
         "fortnight" = out,
         "year" = 26 * out)
  
}
