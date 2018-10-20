#' Rent assistance
#' @description The rent assistance to each individual payable by financial year.
#' @param fortnightly_rent The fortnightly rent paid by each individual. By 
#' default, infinity, so the maximum rent assistance is returned by default, 
#' since rent assistance is capped at a maximum rate. Note the criteria for board
#'  and lodging which can be found at \url{http://guides.dss.gov.au/guide-social-security-law/3/8/1/70}
#' @param per Specifies the timeframe in which payments will be made. Can either
#'  take value "fortnight" or "annual".
#' @param fy.year (character) The financial year over which rent assistance is 
#' to be calculated. When left as \code{NULL}, defaults to the user's financial 
#' year, unless \code{max_rate} and \code{min_rent} are both set. If 
#' \code{fy.year} is set, the annual payment is provided.
#' @param Date (Date vector or coercible to such) An alternative to \code{fy.year}.
#'  If both \code{fy.year} and \code{Date} are provided, \code{fy.year} is ignored, with a warning. 
#'  If \code{Date} is used, the fortnightly rent assistance is provided.
#' @param n_dependants (integer) Number of dependent children. By default, \code{0L}, so no children.
#' @param has_partner (logical) Is each individual married? By default, \code{FALSE}.

#' @param .prop_rent_paid_by_RA The proportion of the rent above the minimum threshold paid by rent assistance. 
#' Since it so happens that this value is constant over the period, it is set here rather than being added to 
#' the internal table.
#' @param max_rate If not \code{NULL}, a numeric vector indicating for each individual the maximum rent assistance payable.
#' @param min_rent If not \code{NULL}, a numeric vector indicating for each individual the minimum fortnightly rent above which rent assistance is payable. \code{max_rate} and \code{min_rent} must not be used when \code{fy.year} is set.
#' 
#' @param sharers_provision_applies (logical, default: FALSE) Does the sharers provision apply to the parent payment? The list of functions can be found in table 2 column 4 \url{http://guides.dss.gov.au/guide-social-security-law/3/8/1/10}
#' @param is_homeowner (logical, default: \code{FALSE}) Does the individual own 
#' their own home?
#' @param lives_in_sharehouse (logical, default: \code{FALSE}) Does the individual
#'  live in a sharehouse?
#' 
#' @return If \code{fy.year} is used, the annual rent assistance payable for each individual; 
#' if \code{Date} is used, the \emph{fortnightly} rent assistance payable.
#' If the arguments cannot be recycled safely, the function errors. 
#' 
#' @examples 
#' # current annual rent assistance
#' rent_assistance()  
#' 
#' # current fortnightly payment
#' rent_assistance(Date = Sys.Date())  
#' 
#' # zero since no rent
#' rent_assistance(0, Date = "2016-01-02") 
#' 
#' # Rent assistance is payable at 75c for every dollar over min rent
#' rent_assistance(101, max_rate = 500, min_rent = 100)
#' rent_assistance(500, max_rate = 500, min_rent = 100)
#' 
#' @export

rent_assistance <- function(fortnightly_rent = Inf,
                            per = "fortnight",
                            fy.year = NULL,
                            Date = NULL,
                            n_dependants = 0L,
                            has_partner = FALSE,
                            .prop_rent_paid_by_RA = 0.75,
                            max_rate = NULL,
                            min_rent = NULL,
                            sharers_provision_applies = FALSE,
                            is_homeowner = FALSE,
                            lives_in_sharehouse = FALSE) {
  
  if (is.null(max_rate) && is.null(min_rent)) {
    if (is.null(Date)) {
      if (is.null(fy.year)) {
        fy.year <- date2fy(Sys.Date())
        message('`fy.year` not set, so defaulting to fy.year = "', fy.year, '"')
      }
      
      permitted_fys <- 
        c("1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", 
          "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", 
          "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", 
          "2017-18", "2018-19", "2019-20", "2020-21")
      
      fy.year <- validate_fys_permitted(fy.year, permitted_fys)
      
      prohibit_vector_recycling(fy.year, n_dependants, has_partner, fortnightly_rent)
    } else {
      if (!is.null(fy.year)) {
        warning("Both `Date` and `fy.year` were set. ",
                "`fy.year` will be ignored.")
        fy.year <- NULL
      }
      
      if (!inherits(Date, "Date")) {
        Date <-
          tryCatch(as.Date(Date),
                   # To avoid arcane error messages when the method is dispatched
                   error = function(e) {
                     stop("`Date` was supplied to `rent_assistance()`, ",
                          "but is neither a Date object ",
                          "nor safely coercible as such.\n\n", 
                          "When attempting `as.Date(Date)`, encountered the error:\n\t", e$m, 
                          call. = FALSE)
                   })
      }
      
      if (!all(between(year(Date), 2000L, 2021L))) {
        i_bad_date <- which(!between(year(Date), 2000L, 2021L))
        first_bad_date_i <- i_bad_date[1]
        first_bad_date <- Date[first_bad_date_i]
        stop("`Date` had value ", first_bad_date, " at position ", first_bad_date_i, ". ",
             "Ensure `Date` only includes dates between 2000 and 2021.")
      }
      prohibit_vector_recycling(Date, n_dependants, has_partner, fortnightly_rent)
    }
    
    if (!is.integer(n_dependants)) {
      if (is.factor(n_dependants)) {
        stop("`n_dependants` was a factor. Change to be a pure integer.")
      }
      if (!is.double(n_dependants)) {
        stop("`n_dependants` was type ", typeof(n_dependants), ". ",
             "Change `n_dependants` to be an integer vector.")
      }
      if (any(n_dependants != as.integer(n_dependants))) {
        stop("`n_dependants` is type double and cannot be safely coerced to type integer.")
      }
      n_dependants <- as.integer(n_dependants)
    }
    
    n_dependants[n_dependants == 2L] <- 1L
    n_dependants[n_dependants > 3L] <- 3L
    
    
    # Actual calculation:
    
    input <- 
      data.table(fy_year = fy.year %||% Date,
                 hasPartner = has_partner,
                 nDependants = n_dependants,
                 Rent = fortnightly_rent)
    
    input[, "ordering" := .I]
    
    if (is.null(fy.year)) {
      setnames(input, "fy_year", "Date")
      setkeyv(input,
              c("hasPartner",
                "nDependants",
                "Date"))
      
      output <- 
        rent_assistance_rates_by_date[input,
                                      on = c("hasPartner",
                                             "nDependants",
                                             "Date"), 
                                      roll = -Inf]
    } else {
      setkeyv(input,
              c("fy_year",
                "hasPartner",
                "nDependants"))
      
      output <- 
        rent_assistance_rates[input,
                              on = c("fy_year",
                                     "hasPartner",
                                     "nDependants")]
    }
    
    Rent <- NULL
    
    ra <- 
      output %>%
      .[, "out" := pminV(.prop_rent_paid_by_RA * pmaxC(Rent - min_rent, 0),
                         max_rate)] %>%
      setorderv("ordering") %>%
      .subset2("out")
    
  } else {
    
    if (is.null(max_rate)) {
      stop("`min_rent` was set, but `max_rate` is NULL. ",
           "Either set both `max_rate` and `min_rent`, or set `fy.year` only.")
    }
    
    if (is.null(min_rent)) {
      stop("`max_rate` was set, but `min_rent` is NULL. ",
           "Either set both `max_rate` and `min_rent`, or set `fy.year` only.")
    }
    
    if (!is.null(fy.year)) {
      warning("`fy.year` must not be supplied if `max_rate` and `min_rent` are. ",
              "`fy.year` will be ignored.")
    }
    
    if (!missing(n_dependants) || !missing(has_partner)) {
      message("`n_dependants` and `has_partner` were supplied but are being ignored ", 
              "since `max_rate` and `min_rent` were set.")
    }
    
    if (!is.numeric(max_rate)) {
      stop("`max_rate` was set but was not numeric.")
    }
    if (!is.numeric(min_rent)) {
      stop("`min_rent` was set but was not numeric.")
    }
    
    max.length <- 
      prohibit_vector_recycling.MAXLENGTH(.prop_rent_paid_by_RA, 
                                          fortnightly_rent,
                                          min_rent,
                                          max_rate)
    
    input <- lapply(list(.prop_rent_paid_by_RA = .prop_rent_paid_by_RA, 
                         Rent = fortnightly_rent,
                         min_rent = min_rent,
                         max_rate = max_rate), 
                    rep_len, max.length)
                      
    setDT(input)
    
    ra <- input[, pminV(.prop_rent_paid_by_RA * pmax0(Rent - min_rent),
                        max_rate)]
  }
  
  # sharers provision
  
  prohibit_vector_recycling(sharers_provision_applies,
                            is_homeowner,
                            lives_in_sharehouse,
                            has_partner,
                            n_dependants)
  
  SharersProvisionApplies <- NULL
  isHomeOwner <- NULL
  ShareHouse <- NULL
  hasPartner <- NULL
  nDependants <- NULL
  sharers_prov <- data.table(SharersProvisionApplies = sharers_provision_applies,
                             isHomeOwner = is_homeowner,
                             ShareHouse = lives_in_sharehouse,
                             hasPartner = has_partner,
                             nDependants = n_dependants)
  
  ra <- ra * (1 - 1/3 * sharers_prov[, SharersProvisionApplies &  
                                       !isHomeOwner &
                                       !hasPartner &
                                       nDependants == 0 &
                                       ShareHouse])
  
  # validate_per assumes yearly payments, however RA has fortnightly rates which is why it must be scaled by 26
  
  ra * 26 / validate_per(per, missing(per))
}
