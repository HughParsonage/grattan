#' Rent assistance
#' @description The rent assistance to each individual payable by financial year.
#' @param fortnightly_rent The fortnightly rent paid by each individual. By default, infinity, so the maximum rent assistance is returned by default, since rent assistance is capped at a maximum rate.
#' @param fy.year (character) The financial year over which rent assistance is to be calculated.
#' When left as \code{NULL}, defaults to the user's financial year, unless \code{max_rate} and \code{min_rent} are both set.
#' @param n_dependants (integer) Number of dependent children. By default, \code{0L}, so no children.
#' @param has_partner (logical) Is each individual married? By default, \code{FALSE}.

#' @param .prop_rent_paid_by_RA The proportion of the rent above the minimum threshold paid by rent assistance. 
#' Since it so happens that this value is constant over the period, it is set here rather than being added to 
#' the internal table.
#' @param max_rate If not \code{NULL}, a numeric vector indicating for each individual the maximum rent assistance payable.
#' @param min_rent If not \code{NULL}, the minimum fortnightly rent above which rent assistance is payable. \code{max_rate} and \code{min_rent} must not be used when \code{fy.year} is set.
#' 
#' @return The rent assistance payable for each individual. 
#' If the arguments cannot be recycled safely, the function errors. 
#' @export

rent_assistance <- function(fortnightly_rent = Inf,
                            fy.year = NULL, 
                            n_dependants = 0L,
                            has_partner = FALSE,
                            .prop_rent_paid_by_RA = 0.75,
                            max_rate = NULL,
                            min_rent = NULL) {
  
  if (is.null(max_rate) && is.null(min_rent)) {
    if (is.null(fy.year)) {
      fy.year <- date2fy(Sys.Date())
      message('`fy.year` not set, so defaulting to fy.year = "', fy.year, '"')
    }
    
    permitted_fys <- 
      c("1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", 
        "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", 
        "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", 
        "2017-18", "2018-19", "2019-20", "2020-21")
    
    if (!all(fy.year %chin% permitted_fys)) {
      if (any(!is.fy(fy.year))) {
        i <- which(!is.fy(fy.year))
        i1 <- i[1]
        if (length(i) > 1) {
          stop("`fy.year` contained invalid FYs. ",
               "There were ",
               length(i), " invalid entries (", length(i) / length(fy.year), "%).",
               "\n\n",
               "First invalid FY:\n\t", fy.year[i1], "\n",
               "at position ", i)
        } else {
          if (length(fy.year) == 1L) {
            stop("`fy.year` set to '", fy.year, "', which is not a valid FY. ",
                 "Select a valid fy.year between ",
                 permitted_fys[1], " and ", last(permitted_fys), ".")
          } else {
            stop("`fy.year` contained invalid entry ",
                 fy.year[i1], " at position ", i1, ".")
          }
        }
      }
      i <- which(fy.year %notin% permitted_fys)
      i1 <- i[1]
      
      if (length(fy.year) == 1L) {
        stop("`fy.year = ", fy.year, "` was not within the allowed range: ",
             permitted_fys[1], " <= fy.year <= ", last(permitted_fys))
      } else {
        stop("`fy.year` were not within the allowed range: ",
             permitted_fys[1], " <= fy.year <= ", last(permitted_fys), "\n\n",
             "First invalid FY:\n\t", fy.year[i], "\n",
             "at position ", i)
      }
    }
    
    prohibit_vector_recycling(fy.year, n_dependants, has_partner, fortnightly_rent)
    
    if (!is.integer(n_dependants)) {
      if (!is.double(n_dependants)) {
        stop("`n_dependants` was type ", typeof(n_dependants), ". ",
             "Change `n_dependants` to be an integer vector.")
      }
      if (any(n_dependants != as.integer(n_dependants))) {
        stop("`n_dependants` is type double and cannot be safely coerced to type integer.")
      }
      n_dependants <- as.integer(n_dependants)
    }
    
    
    # Actual calculation:
    
    input <- 
      data.table(fy_year = fy.year,
                 HasPartner = has_partner,
                 nDependants = n_dependants,
                 Rent = fortnightly_rent)
    input[, "ordering" := .I]
    setkeyv(input,
            c("fy_year",
              "HasPartner",
              "nDependants"))
    ra <- 
      rent_assistance_rates[input,
                            on = c("fy_year",
                                   "HasPartner",
                                   "nDependants"), 
                            roll = TRUE] %>%
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
    
    prohibit_vector_recycling(.prop_rent_paid_by_RA, 
                              fortnightly_rent,
                              min_rent,
                              max_rate)
    Rent <- fortnightly_rent
    
    ra <- pminV(.prop_rent_paid_by_RA * pmaxC(Rent - min_rent, 0),
                max_rate)
  }
  
  return(ra)
}
