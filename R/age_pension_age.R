#' Age of eligibility for the Age Pension
#' @param when Either a Date (or a character vector coercible to such) or a financial year,
#' when the age of eligibility of Age Pension is requested. Defaults to current date.
#' @param sex A character vector the same length as \code{when}, containing strings \code{"male"} and \code{"female
#' "}. May be abbreviated to \code{"m"} or \code{"f"} and is case-insensitive.
#' @return A numeric vector, the age of eligiblity for the Age Pension for each \code{when}.
#' @examples
#' age_pension_age()  # Current age of eligiblity
#' age_pension_age("1995-12-31")
#' age_pension_age("2013-14")
#' @source \url{https://guides.dss.gov.au/social-security-guide/3/4/1/10}
#' @export

age_pension_age <- function(when = Sys.Date(),
                            sex = "male") {
  
  if (!is.character(sex)) {
    stop("`sex` must be a character vector.")
  }
  if (missing(when)) {
    message("`when` not set, so using when = ", when, ".")
    max.length <- length(sex)
  } else {
    lwhen <- length(when)
    lsex <- length(sex)
    if (lsex > 1L) {
      max.length <- lsex
      if (length(when) > 1L &&
          length(sex) != length(when)) {
        stop("`sex` had length ", length(sex),
             ", yet `length(when) = ", length(when), ". ", 
             "`sex` must be length-one or have the same length as `when`.")
      }
    } else {
      max.length <- lwhen
    }
  }
  
  .sex <- tolower(substr(sex, 0L, 1L))
  
  if (!all(.sex %chin% c("m", "f"))) {
    which_first_bad <- first(which(.sex %notin% c("m", "f")))
    stop("`sex` contained ", sex[which_first_bad], " at position ", which_first_bad,
         ". The only valid entries are 'male' or 'female'.")
  }
  
  
  fy_ish <- function(x) {
    is.character(x) &&
      min(nchar(x)) == nchar("2015-16") &&
      max(nchar(x)) == nchar("2015-16")
  }
  
  o <- rep_len(65, max.length)
  
  if (fy_ish(when)) {
    o[when == "2017-18"] <- 65.5
    o[when == "2018-19"] <- 65.5
    o[when == "2019-20"] <- 66
    o[when == "2020-21"] <- 66
    o[when == "2021-22"] <- 66.5
    o[when == "2022-23"] <- 66.5
    o[when >= "2023-24"] <- 67
    Switch(sex,
           DEFAULT = o,
           "f" = {
             o[when <= "1993-94"] <- 60
             o[when == "1994-95"] <- 60
             o[when == "1995-96"] <- 60.5
             o[when == "1996-97"] <- 60.5
             o[when == "1997-98"] <- 61
             o[when == "1998-99"] <- 61
             o[when == "1999-00"] <- 61.5
             o[when == "2000-01"] <- 61.5
             o[when == "2001-02"] <- 62
             o[when == "2002-03"] <- 62
             o[when == "2003-04"] <- 62.5
             o[when == "2004-05"] <- 62.5
             o[when == "2005-06"] <- 63
             o[when == "2006-07"] <- 63
             o[when == "2007-08"] <- 63.5
             o[when == "2008-09"] <- 63.5
             o[when == "2009-10"] <- 64
             o[when == "2010-11"] <- 64
             o[when == "2011-12"] <- 64.5
             o[when == "2012-13"] <- 64.5
           })
  } else {
    if (!inherits(when, "Date")) {
      when <- 
        tryCatch(as.Date(when), 
                 error = function(e) {
                   m <- e$m
                   stop("`when` was not of class 'Date' and ", 
                        "during coercion to Date the following error ", 
                        "was encountered. Ensure `when` is a Date. ", 
                        e$m, 
                        call. = FALSE)
                 })
    }
    
    # http://guides.dss.gov.au/guide-social-security-law/3/4/1/10
    age_pension_dates_female <-
      list(Date = as.Date(c("1995-06-30", paste0(seq(1995, 2013, by = 2), "-07-01"))), 
           Age = seq(60, 65, by = 0.5))
    
    age_pension_dates <- 
      list(Date = as.Date(c("2017-06-30", paste0(seq(2017, 2023, by = 2), "-07-01"))), 
           Age = seq(65, 67, by = 0.5))
    Age_by_Sex_Date <- 
      rbindlist(list(f = age_pension_dates_female, 
                     f = age_pension_dates, 
                     m = age_pension_dates), 
                idcol = "sex") %>%
      setkeyv(c("sex", "Date"))
    
    input <-
      setDT(list(ordering = seq_len(max.length), 
                 sex = rep_len(.sex, max.length),
                 Date = rep_len(when, max.length)))
    setkeyv(input, c("sex", "Date"))
    o <- 
      Age_by_Sex_Date[input,
                      on = c("sex", "Date"),
                      roll = Inf, 
                      rollends = c(TRUE, TRUE)] %>%
      setorderv("ordering") %>%
      .subset2("Age")
  }
  return(o)
}


