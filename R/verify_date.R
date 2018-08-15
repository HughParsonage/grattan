
verify_date <- function(date_to_verify, from , to) {
  Date <- date_to_verify
  if (missing(from)){
    from <- -Inf
  }
  if (missing(to)){
    to <- Inf
  }
  # check if able to make object a Date
  if (any(is.na(as.Date(Date, optional = TRUE)))) {
    i_bad_date <- which(is.na(as.Date(Date, optional = TRUE)))
    first_bad_date_i <- i_bad_date[1]
    first_bad_date <- Date[first_bad_date_i]
    stop("`Date` had value ", first_bad_date, " at position ", first_bad_date_i, ". ")
  }
  
  # check if in correct timeframe
  if (!all(between(year(Date), from, to))) {
    i_bad_date <- which(!between(year(Date), from, to))
    first_bad_date_i <- i_bad_date[1]
    first_bad_date <- Date[first_bad_date_i]
    stop("`Date` had value ", first_bad_date, " at position ", first_bad_date_i, ". ",
         "Ensure `Date` only includes dates between ", from, " and ", to)
  }
  
}