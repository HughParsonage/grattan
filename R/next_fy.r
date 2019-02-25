

next_fy <- function(fy = NULL, h = 1L) {
  if (is.null(fy)) {
    return(yr2fy(year(Sys.Date()) + h))
  }
  yr2fy(as.integer(substr(fy, 0L, 4L)) + 1L + h)
}

prev_fy <- function(fy = NULL, h = 1L) {
  next_fy(fy, h = -h)
}


