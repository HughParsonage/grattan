

next_fy <- function(fy, h = 1L) {
  yr2fy(as.integer(substr(fy, 0L, 4L)) + 1L + h)
}

prev_fy <- function(fy, h = 1L) {
  next_fy(fy, h = -h)
}


