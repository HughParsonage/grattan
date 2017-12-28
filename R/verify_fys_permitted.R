

verify_fys_permitted <- function(to_verify, permitted_fys) {
  fy.year <- to_verify
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
}

