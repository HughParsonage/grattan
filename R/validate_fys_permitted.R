#' Verifying validity of financial years
#' 
#' @description Many functions expect financial years. 
#' Determining that they are validly entered is often quite 
#' computatationally costly, relative to the core calculations.
#' These internal functions provide mechanisms to check validity
#' quickly, while still providing clear, accurate error messages.
#' 
#' @param to_verify A user-provided value, purporting to be
#' character vector of financial years.
#' @param permitted_fys A character vector of valid financial years.
#' @param min.yr,max.yr Integers specifying the range of \code{to_verify}.
#' If \code{NULL}, no restriction on the upper or lower bound of the range. 
#' 
#' @param deparsed A string indicating the argument that the user provided.
#' Should generally be provided explicitly as the default is unlikely 
#' to be user-friendly.
#' 
#' @return \code{NULL} invisibly provided \code{to_verify} 
#' contains financial years as specified. Otherwise an error
#' explaining why \code{to_verify} could not be validated.
#' 
#' 



validate_fys_permitted <- function(to_verify, permitted_fys,
                                 min.yr = NULL, max.yr = NULL,
                                 deparsed = deparse(substitute(to_verify))) {
  fy.year <- to_verify
  if (missing(permitted_fys)) {
    if (anyNA(fmatches <- fmatch(to_verify, fys1901))) {
      first_bad <- which.max(is.na(fmatches))
      stop("`", deparsed, "` contains ", '"',
           to_verify[first_bad], '",', " which ",
           "is not a valid FY.")
    } else {
      if (!is.null(min.yr)) {
        min.k <- min.yr - 1900L
        if (min(fmatches) < min.k) {
          first_bad <- which.min(fmatches)
          stop("`", deparsed, "` contains ",
               to_verify[first_bad], " which ",
               "is earlier than the earliest permitted ",
               "financial year: ", '"', fys1901[min.k], ".")
        }
      }
      if (!is.null(max.yr)) {
        max.k <- max.yr - 1900L
        if (max(fmatches) > max.k) {
          first_bad <- which.max(fmatches)
          stop("`", deparsed, "` contains ",
               to_verify[first_bad], " which ",
               "is later than the latest permitted ",
               "financial year: ", '"', fys1901[max.k], ".")
        }
      }
      return(invisible(NULL))
    }
  }
  
  
  if (!all(fy.year %chin% permitted_fys)) {
    if (any(!is.fy(fy.year))) {
      i <- which(!is.fy(fy.year))
      i1 <- i[1]
      if (length(i) > 1) {
        stop("`", deparsed, "` contained invalid FYs. ",
             "There were ",
             length(i), " invalid entries (", 
             round(100 * length(i) / length(fy.year)), "%).",
             "\n\n",
             "First invalid FY:\n\t", fy.year[i1], "\n",
             "at position ", i)
      } else {
        if (length(fy.year) == 1L) {
          stop("`", deparsed, "` set to '", fy.year, "', which is not a valid FY. ",
               "Select a valid fy.year between ",
               permitted_fys[1], " and ", last(permitted_fys), ".")
        } else {
          stop("`", deparsed, "` contained invalid entry ",
               fy.year[i1], " at position ", i1, ".")
        }
      }
    }
    i <- which(fy.year %notin% permitted_fys)
    i1 <- i[1]
    
    if (length(i) == 1L) {
      stop("`", deparsed, " = ", fy.year[i1], "` was not within the allowed range: ",
           permitted_fys[1], " <= fy.year <= ", last(permitted_fys))
    } else {
      stop("`", deparsed, "` were not within the allowed range: ",
           permitted_fys[1], " <= fy.year <= ", last(permitted_fys), "\n\n",
           "First invalid FY:\n\t", fy.year[i1], "\n",
           "at position ", i1)
    }
  }
}





