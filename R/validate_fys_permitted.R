#' Verifying validity of financial years
#' 
#' @description Many functions expect financial years. 
#' Determining that they are validly entered is often quite 
#' computationally costly, relative to the core calculations.
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
#' @param allow.projection If \code{FALSE} emit a different error message.
#' @param earliest_permitted_financial_year,latest_permitted_financial_year Text
#'  for earliest/latest permitted financial year when \code{min.yr}/\code{max.yr}
#'  condition is violated.
#' 
#' @return If \code{to_verify} contains valid financial years
#' they are returned all in the form \code{2013-14}. If they were
#' already in that form, they obtain the following attributes:
#' \describe{
#' \item{\code{grattan_all_fy}}{\code{TRUE} if all the financial years are valid.}
#' \item{\code{grattan_min_yr}}{An integer, the earliest year ending in \code{to_verify}.}
#' \item{\code{grattan_max_yr}}{An integer, the latest year ending in \code{to_verify}.}
#' }
#' 
#' 
#' 



validate_fys_permitted <- function(to_verify, permitted_fys,
                                   min.yr = NULL, max.yr = NULL,
                                   deparsed = deparse(substitute(to_verify)),
                                   allow.projection = TRUE,
                                   earliest_permitted_financial_year = "earliest permitted financial year",
                                   latest_permitted_financial_year = "latest permitted financial year") {
  
  if (!is.character(to_verify)) {
    stopn("`", deparsed, "` was type ", typeof(to_verify), ", ",
          "but must be type character. ", 
          "Ensure `", deparsed, "` is a character vector of financial years", 
          if (!is.null(min.yr) || !is.null(max.yr)) " satisfying ",
          if (!is.null(min.yr)) paste0("`", yr2fy(min.yr), " <= "),
          if (!is.null(min.yr) || !is.null(max.yr)) deparsed,
          if (!is.null(max.yr)) paste0(" <= ", yr2fy(max.yr), "`"),
          ".")
  }
  
  if (isTRUE(attr(to_verify, "grattan_all_fy"))) {
    # If min.yr and max.yr are fine, we're done
    if (is.null(min.yr) && is.null(max.yr)) {
      return(to_verify)
    }
    
    # Otherwise we just have to check the ranges: either the ranges
    # are no good (in which case error), or return to_verify
    
    # min
    if (!is.null(min.yr)) {
      
      # Unlikely (misspecified), but should assert
      if (is.null(attr(to_verify, "grattan_min_yr"))) {
        min_to_verify_yr <- min_fy2yr(to_verify)
        attr(to_verify, "grattan_min_yr") <- min_to_verify_yr
      }
      
      if (min.yr > attr(to_verify, "grattan_min_yr")) {
        min.k <- min.yr - 1900L
        stopn("`", deparsed, 
              if (length(to_verify) == 1L) " = " else "` contained ",
              '"', fys1901[attr(to_verify, "grattan_min_yr") - 1900L], '"',
              if (length(to_verify) == 1L) "`",
              " which ",
              "is earlier than the ",
              earliest_permitted_financial_year,
              ": ", '"', fys1901[min.k], '"', ".")
      }
    }
    
    # max
    if (!is.null(max.yr)) {
      
      # Unlikely (misspecified), but should assert
      if (is.null(attr(to_verify, "grattan_max_yr"))) {
        max_to_verify_yr <- max_fy2yr(to_verify)
        attr(to_verify, "grattan_max_yr") <- max_to_verify_yr
      }
      
      if (max.yr < attr(to_verify, "grattan_max_yr")) {
        max.k <- max.yr - 1900L
        stopn(if (!allow.projection) "`allow.projection = FALSE`, yet ",
              "`", deparsed,
              if (length(to_verify) == 1L) " = " else "` contained ",
              '"', fys1901[attr(to_verify, "grattan_max_yr") - 1900L], '"',
              if (length(to_verify) == 1L) "`",
              " which ",
              "is later than the ",
              latest_permitted_financial_year,
              ": ", '"', fys1901[max.k], '"', ".")
      }
    }
    
    return(to_verify)
  }
  
  
  fy.year <- to_verify
  if (missing(permitted_fys)) {
    if (anyNA(fmatches <- fmatch(to_verify, fys1901))) {
      if (all(are_fy <- is.fy(to_verify))) {
        nchar_to_verify <- nchar(to_verify)
        out <- sprintf("%s-%s",
                       substr(to_verify, 1L, 4L),
                       substr(to_verify, nchar_to_verify - 1L, nchar_to_verify))
        return(out)
      }
      first_bad <- which.min(are_fy)
      stopn("`", deparsed, 
            if (length(to_verify) == 1L) " = " else "` contained ",
            '"', to_verify[first_bad], '"',
            if (length(to_verify) == 1L) "` was " else " which is ",
            "not a valid financial year.")
    } else {
      attr(to_verify, "grattan_all_fy") <- TRUE
      if (!is.null(min.yr)) {
        min.k <- min.yr - 1900L
        min_fmatches <- min(fmatches)
        if (min_fmatches < min.k) {
          first_bad <- which.min(fmatches)
          stopn("`", deparsed,
                if (length(to_verify) == 1L) " = " else "` contained ",
                '"', to_verify[first_bad], '"',
                if (length(to_verify) == 1L) "`",
                " which ",
                "is earlier than the ",
                earliest_permitted_financial_year,
                ": ", '"', fys1901[min.k], '"', ".")
        }
        attr(to_verify, "grattan_min_yr") <- min_fmatches + 1900L
      }
      if (!is.null(max.yr)) {
        max.k <- max.yr - 1900L
        max_fmatches <- max(fmatches)
        if (max_fmatches > max.k) {
          first_bad <- which.max(fmatches)
          stopn(if (!allow.projection) "`allow.projection = FALSE`, yet ",
                "`", deparsed,
                if (length(to_verify) == 1L) " = " else "` contained ",
                '"', to_verify[first_bad], '"',
                if (length(to_verify) == 1L) "`",
                " which ",
                "is later than the ",
                latest_permitted_financial_year,
                ": ", '"', fys1901[max.k], '"', ".")
        }
        attr(to_verify, "grattan_max_yr") <-  max_fmatches + 1900L
      }
      return(invisible(to_verify))
    }
  }
  
  
  if (!all(fy.year %chin% permitted_fys)) {
    if (any(!is.fy(fy.year))) {
      i <- which(!is.fy(fy.year))
      i1 <- i[1]
      if (length(i) > 1) {
        stopn("`", deparsed, "` contained invalid FYs. ",
              "There were ",
              length(i), " invalid entries (", 
              round(100 * length(i) / length(fy.year)), "%).",
              "\n\n",
              "First invalid FY:\n\t", fy.year[i1], "\n",
              "at position ", i)
      } else {
        if (length(fy.year) == 1L) {
          stopn("`", deparsed, "` set to '", fy.year, "', was not a valid financial year. ",
                "Select a valid fy.year between ",
                permitted_fys[1], " and ", last(permitted_fys), ".")
        } else {
          stopn("`", deparsed, "` contained invalid entry ",
                fy.year[i1], " at position ", i1, ".")
        }
      }
    }
    i <- which(fy.year %notin% permitted_fys)
    i1 <- i[1]
    
    if (length(i) == 1L) {
      stopn("`", deparsed, " = ", fy.year[i1], "` was not within the allowed range: ",
            permitted_fys[1], " <= fy.year <= ", last(permitted_fys))
    } else {
      stopn("`", deparsed, "` were not within the allowed range: ",
            permitted_fys[1], " <= fy.year <= ", last(permitted_fys), "\n\n",
            "First invalid FY:\n\t", fy.year[i1], "\n",
            "at position ", i1)
    }
  }
  return(to_verify)
}





