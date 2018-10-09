
check_TF <- function(x) {
  if (is.logical(x) && length(x) == 1L) {
    if (anyNA(x)) {
      xc <- deparse(substitute(x))
      stop("`", xc, " = NA` but must be TRUE or FALSE. ", 
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      return(NULL)
    }
  } else {
    xc <- deparse(substitute(x))
    if (length(x) != 1L) {
      stop("`", xc, "` had length ", length(x), " but must be length-one. ", 
           "Change `", xc, "` to be TRUE or FALSE.")
    } else {
      stop("`", xc, "` was type ", typeof(x), " but must be logical. ", 
           "Change `", xc, "` to be TRUE or FALSE.")
    }
  }
}


check_num1 <- function(x) {
  if (is.numeric(x) && length(x) == 1L) {
    if (anyNA(x)) {
      xc <- deparse(substitute(x))
      stop("`", xc, " = NA` but must be a non-missing numeric. ", 
           "Change `", xc, "` to be a single value.")
    } else {
      return(NULL)
    }
  } else {
    xc <- deparse(substitute(x))
    if (length(x) != 1L) {
      stop("`", xc, "` had length ", length(x), " but must be length-one. ", 
           "Change `", xc, "` to be a single number.")
    } else {
      stop("`", xc, "` was type ", typeof(x), " but must be numeric. ", 
           "Change `", xc, "` to be a single number.")
    }
  }
}
