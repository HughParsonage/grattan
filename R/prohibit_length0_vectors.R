#' Prohibit zero lengths
#' 
#' Tests whether any vectors have zero length.
#' 
#' @param ... A list of vectors
#' @return An error message if any of the vectors \code{...} have zero length.

prohibit_length0_vectors <- function(...){
  lengths <- vapply(list(...), FUN = length, FUN.VALUE = 0L)
  if (any(lengths == 0L)){
    stop("You have passed a zero length vector to this function, which is not allowed.")
  }
}
