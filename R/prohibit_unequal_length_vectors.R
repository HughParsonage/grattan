#' Prohibit unequal length vectors
#' @description Tests whether all vectors have the same length.
#' @param ... Vectors to test.
#' @return An error message unless all of \code{...} have the same length in which case \code{NULL}, invisibly.

prohibit_unequal_length_vectors <- function(...){
  lengths <- vapply(list(...), FUN = length, FUN.VALUE = 0L)
  
  if (max(lengths) != min(lengths)) {
    stop("Input vectors must all have the same length.")
  }
  invisible(NULL)
}

