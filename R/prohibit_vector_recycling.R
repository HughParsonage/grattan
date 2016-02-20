#' Prohibit vector recyling
#' 
#' Tests (harshly) whether the vectors can be recycled safely.
#' 
#' @param ... A list of vectors
#' @return An error message if the vectors are of different length (unless the alternative length is 1).
#' @examples 
#' # Returns nothing because they are of the same length
#' prohibit_vector_recycling(c(2, 2), c(2, 2))
#' # Returns nothing also, because the only different length is 1
#' prohibit_vector_recycling(c(2, 2), 1)
#' # Returns an error:
#' prohibit_vector_recycling(c(2, 2), 1, c(3, 3, 3))
#' @source \url{http://stackoverflow.com/a/9335687/1664978}

prohibit_vector_recycling <- function(...){
  # http://stackoverflow.com/a/9335687/1664978
  lengths <- vapply(list(...), FUN = length, FUN.VALUE = 0L)
  max.length <- pmax.int(lengths)
  if (any(lengths != 1L & lengths != max.length)){
    stop("Inputs must be of equal length, or length 1")
  }
}