#' Inflate using a general index
#' 
#' @param x The vector to be inflated.
#' @param from The contemporaneous time of x. 
#' @param to The target time to which x is to be inflated.
#' @param inflator_table A \code{data.table} having columns \code{Index} and \code{Time}. 
#' @return A vector of inflated values.
#' @export

general_inflator <- function(x = 1, from, to, inflator_table){
  prohibit_length0_vectors(x, from, to)
  prohibit_vector_recycling(x, from, to)
  stopifnot(all(c("Index", "Time") %in% names(inflator_table)))
  stopifnot(is.numeric(Index))
  
  input <- 
    data.table(x = x, from = from, to = to)
  
  output <- 
    input %>%
    merge(inflator_table, by.x = "from", by.y = "Time", sort = FALSE,
          all.x = TRUE) %>%
    setnames("Index", "from_index") %>%
    merge(inflator_table, by.x = "to", by.y = "Time", sort = FALSE, 
          all.x = TRUE) %>%
    setnames("Index", "to_index") %>%
    inflator_frac("x", "to_index", "from_index", "out")
  
  output[["out"]]
}