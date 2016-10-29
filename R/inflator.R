#' Inflate using a general index
#' 
#' @param x The vector to be inflated.
#' @param from The contemporaneous time of x. 
#' @param to The target time to which x is to be inflated.
#' @param inflator_table A \code{data.table} having columns \code{Index} and \code{Time}. 
#' @param index.col The column in \code{inflator_table} containing the index used for inflation.
#' @param time.col The column in \code{inflator_table} by which times are mapped. 
#' @param roll If \code{NULL}, inflation is calculated only on exact matches in \code{inflator_table}. Otherwise, uses a rolling join. See \code{data.table::data.table}.
#' @return A vector of inflated values.
#' @export

inflator <- function(x = 1, from, to, inflator_table, index.col = "Index", time.col = "Time", roll = NULL){
  prohibit_length0_vectors(x, from, to)
  prohibit_vector_recycling(x, from, to)
  stopifnot(is.numeric(Index))
  
  inflator_table <- 
    inflator_table %>%
    as.data.table %>%
    setnames(c(index.col, time.col), c("Index", "Time")) %>%
    setkeyv("Time")
  
  # Issue #24
  any_from_after_to <- any(to < from)
  
  if (any_from_after_to){
    # if from is after to, we want -1 (so it can be raised by that power)
    out_power <- sign(to - from)  # vectorized
    
    from <- pmin(from, to)
    to   <- pmax(to, from)
  }
  
  input <- 
    data.table(x = x, from = from, to = to)
  
  if (is.null(roll)){
  output <- 
    input %>%
    merge(inflator_table, by.x = "from", by.y = "Time", sort = FALSE,
          all.x = TRUE) %>%
    setnames("Index", "from_index") %>%
    merge(inflator_table, by.x = "to", by.y = "Time", sort = FALSE, 
          all.x = TRUE) %>%
    setnames("Index", "to_index") %>%
    inflator_frac("x", "to_index", "from_index", "out")
  } else {
    output <- 
      input %>%
      .[, order := 1:.N] %>%
      setnames("from", "Time") %>%
      setkeyv("Time") %>%
      inflator_table[., roll = roll] %>%
      setnames("Index", "from_index") %>%
      setnames("to", "Time") %>%
      inflator_table[., roll = roll] %>%
      setnames("Index", "to_index") %>%
      inflator_frac("x", "to_index", "from_index", "out")
  }
  
  if (any_from_after_to){
    output[["out"]] ^ out_power
  } else {
    output[["out"]]
  }
}