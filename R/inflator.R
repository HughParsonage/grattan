#' Inflate using a general index
#' 
#' @param x The vector to be inflated.
#' @param from The contemporaneous time of x. 
#' @param to The target time (in units of the \code{inflator_table}) to which x is to be inflated.
#' @param inflator_table A \code{data.table} having columns \code{index.col} and \code{time.col}. 
#' @param index.col The column in \code{inflator_table} containing the index used for inflation.
#' @param time.col The column in \code{inflator_table} by which times are mapped. 
#' @param roll If \code{NULL}, inflation is calculated only on exact matches in \code{inflator_table}. Otherwise, uses a rolling join. See \code{data.table::data.table}.
#' @param max.length (Internal use only). If not \code{NULL}, the maximum length of \code{x},
#' \code{from}, and \code{to} known in advance. May be provided to improve the performance
#' if known.
#' @return A vector of inflated values. For example, \code{inflator_table = grattan:::cpi_seasonal_adjustment}, 
#' \code{index.col = "obsValue"}, \code{time.col = "obsTime"}, gives the CPI inflator.
#' @export

inflator <- function(x = 1,
                     from,
                     to,
                     inflator_table,
                     index.col = "Index",
                     time.col = "Time",
                     roll = NULL,
                     max.length = NULL) {
  if (is.null(max.length)) {
    prohibit_length0_vectors(x, from, to)
    max.length <- prohibit_vector_recycling.MAXLENGTH(x, from, to)
  }
  
  if (max.length == 1L && is.null(roll)) {
    Values <- .subset2(inflator_table, index.col)
    Times <- .subset2(inflator_table, time.col)
    return(x * Values[chmatch(to, Times)] / Values[chmatch(from, Times)])
  }
  
  inflator_table <- 
    inflator_table %>%
    # Possibly locked binding
    as.data.table %>%
    setnames(c(index.col, time.col), c("Index", "Time")) %>%
    setkeyv("Time")
  
  # Issue #24
  any_from_after_to <- any(to < from)
  
  if (any_from_after_to){
    # if from is after to, we want -1 (so it can be raised by that power)
    out_power <- sign((to > from) - 0.5)  # vectorized
    
    .from <- pmin(from, to)
    .to   <- pmax(to, from)
    
    from <- .from
    to <- .to
  }
  
  input <- as.data.table(list(y = 1, from = from, to = to))
  input[, "order" := .I]
  
  if (is.null(roll)){
    output <- 
      input %>%
      merge(inflator_table, by.x = "from", by.y = "Time",
            all.x = TRUE) %>%
      setnames("Index", "from_index") %>%
      merge(inflator_table, by.x = "to", by.y = "Time", 
            all.x = TRUE) %>%
      setnames("Index", "to_index") %>%
      inflator_frac("y", "to_index", "from_index", "out")
  } else {
    output <- 
      input %>%
      setnames("from", "Time") %>%
      setkeyv("Time") %>%
      inflator_table[., roll = roll] %>%
      setnames("Index", "from_index") %>%
      setnames("Time", "from_Time") %>%
      setnames("to", "Time") %>%
      setkeyv("Time") %>%
      inflator_table[., roll = roll] %>%
      setnames("Index", "to_index") %>%
      inflator_frac("y", "to_index", "from_index", "out")
      
  }
  
  setorderv(output, "order")
  
  if (any_from_after_to){
    x * output[["out"]] ^ out_power
  } else {
    x * output[["out"]]
  }
}

