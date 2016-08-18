#' Inflate using a general index
#' 
#' @param x The vector to be inflated.
#' @param from The contemporaneous time of x. 
#' @param to The target time to which x is to be inflated.
#' @param inflator_table A \code{data.table} having columns 
#' @return A vector of inflated values.
#' @export

general_inflator <- function(x = 1, from, to, inflator_table){
  prohibit_length0_vectors(x, from, to)
  prohibit_vector_recycling(x, from, to)
  stopifnot(all(c("Index", "Time") %in% names(inflator_table)))
  
  input <- 
    data.table(x = x, from = from, to = to)
  
  # NSE
  inflator_frac <- function(.data, front, over, under, new_col_name){
    # http://www.r-bloggers.com/using-mutate-from-dplyr-inside-a-function-getting-around-non-standard-evaluation/
    mutate_call <- lazyeval::interp(~r*a/b, a = as.name(over), b = as.name(under), r = as.name(front))
    .data %>%
      dplyr::mutate_(.dots = stats::setNames(list(mutate_call), new_col_name))
  }
  
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