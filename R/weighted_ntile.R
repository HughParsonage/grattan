#' Weighted quantiles
#' 
#' @param vector The vector for which quantiles are desired.
#' @param weights The weights associated with the vector. None should be \code{NA} or zero.
#' @param n The number of quantiles desired.
#' @return A vector of integers corresponding to the ntiles. (As in \code{dplyr::ntile}.)
#' @examples
#' weighted_ntile(1:10, n = 5)
#' weighted_ntile(1:10, weights = c(rep(4, 5), rep(1, 5)), n = 5)
#' @export
#' @details With a short-length vector, or with weights of a high variance, the results may be unexpected.

weighted_ntile <- function(vector, weights = rep(1, length(vector)), n) {
  if (is.null(weights)) {
    weights <- 1
  }
  min_w <- min(weights)
  if (min_w < 0) {
    stop("`weights` contained negative values. Ensure `weights` is non-negative.")
  }
  
  if (min_w == 0) {
    warning("Some weights are zero. Maximum ntile may be incorrect.")
  }
  
  # We need to sort `vector` first before cumsumming.
  # CRAN NOTE avoidance
  vec <- wts <- orig_order <- NULL
  # 
  .weight <- 
    if (length(weights) == 1L) {
      rep(weights, length(vector))
    } else if (length(weights) == length(vector)) {
      weights
    } else {
      stop("`weights` must be length-one or length(vector).")
    }
  
  out <- 
    setDT(list(vec = vector, 
               wts = .weight,
               orig_order = seq_along(vector))) %>%
    setorderv("vec") %>%
    .[, out := as.integer(floor((n * cumsum(shift(x = wts, n = 1L, fill = 0)) / sum(wts)) + 1))] %>% 
    setorderv("orig_order") %>%
    .[["out"]]
  
  if (max(out) > n){
    warning("Some ntiles greater than n = ", n)
  } 
  out
}

mutate_ntile <- function(DT, col, n, new.col = NULL,
                         overwrite = TRUE,
                         check.na = FALSE) {
  if (length(n) != 1L) {
    stop("`length(n) = ", length(n), "`.", 
         "`n` must be a single whole number.")
  }
  if (!is.integer(n)) {
    if (!is.double(n)) {
      stop("`n` was type ", typeof(n), ".", 
           "`n` must be a single whole number.")
    }
    .n <- as.integer(n)
    if (.n != n) {
      stop("`n` was type double but `n != as.integer(n)`.", 
           "`n` must be a single whole number.")
    }
    n <- .n
  }
  
  .col <- as.character(substitute(col))
  if (is.null(new.col)) {
    suffix <- 
      switch(as.character(n), 
             "3" = "Terciles",
             "4" = "Quartile", 
             "5" = "Quintile",
             "6" = "Sextile",
             "7" = "Septile",
             "8" = "Octile",
             "10" = "Decile", 
             "12" = "DuoDecile",
             "16" = "Hexadecile",
             "20" = "Vigintile",
             "100" = "Percentile", 
             "1000" = "Permilles",
             stop("`n = ", n, "` and new.col is NULL, ",
                  "but no default column suffix is supported.", 
                  "Supply a column name using `new.col`."))
    new.col <- paste0(.col, suffix)
  }
  
  if (not_DT <- !is.data.table(DT)) {
    input_class <- class(DT)
    if (!is.data.frame(DT)) {
      stop("`DT` was a ", class(DT)[1], " but must be a data.frame.")
    }
    DT <- as.data.table(DT)
  }
  
  
  if (OR(haskey(DT) && key(DT)[1] == .col,
         !is.unsorted(DT[[.col]]))) {
    DT[, (new.col) := .ntile(.SD[[1L]], n, check.na = check.na),
       .SDcols = c(.col)]
  } else {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      ntile <- dplyr::ntile
    } else {
      ntile <- weighted_ntile
    }
    
    # n must be named because of weighted_ntile
    DT[, (new.col) := ntile(.SD[[1L]], n = n),
       .SDcols = c(.col)]
    
  }
  if (not_DT) {
    class(DT) <- old_class
  }
  DT[]
}

.ntile <- function(x, n, check.sorted = FALSE, check.na = FALSE) {
  if (check.sorted && is.unsorted(x)) {
    stop("`x` must be already sorted.")
  }
  if (check.na && anyNA(x)) {
    stop("`anyNA(x)` is TRUE.")
  }
  
  as.integer(n * {seq_along(x) - 1L} / length(x) + 1L)
} 




