
# select columns satisfying a condition

select_which_ <- function(.data, Which, .and.dots){
  Which <- match.fun(Which)
  if (!missing(.and.dots)){
    dplyr::select_(.data, .dots = c(names(.data)[vapply(.data, Which, logical(1))], .and.dots))
  } else {
    dplyr::select_(.data, .dots = names(.data)[vapply(.data, Which, logical(1))])
  }
}

unselect_ <- function(.data, .dots){
  all_names <- names(.data)
  keeps <- names(.data)[!names(.data) %in% .dots]
  dplyr::select_(.data, .dots = keeps)
}

`%notin%` <- function(x, y) {
  !(x %in% y)
}

as.numeric_unless_warning <- function(x){
  tryCatch(as.numeric(x),
           error = function(e) e,
           warning = function(w){x})
}

# from testthat
`%||%` <- function (a, b) {
  if (is.null(a)) b else a
}

# Cosmetic: For line-breaking. Slower but easier to read.  
.add <- function(...) Reduce("+", list(...))

gforecast <- function(x, ...) {
  forecast::forecast(forecast::auto.arima(x), ...)
} 

# NSE
hugh_frac <- function(.data, front, over, under, new_col_name){
  # http://www.r-bloggers.com/using-mutate-from-dplyr-inside-a-function-getting-around-non-standard-evaluation/
  mutate_call <- lazyeval::interp(~r*a/b, a = as.name(over), b = as.name(under), r = as.name(front))
  out <- 
    .data %>%
    dplyr::mutate_(.dots = stats::setNames(list(mutate_call), new_col_name))
  if (is.data.table(.data)){
    setDT(out)
  }
}

# NSE
inflator_frac <- function(.data, front, over, under, new_col_name){
  # http://www.r-bloggers.com/using-mutate-from-dplyr-inside-a-function-getting-around-non-standard-evaluation/
  mutate_call <- lazyeval::interp(~r*a/b, a = as.name(over), b = as.name(under), r = as.name(front))
  .data %>%
    dplyr::mutate_(.dots = stats::setNames(list(mutate_call), new_col_name)) %>%
    as.data.table
}

last_over_first <- function(x){
  if (is.numeric(x)){
    last(x) / first(x)
  } else {
    x
  }
}

MeanNumeric <- function(x){
  sum(as.numeric(x)) / length(x)
}

mean_of_nonzero <- function(x){
  MeanNumeric(x[x > 0])
}

is.nonnegative <- function(vec){
  is.numeric(vec) && !anyNA(vec) && all(vec >= 0)
}

are_zero <- function(x){
  x < .Machine$double.eps ^ 0.5
}

qtrs_ahead <- function(x, y) {
  stopifnot(y > x)
  x_year <- as.integer(substr(x, 0, 4))
  y_year <- as.integer(substr(y, 0, 4))
  x_qtr <- as.integer(substr(x, 7, 7))
  y_qtr <- as.integer(substr(y, 7, 7))
  
  if (x_year == y_year) {
    qtrs_ahead <- y_qtr - x_qtr
  } else {
    qtrs_ahead <- 4 - x_qtr + y_qtr + 4 * (y_year - x_year - 1)
  }
  qtrs_ahead
}


