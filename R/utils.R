
# select columns satisfying a condition

select_which_ <- hutils::select_which

# fast selector, shallow copy
.selector <- function(dt, noms) {
  dt_key <- key(dt)
  out <- setnames(setDT(lapply(noms, function(v) .subset2(dt, v))), noms)
  if (!is.null(dt_key)) {
    setattr(out, "sorted", dt_key)
  }
  out
}

unselect_ <- function(.data, .dots) {
  hutils::drop_cols(.data, vars = .dots)
}

`%notin%` <- function(x, y) {
  !(x %in% y)
}

any_notin <- function(x, y) {
  anyNA(match(x, y))
}

anyIntersection <- function(x, y) {
  max(match(x, y, nomatch = 0L)) &&
    max(match(y, x, nomatch = 0L))
}

is_knitting <- function() {
  isTRUE(getOption('knitr.in.progress'))
}

# i.e. is the output latex -- useful for constructing
# print objects that may need different escaping/spacing
is_knitting_latex <- function() {
  OR(is_knitting() && 
       identical(knitr::opts_knit$get("out.format"), "latex"),
     # use this for unit tests
     Sys.getenv("_R_GRATTAN_MOCK_KNITR_LATEX_") == "true")
}

if_knitting_latex <- function(yes, no) {
  if (is_knitting_latex()) {
    yes
  } else {
    no
  }
}

# from dplyr::near
near <- function (x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
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
.add <- function(x, ...) {
  if (missing(..1)) {
    return(x)
  }
  x + .add(...)
}

gforecast <- function(x, ...) {
  forecast::forecast(forecast::auto.arima(x), ...)
} 

# NSE
inflator_frac <- function(.data, front, over, under, new_col_name){
  # Will only be invoked where these vars don't exist
  setnames(.data, c(front, over, under), c("r", "a", "b"))
  r <- a <- b <- NULL
  .data[, (new_col_name) := r * a / b]
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

is.nonnegative <- function(vec) {
  is.numeric(vec) && !anyNA(vec) && min(vec) >= 0
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

seq_qtr <- function(from, length.out) {
  out <- character(length.out)
  if (length.out < 1) {
    return(out)
  }
  out[1] <- from
  if (length.out < 2) {
    return(out)
  }
  yr <- as.integer(substr(from, 0, 4))
  qt <- as.integer(substr(from, 7, 7))
  for (i in 2:length.out) {
    if (endsWith(out[i - 1], "Q4")) {
      yr <- yr + 1
    }
    qt <- c(2L, 3L, 4L, 1L)[qt]
    out[i] <- sprintf("%d-Q%d", yr, qt)
  }
  out
}

# vectorized switch 
# e.g. Switch(c("A", "B", "C", "A"), "A" = 1, "B" = 2, "C" = 11:14)
Switch <- function(Expr, ..., DEFAULT) {
  max.length <- max(prohibit_vector_recycling.MAXLENGTH(...), 
                    length(DEFAULT))
  out <- rep_len(DEFAULT, max.length)
  dots <- list(...)
  dot_noms <- names(dots)
  for (n in seq_along(dots)) {
    w <- which(Expr == dot_noms[n])
    n_res <- switch(n, ...)
    if (length(n_res) == 1L) {
      out[w] <- n_res
    } else {
      out[w] <- n_res[w]
    }
  }
  out
}


# NOTE: FUN must return the same length as 'x'. e.g. mean will fail badly.
accel_repetitive_input <- function(x, FUN, ..., THRESHOLD = 1000L) {
  .FUN <- match.fun(FUN)
  if (length(x) <= 1L || length(x) < THRESHOLD) {
    .FUN(x)
  } else {
    DT <- setDT(list(x = x))
    .subset2(DT[, "res" := .FUN(.BY[[1L]], ...), by = "x"], "res")
  }
}

.getOption <- function(x, default = NULL) {
  ans <- getOption(x)
  if (is.null(ans)) {
    default
  } else {
    ans
  }
}

is.Date <- function(x) inherits(x, "Date")

autonamed_list <- function(...) {
  setNames(list(...), nm = eval(substitute(alist(...))))
}

get_qtr <- function(x) {
  stopifnot(is.Date(x))
  {month(x) - 1L} %/% 3L + 1L
}

hasntName <- function(x, name) {
  match(name, names(x), nomatch = 0L) == 0L
}

haveNames <- function(x, noms) {
  all(noms %in% names(x))
}

age2age_range <- function(age) {
  pmax.int(pmin.int(11L - {(age - 15L) %/% 5L}, 11L), 0L)
}

is_testing <- function() {
  requireNamespace("testthat", quietly = TRUE) && testthat::is_testing()
}

