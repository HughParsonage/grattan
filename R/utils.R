
# select columns satisfying a condition

select_which_ <- function(.data, Which, .and.dots){
  Which <- match.fun(Which)
  if (!missing(.and.dots)){
    dplyr::select_(.data, .dots = c(names(.data)[sapply(.data, Which)] , .and.dots))
  } else {
    dplyr::select_(.data, .dots = names(.data)[sapply(.data, Which)])
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
           warning = function(w){x}, 
           finally = x)
}

# from testthat
`%||%` <- function (a, b) {
  if (is.null(a)) b else a
}

# Cosmetic: For line-breaking. Slower but easier to read.  
.add <- function(...) Reduce("+", list(...))

