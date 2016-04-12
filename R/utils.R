
# select columns satisfying a condition

select_which_ <- function(.data, Which, .and.dots){
  Which <- match.fun(Which)
  if (!missing(.and.dots)){
    dplyr::select_(.data, .dots = c(names(.data)[sapply(.data, Which)] , .and.dots))
  } else {
    dplyr::select_(.data, .dots = names(.data)[sapply(.data, Which)])
  }
}