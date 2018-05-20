#' Revenue foregone from a modelled sample file
#' @param dt A \code{data.table}
#' @param revenue_positive If \code{TRUE}, the default, tax increase (revenue) is positive and tax cuts are negative.
#' @param digits If not \code{NULL}, affects the print method of the value.
#' @export revenue_foregone

revenue_foregone <- function(dt, revenue_positive = TRUE, digits = NULL) {
  stopifnot(is.data.table(dt), 
            "WEIGHT" %in% names(dt),
            "new_tax" %in% names(dt),
            "baseline_tax" %in% names(dt))
  delta <- .subset2(dt, "WEIGHT") * (.subset2(dt, "new_tax") - .subset2(dt, "baseline_tax"))
  out <- sum(delta)
  if (!revenue_positive) {
    out <- -out
  }
  class(out) <- "revenue_foregone"
  setattr(out, "digits", digits)
  out
}

.revenue_foregone <- function(dt, revenue_positive = TRUE, digits = NULL) {
  stopifnot(is.data.table(dt), 
            "WEIGHT" %in% names(dt),
            "new_tax" %in% names(dt),
            "baseline_tax" %in% names(dt))
  delta <- .subset2(dt, "WEIGHT") * (.subset2(dt, "new_tax") - .subset2(dt, "baseline_tax"))
  out <- sum(delta)
  if (!revenue_positive) {
    out <- -out
  }
  class(out) <- "revenue_foregone"
  setattr(out, "digits", digits)
  out
}

#' @export
print.revenue_foregone <- function(x, ...) {
  if (x < 0) {
    pre <- paste0("\u2212", if (is_knitting()) "\\$" else "$")
    x <- -x
  } else {
    pre <- "$"
  }
  d <- function(default) {
    if (is.null(attr(x, "digits"))) {
      default
    } else {
      attr(x, "digits")
    }
  }
  if (x > 10e9) {
    res <- paste0(pre, prettyNum(round(x / 1e9, d(0)), big.mark = ","), " billion")
  } else if (x > 1e9) {
    res <- paste0(pre, prettyNum(round(x / 1e9, d(1)), big.mark = ","), " billion")
  } else {
    res <- paste0(pre, prettyNum(round(x / 1e6, d(0)), big.mark = ","), " million")
  }
  print(res)
}


