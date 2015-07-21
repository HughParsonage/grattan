#' Financial functions
#' 
#' \url{http://cvs.moodle.org/contrib/patches/question_calculated_extended/calculated/packages/financial/financial_class.php?view=co}
#' @author Enrique Garcia M. \email{egarcia@@egm.as}
#' @author Karsten W. \email{k.weinert@@gmx.net}

npv <- function(rate, values) sum(values / (1 + rate)^seq_along(values))

irr <- function(x, start=0.1) {
  t <- seq_along(x)-1
  f <- function(i) abs(sum(x/(1+i)^t))
  return(nlm(f,start)$estimate)
}

fv <- function(rate, nper, pmt, pv = 0.0, type = 0) {
  pvif <- (1+rate)^nper # Present value interest factor
  fvifa <- if(rate==0) nper else ((1+rate)^nper - 1) / rate
  return(-((pv * pvif) + pmt * (1.0 + rate * type) * fvifa))
}

pv <- function(rate, nper, pmt, fv = 0.0, type = 0) {
  pvif <- (1+rate)^nper # Present value interest factor
  fvifa <- if(rate==0) nper else ((1+rate)^nper - 1) / rate
  return((-fv - pmt * (1.0 + rate * type) * fvifa) / pvif)
}

pmt <- function(rate, nper, pv, fv=0, type=0) {
  rr <- 1/(1+rate)^nper
  res <- (-pv-fv*rr)*rate/(1-rr)
  return(res/(1+rate*type))
}
