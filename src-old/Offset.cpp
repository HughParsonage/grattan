#include "grattan.h"

// y ----
//       \                        .
//        \       m = y / (b - a) .
//         \_____                 .
// 0     a  b       

//' @title General offset in C++
//' @name Offset
//' @description Calculate the offset given a threshold, a maximum offset, and a taper. 
//' @param x A vector of incomes etc.
//' @param y The maximum offset available; the offset when \code{x} is zero.
//' @param a The maximum value of \code{x} at which the maximum offset is available.
//' @param m The taper rate (the \strong{negative} slope).
//' @export Offset


// [[Rcpp::export]]
NumericVector Offset(NumericVector x,
                     double y,
                     double a,
                     double m) {
  int n = x.length();
  NumericVector out(n);
  // m is negative slope
  // b is x-intercept
  double b = y / m + a;
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi > b) {
      out[i] = 0;
    } else {
      if (xi < a) {
        out[i] = y;
      } else {
        out[i] = m * (b - xi);
      }
    }
  }
  return out;
}

//' @name MultiOffset
//' @title Multioffset
//' @description An offset with multiple thresholds and tapers.
//' 
//' @param x A numeric vector that the thresholds refer to.
//' @param first_offset \code{numeric(1)} The value of the first offset,
//' the value of the offset for \code{x < first(thresholds)}.
//' @param thresholds The thresholds for changes in the offset.
//' @param tapers The tapers that apply \strong{above} the corresponding element
//' of \code{thresholds}.
//' @param above_zero (bool) Should negative offsets be set to zero?
//' 
//' 
//' @examples
//' MultiOffset(c(36e3, 37e3, 38e3, 47e3, 48e3, 49e3), 
//'             255,
//'             c(37e3, 48e3, 90e3),
//'             c(0.075, 0, -0.03))
//' @noRd

// [[Rcpp::export(rng = false)]]
DoubleVector MultiOffset(NumericVector x, 
                         double first_offset, 
                         DoubleVector thresholds,
                         DoubleVector tapers,
                         bool above_zero = true) {
  R_xlen_t N = x.length();
  R_xlen_t tn = thresholds.length();
  
  for (R_xlen_t t = 1; t < tn; ++t) {
    if (thresholds[t - 1] >= thresholds[t]) {
      Rcerr << "(MultiOffset) t = " << t << " ";
      stop("`thresholds` was not sorted.");
    }
  }
  
  NumericVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = first_offset;
    double xi = x[i];
    if (xi < thresholds[0]) {
      continue;
    }
    for (R_xlen_t t = 0; t < tn; ++t) {
      if (xi < thresholds[t]) {
        break;
      }
      double taper = tapers[t];
      double above_threshold = xi - thresholds[t];
      if ((t + 1) < tn) {
        double next_threshold = thresholds[t + 1];
        if (xi > next_threshold) {
          above_threshold = next_threshold - thresholds[t];
        }
      } 
      out[i] += above_threshold * taper;
    }
    if (above_zero && out[i] < 0) {
      out[i] = 0;
    }
  }
  return out;
}




