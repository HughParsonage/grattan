#include <Rcpp.h>
using namespace Rcpp;

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

