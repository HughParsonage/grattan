//' @title Parallel maximum
//' @description A faster \code{pmin()}.
//'
//' @name pminV
//' @param x A numeric vector.
//' @param y A numeric vector, the same length as x.
//' @return The parallel maximum of the input values.
//' @export pminV

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pminV(NumericVector x, NumericVector y) {
  int n = x.length();
  int m = y.length();
  if (n != m){
    stop("x and y must be same length.");
  }
  NumericVector out(n);
  
  
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    if (yi < xi) {
      out[i] = yi;
    } else {
      out[i] = xi;
    }
  }
  
  return out;
}
