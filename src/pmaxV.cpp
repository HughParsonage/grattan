//' @title Parallel maximum
//' @description A faster \code{pmax()}.
//'
//' @name pmaxV
//' @param x A numeric vector.
//' @param y A numeric vector, the same length as x.
//' @return The parallel maximum of the input values.
//' @export pmaxV


#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pmaxV(NumericVector x, NumericVector y) {
  int n = x.length();
  int m = y.length();
  if (n != m){
    stop("x and y must be same length.");
  }
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    if (xi < yi) {
      out[i] = yi;
    } else {
      out[i] = xi;
    }
  }
  
  return out;
}
