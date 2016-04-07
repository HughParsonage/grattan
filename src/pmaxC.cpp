//' @title Parallel maximum
//' @description A faster \code{pmax()}.
//'
//' @name pmaxC
//' @param x A numeric vector.
//' @param a A single numeric value.
//' @return The parallel maximum of the input values.
//' @note This function will always be faster than \code{pmax(x, a)} when \code{a} is a single value, but can be slower than \code{pmax.int(x, a)} when \code{x} is short. Use this function when comparing a numeric vector with a single value.
//' @export pmaxC

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pmaxC(NumericVector x, double a) {
  int n = x.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi < a) {
      out[i] = a;
    } else {
      out[i] = xi;
    }
  }
  
  return out;
}