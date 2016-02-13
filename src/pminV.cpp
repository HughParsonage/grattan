#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pminV(NumericVector x, NumericVector y) {
  int n = x.length();
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