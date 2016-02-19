#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pminC(NumericVector x, double a) {
  int n = x.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    if (xi > a) {
      out[i] = a;
    } else {
      out[i] = xi;
    }
  }
  
  return out;
}