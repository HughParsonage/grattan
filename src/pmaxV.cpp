#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pmaxV(NumericVector x, NumericVector y) {
  int n = x.length();
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