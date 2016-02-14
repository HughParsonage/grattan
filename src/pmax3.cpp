#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pmax3(NumericVector x, NumericVector y, NumericVector z) {
  int n = x.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    double zi = z[i];
    if (xi < yi && yi < zi){
      out[i] = zi;
    } else {
      if (xi < yi){
        out[i] = yi;
      } else {
        out[i] = xi;
      }
    }
  }
  return out;
}