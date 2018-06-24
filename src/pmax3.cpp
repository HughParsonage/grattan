//' @title Threeway parallel maximum
//' @description Returns the parallel maximum of three 
//' 
//' @name pmax3
//' @param x,y,z Numeric vectors of identical lengths.
//' @return The parallel maximum of the vectors.
//' @export pmax3

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pmax3(NumericVector x, NumericVector y, NumericVector z) {
  int n = x.length();
  int ny = y.length();
  int nz = z.length();
  if ((n != ny) || (n != nz)){
    stop("x, y, z must have the same length.");
  }
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    double yi = y[i];
    double zi = z[i];
    if (xi < zi && yi < zi){
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
