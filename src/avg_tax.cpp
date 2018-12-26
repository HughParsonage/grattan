#include <Rcpp.h>
#include "grattan.h"
using namespace Rcpp;

//' @title Simple average tax in C++
//' @name simple_avg
//' @param y Initial income to use.
//' @param a The average tax rate
//' @return The income corresponding to 'a'.
//' @export simple_avg
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double simple_avg(double y, double a) {
  NumericVector tt(5);
  tt[0] = 0;
  tt[1] = 18200;
  tt[2] = 37000;
  tt[3] = 80000;
  tt[4] = 180000;
  
  NumericVector rr(5);
  rr[0] = 0;
  rr[1] = 0.19;
  rr[2] = 0.325;
  rr[3] = 0.37;
  rr[4] = 0.45;
  while (y < 0.001) {
    y += 1;
  }
  
  double o = 0;
  o = IncomeTax(y, tt, rr)[0] / y;
  int i = 0;
  while (o < a && i < 10000) {
    y += 1;
    i += 1;
    o = IncomeTax(y, tt, rr)[0] / y;
  }
  return y;
}
