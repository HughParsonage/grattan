
#include <Rcpp.h>
using namespace Rcpp;

//' @title IncomeTax
//' @description Calculates the ordinary tax payable given income and tax thresholds and rates.
//' Basic, designed for performance.
//' @name IncomeTax
//' @param x Taxable income.
//' @param thresholds Lower brackets of the tax tables.
//' @param rates Marginal rates
//' @export IncomeTax

// [[Rcpp::export]]
NumericVector IncomeTax(NumericVector x, NumericVector thresholds, NumericVector rates) {
  int n = x.length();
  NumericVector tax(n);
  
  // Environment Env = Environment::parent();
  int tn = thresholds.length();
  
  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    // start from the second (t = 1) 
    // threshold and look back if necessary
    for (int t = 1; t < tn; ++t) {
      double t0 = thresholds[t - 1];
      double t1 = thresholds[t];
      double r0 = rates[t - 1];
      if (xi < t1) {
        tax[i] += r0 * (xi - t0);
        break;
      } else {
        tax[i] += r0 * (t1 - t0);
        if (t == tn - 1) {
          double r1 = rates[t];
          tax[i] += r1 * (xi - t1); 
        }
      }
    }
  }
  
  return tax;
}


