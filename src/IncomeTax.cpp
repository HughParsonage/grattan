#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

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


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
