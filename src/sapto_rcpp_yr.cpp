#include <Rcpp.h>
#include "grattan.h"
using namespace Rcpp;

double sapto_rcpp_yr_singleton (double rebateIncome, bool isMarried, double spouseIncome, int yr) {
  double out = 0;
  switch (yr) {
  case 2014:
    if (isMarried) {
      out = sapto_rcpp_singleton(rebateIncome, 3204, 57948, 0.125, true, spouseIncome, "married");
    } else {
      out = sapto_rcpp_singleton(rebateIncome, 2230, 32279, 0.125, true, spouseIncome, "single");
    }
  default: out = 0;
  }
  return out;
}

//' @title SAPTO for specific years in C++
//' @name sapto_rcpp_yr
//' @description Fast way to calculate SAPTO for multiple people when the year is known in advance. Speed is by cheating and entering in the year's parameters literally.
//' @param RebateIncome,IsMarried,SpouseIncome As in \code{\link{sapto}}.
//' @export sapto_rcpp_yr

// [[Rcpp::export]]
NumericVector sapto_rcpp_yr(NumericVector RebateIncome, LogicalVector IsMarried, NumericVector SpouseIncome, int yr) {
  int n = RebateIncome.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = sapto_rcpp_yr_singleton(RebateIncome[i], IsMarried[i], SpouseIncome[i], yr);
  }
  return out;
}

