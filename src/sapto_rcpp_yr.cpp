#include <Rcpp.h>
#include "grattan.h"
using namespace Rcpp;

// [[Rcpp::export]]
double sapto_rcpp_yr_singleton (double rebateIncome, bool isMarried, double spouseIncome, int yr) {
  double out = 0;
  if (yr >= 2013) {
    if (isMarried) {
      out = sapto_rcpp_singleton(rebateIncome, 3204, 57948, 0.125, true, spouseIncome, true);
    } else {
      out = sapto_rcpp_singleton(rebateIncome, 2230, 32279, 0.125, true, 0, false);
    }
  }
  return out;
}

//' @title SAPTO for specific years in C++
//' @name sapto_rcpp_yr
//' @description Fast way to calculate SAPTO for multiple people when the year is known in advance. Speed is by cheating and entering in the year's parameters literally.
//' @param RebateIncome,IsMarried,SpouseIncome As in \code{\link{sapto}}.
//' @export sapto_rcpp_yr

// [[Rcpp::export]]
NumericVector sapto_rcpp_yr(NumericVector RebateIncome, NumericVector SpouseIncome, LogicalVector IsMarried, int yr) {
  int n = RebateIncome.length();
  NumericVector out(n);
  
  double rii;
  bool imi;
  double sii;
  for (int i = 0; i < n; ++i) {
    rii = RebateIncome[i];
    imi = IsMarried[i];
    sii = SpouseIncome[i];
    out[i] = sapto_rcpp_yr_singleton(rii, imi, sii, yr);
  }
  return out;
}

