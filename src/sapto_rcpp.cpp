#include <Rcpp.h>
#include "grattan.h"
using namespace Rcpp;


//' @title SAPTO in C++
//' @description An implementation of SAPTO in C++.
//' @name sapto_rcpp
//' @param RebateIncome,MaxOffset,LowerThreshold,TaperRate,SaptoEligible,SpouseIncome,IsMarried As in \code{\link{sapto}}.
//' @export
// [[Rcpp::export]]
NumericVector sapto_rcpp(NumericVector RebateIncome,
                         NumericVector MaxOffset,
                         NumericVector LowerThreshold,
                         NumericVector TaperRate,
                         LogicalVector SaptoEligible,
                         NumericVector SpouseIncome,
                         LogicalVector IsMarried) {
  
  
  
  int n = RebateIncome.length();
  NumericVector out(n);
  
  double rik = 0;
  double mok = 0;
  double ltk = 0;
  double tpk = 0;
  bool sek = false;
  double sik = 0;
  bool imk = false;
  
  for (int k = 0; k < n; ++k) {
    rik = RebateIncome[k];
    mok = MaxOffset[k];
    ltk = LowerThreshold[k];
    tpk = TaperRate[k];
    sek = SaptoEligible[k];
    sik = SpouseIncome[k];
    imk = IsMarried[k];
    if (sek) {
      out[k] = sapto_rcpp_singleton(rik,mok,ltk,tpk,sek,sik,imk);
    }
  }
  return out;
}


