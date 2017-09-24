#include <Rcpp.h>
#include "grattan.h"
using namespace Rcpp;


//' @title SAPTO in C++
//' @name sapto_rcpp
//' @param RebateIncome,MaxOffset,LowerThreshold,ThresholdRate,TaperRate,SpouseIncome,FamilyStatus As in \code{\link{sapto}}.
//' @export sapto_rcpp
// [[Rcpp::export]]
NumericVector sapto_rcpp(NumericVector RebateIncome,
                         NumericVector MaxOffset,
                         NumericVector LowerThreshold,
                         NumericVector ThresholdRate,
                         NumericVector TaperRate,
                         LogicalVector SaptoEligible,
                         NumericVector SpouseIncome,
                         StringVector FamilyStatus) {
  
  
  
  int n = RebateIncome.length();
  NumericVector out(n);
  
  double rik = 0;
  double mok = 0;
  double ltk = 0;
  double thk = 0;
  double tpk = 0;
  bool sek = false;
  double sik = 0;
  std::string fsk = "";
  
  for (int k = 0; k < n; ++k) {
    rik = RebateIncome[k];
    mok = MaxOffset[k];
    ltk = LowerThreshold[k];
    thk = ThresholdRate[k];
    tpk = TaperRate[k];
    sek = SaptoEligible[k];
    sik = SpouseIncome[k];
    fsk = FamilyStatus[k];
    
    out[k] = sapto_rcpp_singleton(rik,mok,ltk,thk,tpk,sek,sik,fsk);
  }
  return out;
}


