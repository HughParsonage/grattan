#include <Rcpp.h>
#include "grattan.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector do_sapto_rcpp(NumericVector RebateIncome,
                            NumericVector MaxOffset,
                            NumericVector LowerThreshold,
                            NumericVector TaperRate,
                            LogicalVector SaptoEligible,
                            NumericVector SpouseIncome,
                            LogicalVector IsMarried) {
  int n = RebateIncome.length();
  NumericVector out(n);
  
  // determine whether arguments should be considered constants (if length-1)
  // or vectorized
  const bool se1 = SaptoEligible.length() == 1;
  const bool mo1 = MaxOffset.length() == 1;
  const bool lt1 = LowerThreshold.length() == 1;
  const bool tr1 = TaperRate.length() == 1;
  const bool im1 = IsMarried.length() == 1;
  const bool si1 = SpouseIncome.length() == 1;
  
  double rik = 0;
  double mok = 0;
  double ltk = 0;
  double tpk = 0;
  bool sek = false;
  double sik = 0;
  bool imk = false;
  
  for (int k = 0; k < n; ++k) {
    // sapto eligible early to provide short-circuit
    if (se1) {
      sek = SaptoEligible[0];
    } else {
      sek = SaptoEligible[k];
    }
    if (!sek) {
      continue;
    }
    
    rik = RebateIncome[k];
    
    if (mo1) {
      mok = MaxOffset[0];
    } else {
      mok = MaxOffset[k];
    }
    if (lt1) {
      ltk = LowerThreshold[0];
    } else {
      ltk = LowerThreshold[k];
    }
    if (tr1) {
      tpk = TaperRate[0];
    } else {
      tpk = TaperRate[k];
    }
    if (si1) {
      sik = SpouseIncome[0];
    } else {
      sik = SpouseIncome[k];
    }
    if (im1) {
      imk = IsMarried[0];
    } else {
      imk = IsMarried[k];
    }
    out[k] = sapto_rcpp_singleton(rik,mok,ltk,tpk,sek,sik,imk);
  }
  return out;
}

// Differs from the above by offering varying thresholds depending on marital status
// [[Rcpp::export]]
NumericVector do_sapto_rcpp2(NumericVector RebateIncome,
                             double maxOffsetSingle,
                             double maxOffsetMarried,
                             double lowerThresholdSingle,
                             double lowerThresholdMarried,
                             double taperRateSingle,
                             double taperRateMarried,
                             LogicalVector SaptoEligible,
                             LogicalVector IsMarried,
                             NumericVector SpouseIncome) {
  int n = RebateIncome.length();
  if (n != SaptoEligible.length()) {
    stop("`length(SaptoEligible) != length(RebateIncome)`");
  }
  if (n != IsMarried.length()) {
    stop("`length(IsMarried) != length(RebateIncome)`");
  }
  if (n != SpouseIncome.length()) {
    stop("`length(SpouseIncome) != length(RebateIncome)`");
  }
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    if (SaptoEligible[i]) {
      if (IsMarried[i]) {
        out[i] = sapto_rcpp_singleton(RebateIncome[i],
                                      maxOffsetMarried, 
                                      lowerThresholdMarried, 
                                      taperRateMarried,
                                      true,
                                      SpouseIncome[i],
                                      true);
      } else {
        out[i] = sapto_rcpp_singleton(RebateIncome[i],
                                      maxOffsetSingle, 
                                      lowerThresholdSingle, 
                                      taperRateSingle,
                                      true,
                                      0,
                                      false);
      }
    }
  }
  return out;
}


