#include "grattan.h"
#include <Rcpp.h>
using namespace Rcpp;

//' SAPTO singleton
//' @name sapto_rcpp_singleton
//' @description Length-one version of \code{SAPTO} in C++.
//' @param rebate_income,max_offset,lower_threshold,taper_rate,sapto_eligible,Spouse_income,is_married As in \code{\link{sapto}}.
//' @export
// [[Rcpp::export]]
double sapto_rcpp_singleton(double rebate_income, double max_offset, double lower_threshold, double taper_rate, bool sapto_eligible, double Spouse_income, bool is_married) {
  double sapto_value = 0;
  if (sapto_eligible) {
    double sapto_income = rebate_income;
    sapto_income += Spouse_income;
    sapto_value += max_offset;
    sapto_value += lower_threshold * taper_rate;
    sapto_value -= sapto_income * taper_rate;
    
    if (sapto_value > max_offset) {
      sapto_value = max_offset;
    }
    
    if (sapto_value < 0) {
      sapto_value = 0;
    }
    
    if (is_married) {
      double partner_unused_sapto = 0;
      double BB = max_offset / 2;
      partner_unused_sapto += BB;
      partner_unused_sapto += taper_rate * lower_threshold / 2;
      partner_unused_sapto -= taper_rate * Spouse_income;
      
      if (partner_unused_sapto > BB) {
        partner_unused_sapto = BB;
      } 
      
      if (partner_unused_sapto < 0) {
        partner_unused_sapto = 0;
      }
      
      double lito = 445;
      double AA = rebate_income;
      
      double CC = BB + partner_unused_sapto; 
      double DD = CC + lito;
      // double EE = DD / 0.19;
      // double FF = EE + 18200;
      // # https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
      double GG = 18200 + DD / 0.19;
      // # ATO calculator suggests this was intended:
      // # double GG := 37230;
      double HH = AA - GG;
      if (HH < 0) {
        HH = 0;
      }
      double II = HH / 8;
      double JJ = CC - II;
      if (JJ < 0) {
        JJ = 0;
      }
      
      sapto_value = JJ;
      if (rebate_income < GG) {
        sapto_value = CC;
      }
    }
  }
  return sapto_value;
}


