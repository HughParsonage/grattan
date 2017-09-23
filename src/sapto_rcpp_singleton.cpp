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

//' SAPTO
//' @name sapto_rcpp
//' 
// [[Rcpp::export]]
double sapto_rcpp_singleton(double rebate_income,
                            double max_offset,
                            double lower_threshold,
                            double threshold_rate,
                            double taper_rate,
                            bool sapto_eligible = true,
                            double Spouse_income = 0,
                            const char* family_status = "single") {
  double sapto = 0;
  if (sapto_eligible) {
    double sapto_income = rebate_income;
    if (strcmp(family_status, "married") == 0) {
      sapto_income += Spouse_income;
    }
    double sapto_value = max_offset;
    sapto_value += lower_threshold * taper_rate;
    sapto_value -= sapto_income * taper_rate;
    if (sapto_value < 0) {
      sapto_value = 0;
    }
    
    double partner_unused_sapto = 0;
    if (strcmp(family_status, "married") == 0) {
      partner_unused_sapto += max_offset / 2;
      partner_unused_sapto += taper_rate * lower_threshold / 2;
      partner_unused_sapto -= taper_rate * Spouse_income;
    }
    
    double lito = 0;
    double AA = rebate_income;
    double BB = max_offset / 2;
    double CC = BB + partner_unused_sapto; 
    double DD = CC + lito;
    double EE = DD / 0.19;
    double FF = EE + 18200;
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
    
    if (strcmp(family_status, "married") == 0) {
      sapto_value = JJ;
      if (rebate_income < GG) {
        sapto_value = CC;
      }
    }
  }
  return sapto;
}


