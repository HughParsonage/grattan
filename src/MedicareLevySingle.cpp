#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double MedicareLevySingle(double income, double lowerThreshold, double upperThreshold, double rate = 0.02, double taper = 0.2, double SpouseIncome = 0, bool isFamily = false, int nDependants = 0, double lowerFamilyThreshold = 46000, double upperFamilyThreshold = 54119, double lowerUpForEachChild = 3306) {
  double familyIncome = income;
  familyIncome += SpouseIncome;
  double childExtra = lowerUpForEachChild;
  childExtra *= nDependants;
  lowerFamilyThreshold += childExtra;
  upperFamilyThreshold += childExtra;
  
  double incomeShare = 1;
  if (SpouseIncome > 0) {
    incomeShare = income / familyIncome;
  }
  double tmp = income;
  double alt = income - lowerThreshold;
  tmp *= rate;
  if (isFamily && familyIncome <= upperFamilyThreshold && income > lowerThreshold) {
    tmp = familyIncome - lowerFamilyThreshold;
    tmp *= taper;
    if (tmp < 0) {
      tmp = 0;
    }
    
    alt = familyIncome * rate;
    if (tmp > alt) {
      tmp = alt;
    }
    tmp *= incomeShare;
  } else {
    if (alt < 0) {
      alt = 0;
    } else {
      alt *= taper;
    }
    
    if (tmp > alt) {
      tmp = alt;
    }
  }
  
  return tmp;
}

//' @title Medicare levy in C++
//' @description Medicare levy. Experimental function in C++, equivalent to \code{\link{medicare_levy}}.
//' @name MedicareLevy
//' @param income,SpouseIncome,SaptoEligible,isFamily,nDependants,lowerThreshold,upperThreshold,lowerFamilyThreshold,upperFamilyThreshold,lowerUpForEachChild As in \code{medicare_levy}.
//' @param rate,taper The parameters for the specific year or hypothetical requested.
//' @export MedicareLevy 

// MedicareLevy201314NoSapto MedicareLevy201314Sapto MedicareLevy201213NoSapto MedicareLevy201213Sapto MedicareLevy201415NoSapto MedicareLevy201415Sapto MedicareLevy201516NoSapto MedicareLevy201516Sapto MedicareLevy201617NoSapto MedicareLevy201617Sapto MedicareLevy201718NoSapto MedicareLevy201718Sapto

// [[Rcpp::export]]
NumericVector MedicareLevySaptoYear(NumericVector income,
                                    NumericVector SpouseIncome,
                                    IntegerVector NDependants,
                                    bool sapto,
                                    int yr) {
  int n = income.length();
  NumericVector out(n);
  
  if (sapto) {
    for (int i = 0; i < n; ++i) {
      // Declare them explicitly so that any argument
      // mismatch is found.
      double ii = income[i];
      double sii = SpouseIncome[i];
      int ndi = NDependants[i];
      bool ifi = sii > 0 || ndi > 0;
      switch (yr) {
      case 2013:
        out[i] = MedicareLevySingle(ii, 32279, 37976, 0.015, 0.1, sii, ifi, ndi, 46000, 54119, 3094);
        break;
      case 2014:
        out[i] = MedicareLevySingle(ii, 32279, 37976, 0.015, 0.1, sii, ifi, ndi, 46000, 54119, 3156);
        break;
      case 2015:
        out[i] = MedicareLevySingle(ii, 33044, 41306, 0.020, 0.1, sii, ifi, ndi, 46000, 57501, 3238);
        break;
      case 2016:
        out[i] = MedicareLevySingle(ii, 33738, 42174, 0.020, 0.1, sii, ifi, ndi, 46966, 58709, 3238);
        break;
      case 2017:
        out[i] = MedicareLevySingle(ii, 33738, 42174, 0.020, 0.1, sii, ifi, ndi, 46966, 58709, 3306);
        break;
      case 2018:
        out[i] = MedicareLevySingle(ii, 34244, 42806, 0.020, 0.1, sii, ifi, ndi, 47670, 59589, 3306);
        break;
      default:
        stop("SAPTO not defined for yr provided.");
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      double ii = income[i];
      double sii = SpouseIncome[i];
      int ndi = NDependants[i];
      bool ifi = sii > 0 || ndi > 0;
      switch (yr) {
      case 2013:
        out[i] = MedicareLevySingle(ii, 20542, 24168, 0.015, 0.1, sii, ifi, ndi, 33693, 39640, 3094);
        break;
      case 2014:
        out[i] = MedicareLevySingle(ii, 20542, 24168, 0.015, 0.1, sii, ifi, ndi, 34367, 40433, 3156);
        break;
      case 2015:
        out[i] = MedicareLevySingle(ii, 20896, 26121, 0.020, 0.1, sii, ifi, ndi, 35261, 44078, 3238);
        break;
      case 2016:
        out[i] = MedicareLevySingle(ii, 21335, 26670, 0.020, 0.1, sii, ifi, ndi, 35261, 44078, 3238);
        break;
      case 2017:
        out[i] = MedicareLevySingle(ii, 21335, 26670, 0.020, 0.1, sii, ifi, ndi, 36001, 44077, 3306);
        break;
      case 2018:
        out[i] = MedicareLevySingle(ii, 21655, 27070, 0.020, 0.1, sii, ifi, ndi, 36541, 44077, 3356);
        break;
      default:
        stop("SAPTO not defined for yr provided.");
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy(NumericVector income, double lowerThreshold, double upperThreshold, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants, double lowerFamilyThreshold, double upperFamilyThreshold, double lowerUpForEachChild, double rate = 0.02, double taper = 0.1) {
  int n = income.length();
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    double incomei = income[i];
    double SpouseIncomei = SpouseIncome[i];
    bool FamilyEligiblei = isFamily[i];
    int nDependants = NDependants[i];
    out[i] = MedicareLevySingle(incomei, lowerThreshold, upperThreshold, rate, taper, SpouseIncomei, FamilyEligiblei, nDependants, lowerFamilyThreshold, upperFamilyThreshold, lowerUpForEachChild);
  }
  return out;
}




