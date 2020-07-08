#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double MedicareLevySingle(double income, double lowerThreshold, double upperThreshold, double rate = 0.02, double taper = 0.1, double SpouseIncome = 0, bool isFamily = false, int nDependants = 0, double lowerFamilyThreshold = 46000, double upperFamilyThreshold = 54119, double lowerUpForEachChild = 3306) {
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
//' @param income,SpouseIncome,isFamily,NDependants,lowerThreshold,upperThreshold,lowerFamilyThreshold,upperFamilyThreshold,lowerUpForEachChild As in \code{medicare_levy}.
//' @param rate,taper The parameters for the specific year or hypothetical requested.
//' @export MedicareLevy
//' @details For \code{yr > 2018}, the 2017-18 values are used.

// MedicareLevy201314NoSapto MedicareLevy201314Sapto MedicareLevy201213NoSapto MedicareLevy201213Sapto MedicareLevy201415NoSapto MedicareLevy201415Sapto MedicareLevy201516NoSapto MedicareLevy201516Sapto MedicareLevy201617NoSapto MedicareLevy201617Sapto MedicareLevy201718NoSapto MedicareLevy201718Sapto

// [[Rcpp::export]]
NumericVector MedicareLevySaptoYear(NumericVector income,
                                    NumericVector SpouseIncome,
                                    IntegerVector NDependants,
                                    LogicalVector SaptoEligible,
                                    int yr) {
  int n = income.length();
  int n1 = SpouseIncome.length();
  int n2 = NDependants.length();
  int n3 = SaptoEligible.length();
  
  bool r1 = n1 == n;
  bool r2 = n2 == n;
  bool r3 = n3 == n;
  
  NumericVector out(n);
  double sii = SpouseIncome[0];
  int ndi = NDependants[0];
  bool sapto = SaptoEligible[0];
  
  for (int i = 0; i < n; ++i) {
    double ii = income[i];
    if (r1) {
      sii = SpouseIncome[i];
    }
    if (r2) {
      ndi = NDependants[i];
    }
    if (r3) {
      sapto = SaptoEligible[i];
    }
    bool ifi = sii > 0 || ndi > 0;
    if (sapto) {
      // Declare them explicitly so that any argument
      // mismatch is found.
      
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
        out[i] = MedicareLevySingle(ii, 33738, 42174, 0.020, 0.1, sii, ifi, ndi, 46966, 58709, 3306);
        break;
      case 2017:
        out[i] = MedicareLevySingle(ii, 34244, 42806, 0.020, 0.1, sii, ifi, ndi, 47670, 59589, 3356);
        break;
      case 2018:
        out[i] = MedicareLevySingle(ii, 34758, 43449, 0.020, 0.1, sii, ifi, ndi, 48385, 60483, 3406);
        break;
      case 2019:
        out[i] = MedicareLevySingle(ii, 35418, 44272, 0.020, 0.1, sii, ifi, ndi, 49304, 61630, 3471);
        break;
      case 2020:
        out[i] = MedicareLevySingle(ii, 36056, 45069, 0.020, 0.1, sii, ifi, ndi, 50191, 62739, 3533);
        break;
        // # nocov start
      default:
        out[i] = MedicareLevySingle(ii, 36056, 45069, 0.020, 0.1, sii, ifi, ndi, 50191, 62739, 3533);
      break;
      // # nocov end
      }
    } else {
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
        out[i] = MedicareLevySingle(ii, 21665, 27083, 0.020, 0.1, sii, ifi, ndi, 36541, 45676, 3406);
        break;
      case 2018:
        out[i] = MedicareLevySingle(ii, 21980, 27476, 0.020, 0.1, sii, ifi, ndi, 37089, 46364, 3406);
        break;
      case 2019:
        out[i] = MedicareLevySingle(ii, 22398, 27997, 0.020, 0.1, sii, ifi, ndi, 37794, 47242, 3471);
        break;
      case 2020:
        out[i] = MedicareLevySingle(ii, 22801, 28501, 0.020, 0.1, sii, ifi, ndi, 38474, 48092, 3533);
        break;
      default:
        out[i] = MedicareLevySingle(ii, 22801, 28501, 0.020, 0.1, sii, ifi, ndi, 38474, 48092, 3533);
      break;
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy(NumericVector income,
                           NumericVector lowerThreshold,
                           NumericVector upperThreshold,
                           NumericVector SpouseIncome,
                           LogicalVector isFamily,
                           IntegerVector NDependants,
                           NumericVector lowerFamilyThreshold,
                           NumericVector upperFamilyThreshold,
                           NumericVector lowerUpForEachChild,
                           NumericVector rate,
                           NumericVector taper) {
  int n = income.length();
  
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    double incomei = income[i];
    double lowerThresholdi = lowerThreshold[i];
    double upperThresholdi = upperThreshold[i];
    double ratei = rate[i];
    double taperi = taper[i];
    double SpouseIncomei = SpouseIncome[i];
    bool FamilyEligiblei = isFamily[i];
    int nDependantsi = NDependants[i];
    double lowerFamilyThresholdi = lowerFamilyThreshold[i];
    double upperFamilyThresholdi = upperFamilyThreshold[i];
    double lowerUpForEachChildi = lowerUpForEachChild[i];
    
    out[i] = MedicareLevySingle(incomei, lowerThresholdi, upperThresholdi, ratei, taperi, SpouseIncomei, FamilyEligiblei, nDependantsi, lowerFamilyThresholdi, upperFamilyThresholdi, lowerUpForEachChildi);
  }
  return out;
}


// NumericVector MedicareLevy2(NumericVector income, double lowerThreshold, double upperThreshold, double rate, double taper, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector nDependants, double lowerFamilyThreshold = 46000, double upperFamilyThreshold = 54119, double lowerUpForEachChild = 3306) {
//   int n = income.size();
//   NumericVector out(n);
//   
//   for (int i = 0; i < n; ++i) {
//     double ii = income[i];
//     double sii = SpouseIncome[i];
//     bool ifi = isFamily[i];
//     int ndi = nDependants[i];
//     out[i] = MedicareLevySingle(ii, lowerThreshold, upperThreshold, rate, taper, sii, ifi, ndi, lowerFamilyThreshold, upperFamilyThreshold, lowerUpForEachChild);
//   }
//   
//   return out;
// }



