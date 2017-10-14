#include <Rcpp.h>
using namespace Rcpp;

double MedicareLevySingle(double income, double lowerThreshold, double upperThreshold, double rate = 0.02, double taper = 0.2, double SpouseIncome = 0, bool isFamily = false, int nDependants = 0, double lowerFamilyThreshold = 46000, double upperFamilyThreshold = 54119, double lowerUpForEachChild = 3306) {
  double familyIncome = income;
  familyIncome += SpouseIncome;
  lowerUpForEachChild *= nDependants;
  lowerFamilyThreshold += lowerUpForEachChild;
  upperFamilyThreshold += lowerUpForEachChild;
  
  double incomeShare = 1;
  if (SpouseIncome > 0) {
    incomeShare = income / familyIncome;
  }
  double tmp = income;
  tmp *= rate;
  if (isFamily && familyIncome <= upperFamilyThreshold && income > lowerThreshold) {
    double tmp = familyIncome - lowerFamilyThreshold;
    tmp *= taper;
    double alt = familyIncome * rate;
    if (tmp > alt) {
      tmp = alt;
    }
    tmp *= incomeShare;
  } else {
    double alt = income - lowerThreshold;
    if (alt < 0) {
      alt = 0;
    } else {
      alt *= taper;
    }
    
    if (alt < tmp) {
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
NumericVector MedicareLevy201213Sapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 32279, 37976, 0.015, 0.1, SpouseIncome[i], NDependants[i], 46000, 54119, 3094);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201213NoSapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 20542, 24168, 0.015, 0.1, SpouseIncome[i], NDependants[i], 33693, 39640, 3094);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201314Sapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 32279, 37976, 0.015, 0.1, SpouseIncome[i], NDependants[i], 46000, 54119, 3156);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201314NoSapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 20542, 24168, 0.015, 0.1, SpouseIncome[i], NDependants[i], 34367, 40433, 3156);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201415Sapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 33044, 41306, 0.020, 0.1, SpouseIncome[i], NDependants[i], 46000, 57501, 3238);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201415NoSapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 20896, 26121, 0.020, 0.1, SpouseIncome[i], NDependants[i], 35261, 44078, 3238);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201516Sapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 33738, 42174, 0.020, 0.1, SpouseIncome[i], NDependants[i], 46966, 58709, 3238);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201516NoSapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 21335, 26670, 0.020, 0.1, SpouseIncome[i], NDependants[i], 35261, 44078, 3238);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201617Sapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 33738, 42174, 0.020, 0.1, SpouseIncome[i], NDependants[i], 46966, 58709, 3306);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201617NoSapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 21335, 26670, 0.020, 0.1, SpouseIncome[i], NDependants[i], 36001, 44077, 3306);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201718Sapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 34244, 42806, 0.020, 0.1, SpouseIncome[i], NDependants[i], 47670, 59589, 3306);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector MedicareLevy201718NoSapto(NumericVector income, NumericVector SpouseIncome, LogicalVector isFamily, IntegerVector NDependants) {
  int n = income.length();
  NumericVector out(n);
  
  for (int i = 0; i < n; ++i) {
    out[i] = MedicareLevySingle(income[i], 21655, 27070, 0.020, 0.1, SpouseIncome[i], NDependants[i], 36541, 44077, 3356);
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




