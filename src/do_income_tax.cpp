#include "grattan.h"


#include <Rcpp.h>
using namespace Rcpp;

double pmax(double x, double y) {
  return (x >= y) ? x : y;
}

double max0(double x) {
  return (x > 0) ? x : 0;
}

int max0(int x) {
  return (x > 0) ? x : 0;
}

double do_1_lito_2001_2012(int yr, double x) {
  double y = LITO_MAX_OFFSET_2001_2012[yr - 2001];
  double r = LITO_TAPER_RATE_2001_2012;
  double b = LITO_1ST_THRESH_2001_2012[yr - 2001];
  return (x < b) ? y : max0(y + r * (x - b));
}

double do_1_lito_2013_2022(double x) {
  double y = LITO_MAX_OFFSET_2013_2022;
  double r = LITO_TAPER_RATE_2013_2022;
  double b = LITO_1ST_THRESH_2013_2022;
  return (x < b) ? y : max0(y + r * (x - b));
}

double do_1_lito_2023_____(double x) {
  double lito_y = LITO_MAX_OFFSET_2013_2022;
  double lito_b1 = LITO_1ST_THRESH_2023_____;
  double lito_b2 = LITO_2ND_THRESH_2023_____;
  double lito_r1 = LITO_1ST_TAPER_2023_____;
  double lito_r2 = LITO_2ND_TAPER_2023_____;
  if (x > lito_b1) {
    if (x > lito_b2) {
      lito_y += lito_r1 * (lito_b2 - lito_b1) + lito_r2 * (x - lito_b2);
    } else {
      lito_y += lito_r1 * (x - lito_b1);
    }
  }
  return max0(lito_y);
}

void apply_lito(int yr, double taxi, double xd) {
  double lito = 0;
  if (yr < 2013) {
    lito = do_1_lito_2001_2012(yr, xd);
  } else if (yr < 2022) {
    lito = do_1_lito_2013_2022(xd);
  } else {
    lito = do_1_lito_2023_____(xd);
  }
  if (taxi > lito) {
    taxi = 0;
  } else {
    taxi -= lito;
  }
}



// [[Rcpp::export(rng = false)]]
DoubleVector do_lito(DoubleVector x, int yr) {
  R_xlen_t N = x.length();
  DoubleVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {
    double xd = x[i];
    double lito = 0;
    if (yr < 2013) {
      lito = do_1_lito_2001_2012(yr, xd);
    } else if (yr < 2022) {
      lito = do_1_lito_2013_2022(xd);
    } else {
      lito = do_1_lito_2023_____(xd);
    }
    out[i] = lito;
  }
  return out;
}

double do_1_lmito(double x) {
  double out = LMITO_1ST_OFFSET;
  if (x < LMITO_THRESHOLDS[0]) {
    return out;
  }
  out += LMITO_TAPER_RATES[0] * (x - LMITO_THRESHOLDS[0]);
  if (x < LMITO_THRESHOLDS[1]) {
    return out;
  }
  
  out = LMITO_2ND_LEVEL;
  
  if (x >= LMITO_THRESHOLDS[2]) {
    out += LMITO_TAPER_RATES[2] * (x - LMITO_THRESHOLDS[2]);
  }
  return max0(out);
}

// [[Rcpp::export(rng = false)]]
DoubleVector do_lmito(DoubleVector x) {
  // for testing purposes
  R_xlen_t N = x.length();
  DoubleVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {
    
  }
  return out;
}


void check_yr_validity(int yr) {
  if (yr < 1990 || yr > 2030) {
    Rcerr << "yr = " << yr << " outside the range 1990 -- 2030";
    stop("check_yr_validity failed.");
  }
}



// pass an array [std::array is excessive] to this function, with the 
// size of the array
template <int nb>
double income_taxi_nb(double& xd, const double (&bracks)[nb], const double (&rates)[nb]) {
  double out = 0;
  for (int b = 0; b < nb; ++b) {
    if (xd < bracks[b]) {
      break;
    }
    out += (xd - bracks[b]) * rates[b];
  }
  return out;
}

const int NA_ALIAS = INT_MIN;

// [[Rcpp::export(rng = false)]]
int verify_NA_ALIAS(int x = 0) {
  if (NA_ALIAS != NA_INTEGER) {
    stop("NA_ALIAS != NA_INTEGER");
  }
  return x;
}


double income_taxi_yr(double xi, int yr) {
  switch(yr) {
  case NA_ALIAS:
    return income_taxi_nb(xi, ORD_TAX_BRACK_CURRENT_YEAR, ORD_TAX_RATES_CURRENT_YEAR);
  case 1990: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1990, ORD_TAX_RATES_1990);
  case 1991: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1991, ORD_TAX_RATES_1991);
  case 1992: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1992, ORD_TAX_RATES_1992);
  case 1993: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1993, ORD_TAX_RATES_1993);
  case 1994: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1994, ORD_TAX_RATES_1994);
  case 1995: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1995, ORD_TAX_RATES_1995);
  case 1996: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1996, ORD_TAX_RATES_1996);
  case 1997: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1997, ORD_TAX_RATES_1997);
  case 1998: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1998, ORD_TAX_RATES_1998);
  case 1999: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_1999, ORD_TAX_RATES_1999);
  case 2000: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2000, ORD_TAX_RATES_2000);
  case 2001: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2001, ORD_TAX_RATES_2001);
  case 2002: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2002, ORD_TAX_RATES_2002);
  case 2003: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2003, ORD_TAX_RATES_2003);
  case 2004: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2004, ORD_TAX_RATES_2004);
  case 2005: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2005, ORD_TAX_RATES_2005);
  case 2006: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2006, ORD_TAX_RATES_2006);
  case 2007: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2007, ORD_TAX_RATES_2007);
  case 2008: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2008, ORD_TAX_RATES_2008);
  case 2009: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2009, ORD_TAX_RATES_2009);
  case 2010: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2010, ORD_TAX_RATES_2010);
  case 2011: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2011, ORD_TAX_RATES_2011);
  case 2012: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2012, ORD_TAX_RATES_2012);
  case 2013: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2013, ORD_TAX_RATES_2013);
  case 2014: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2014, ORD_TAX_RATES_2014);
  case 2015: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2015, ORD_TAX_RATES_2015);
  case 2016: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2016, ORD_TAX_RATES_2016);
  case 2017: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2017, ORD_TAX_RATES_2017);
  case 2018: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2018, ORD_TAX_RATES_2018);
  case 2019: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2019, ORD_TAX_RATES_2019);
  case 2020: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2020, ORD_TAX_RATES_2020);
  case 2021: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2021, ORD_TAX_RATES_2021);
  case 2022: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2022, ORD_TAX_RATES_2022);
  case 2023: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2023, ORD_TAX_RATES_2023);
  case 2024: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2024, ORD_TAX_RATES_2024);
  case 2025: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2025, ORD_TAX_RATES_2025);
  case 2026: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2026, ORD_TAX_RATES_2026);
  case 2027: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2027, ORD_TAX_RATES_2027);
  case 2028: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2028, ORD_TAX_RATES_2028);
  case 2029: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2029, ORD_TAX_RATES_2029);
  case 2030: 
    return income_taxi_nb(xi, ORD_TAX_BRACK_2030, ORD_TAX_RATES_2030);
  }
  return 0;
}


// sapto
double do_1_sapto_2013_2022(double x, double y, bool is_married) {
  // x is rebate income
  // y is spouse rebate income
  double z = x + y;
  int max_offset = is_married ? SAPTO_MAX_MARRIED_2013_____ : SAPTO_MAX_SINGLE_2013_____;
  int lwr_thresh = is_married ? SAPTO_THRESH_MARRIED_2013_2022 : SAPTO_THRESH_SINGLE_2013_2022;
  
  
  static_assert(SAPTO_TAPER < 0, "SAPTO_TAPER < 0");
  double o = max_offset + SAPTO_TAPER * max0(z - lwr_thresh);
  
  
  if (is_married) {
    double partner_unused_sapto = max_offset + SAPTO_TAPER * max0(y - lwr_thresh);
    double lito = LITO_MAX_OFFSET_2013_2022;
    
    // https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
    // Following the lettering there
    double AA, BB, CC, DD, GG, HH, II, JJ = 0;
    AA = x;
    BB = max_offset / 2;
    CC = BB + partner_unused_sapto;
    DD = CC + lito;
    // https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
    static_assert(ORD_TAX_BRACK_2013[1] == ORD_TAX_BRACK_2020[1],
                  "Tax-free threshold (ordinary tax brackets) changed between 2013 and 2020");
    GG = ORD_TAX_BRACK_2013[1] + DD / ORD_TAX_RATES_2013[1];
    HH = max0(AA - GG);
    II = HH * (-SAPTO_TAPER);
    JJ = max0(CC - II);
    o = (x < GG) ? CC : JJ;
  }
  return o;
}

double do_1_sapto_2023_____(double x, double y, bool is_married) {
  // x is rebate income
  // y is spouse rebate income
  double z = x + y;
  int max_offset = is_married ? SAPTO_MAX_MARRIED_2013_____ : SAPTO_MAX_SINGLE_2013_____;
  int lwr_thresh = is_married ? SAPTO_THRESH_MARRIED_2023_____ : SAPTO_THRESH_SINGLE_2023_____;
  
  
  static_assert(SAPTO_TAPER < 0, "SAPTO_TAPER < 0");
  double o = max_offset + SAPTO_TAPER * max0(z - lwr_thresh);
  
  
  if (is_married) {
    double partner_unused_sapto = max_offset + SAPTO_TAPER * max0(y - lwr_thresh);
    double lito = LITO_MAX_OFFSET_2023_____;
    
    // https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
    // Following the lettering there
    double AA, BB, CC, DD, GG, HH, II, JJ = 0;
    AA = x;
    BB = max_offset / 2;
    CC = BB + partner_unused_sapto;
    DD = CC + lito;
    // https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
    static_assert(ORD_TAX_BRACK_2023[1] == ORD_TAX_BRACK_2030[1],
                  "Tax-free threshold (ordinary tax brackets) changed between 2023 and 2030");
    GG = ORD_TAX_BRACK_2023[1] + DD / ORD_TAX_BRACK_2023[1];
    HH = max0(AA - GG);
    II = HH * (-SAPTO_TAPER);
    JJ = max0(CC - II);
    o = (x < GG) ? CC : JJ;
  }
  return o;
}

void apply_sapto(bool post2023, double taxi, double x, double y, bool is_married) {
  double sapto = post2023 ? do_1_sapto_2023_____(x, y, is_married) :  do_1_sapto_2013_2022(x, y, is_married);
  if (sapto >= taxi) {
    taxi = 0;
  } else {
    taxi -= sapto;
  }
}



void apply_lmito(double taxi, double xd) {
  double lmito = do_1_lmito(xd);
  if (lmito > taxi) {
    taxi = 0;
  } else {
    taxi -= lmito;
  }
}




//' @name do_income_tax
//' @description Accepts a sample file-like List and a tax year and returns
//' a double vector.
//' 
//' @param c_age_30_june Age of taxpayer at 30 June of the financial year.
//' @param is_net_rent Net rent amount.
//' @param it_invest_loss Net financial income loss.
//' @param 
//' 

// [[Rcpp::export(rng = false)]]
DoubleVector do_income_tax_sf(int yr,
                              R_xlen_t N,
                              IntegerVector ic_taxable_income_loss, 
                              IntegerVector c_age_30_june,
                              IntegerVector is_net_rent,
                              IntegerVector it_property_loss,
                              IntegerVector it_rept_empl_super_cont,
                              IntegerVector it_rept_fringe_benefit,
                              IntegerVector it_invest_loss,
                              IntegerVector spc_rebate_income,
                              LogicalVector partner_status) {
  DoubleVector out = no_init(N);
  
  // index for yr -- relative to 2010
  check_yr_validity(yr);
  
  if (N != ic_taxable_income_loss.length()) {
    stop("(do_income_tax_sf): N != ic_taxable_income_loss.length()");
  }
  switch(yr) {
  case 1990: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1990, ORD_TAX_RATES_1990);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1991: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1991, ORD_TAX_RATES_1991);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1992: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1992, ORD_TAX_RATES_1992);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1993: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1993, ORD_TAX_RATES_1993);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1994: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1994, ORD_TAX_RATES_1994);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1995: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1995, ORD_TAX_RATES_1995);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1996: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1996, ORD_TAX_RATES_1996);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1997: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1997, ORD_TAX_RATES_1997);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1998: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1998, ORD_TAX_RATES_1998);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 1999: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1999, ORD_TAX_RATES_1999);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2000: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2000, ORD_TAX_RATES_2000);
        // Offsets
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2001: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2001, ORD_TAX_RATES_2001);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2002: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2002, ORD_TAX_RATES_2002);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2003: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2003, ORD_TAX_RATES_2003);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2004: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2004, ORD_TAX_RATES_2004);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2005: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2005, ORD_TAX_RATES_2005);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2006: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2006, ORD_TAX_RATES_2006);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2007: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2007, ORD_TAX_RATES_2007);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2008: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2008, ORD_TAX_RATES_2008);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2009: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2009, ORD_TAX_RATES_2009);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2010: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2010, ORD_TAX_RATES_2010);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2011: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2011, ORD_TAX_RATES_2011);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        // flood levy
        taxi += 0.005 * (max0(xd - 50e3) * max0(xd - 100e3));
        out[i] = taxi;
      }
    }
  }
    break;
  case 2012: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2012, ORD_TAX_RATES_2012);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2013: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2013, ORD_TAX_RATES_2013);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2014: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2014, ORD_TAX_RATES_2014);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2015: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2015, ORD_TAX_RATES_2015);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        // temporary budget repair levy
        taxi += 0.02 * max0(xd - 180e3);
        out[i] = taxi;
      }
    }
  }
    break;
  case 2016: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2016, ORD_TAX_RATES_2016);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        // temporary budget repair levy
        taxi += 0.02 * max0(xd - 180e3);
        out[i] = taxi;
      }
    }
  }
    break;
  case 2017: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2017, ORD_TAX_RATES_2017);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        // temporary budget repair levy
        taxi += 0.02 * max0(xd - 180e3);
        out[i] = taxi;
      }
    }
  }
    break;
  case 2018: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2018, ORD_TAX_RATES_2018);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2019: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2019, ORD_TAX_RATES_2019);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2020: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2020, ORD_TAX_RATES_2020);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2021: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2021, ORD_TAX_RATES_2021);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2022: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2022, ORD_TAX_RATES_2022);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2023: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2023, ORD_TAX_RATES_2023);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2024: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2024, ORD_TAX_RATES_2024);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2025: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2025, ORD_TAX_RATES_2025);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2026: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2026, ORD_TAX_RATES_2026);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  case 2027: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        double xd = (double)xi;
        double yd = (double)spc_rebate_income[i];
        double is_married = yd > 0 || partner_status[i];
        int agei = c_age_30_june[i];
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2027, ORD_TAX_RATES_2027);
        // Offsets
        // SAPTO
        bool sapto_eligible = agei >= 65;
        if (sapto_eligible) {
          apply_sapto(yr >= 2023, taxi, xd, yd, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        // Budget levies
        out[i] = taxi;
      }
    }
  }
    break;
  }
  return out;
}