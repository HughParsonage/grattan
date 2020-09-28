
#include "grattan.h"
#include "Sapto.h"
#include "Person.h"
#include "Medicare.h"
#include "grattanMedicareLevy.h"


const int ages_by_age_range[12] = {72, 67, 62, 57, 52, 47, 42, 37, 32, 27, 22, 17};

int do_decode_age_range(int age_range) {
  return ages_by_age_range[age_range];
}

void validate_age_range(IntegerVector x, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    int xi = x[i];
    if (xi < 0 || xi > 11) {
      Rcerr << "`age_range` had values outside [0, 11]. ";
      Rcerr << "First such value: " << x[i] << " at position " << i;
      stop("`age_range` must be an integer vector with values in [0, 11].");
    }
  }
}

// [[Rcpp::export(rng = false)]]
SEXP decode_age_range(SEXP X) {
  if (TYPEOF(X) == NILSXP) {
    return R_NilValue;
  }
  IntegerVector x = X;
  R_xlen_t N = x.length();
  validate_age_range(x, N);
  IntegerVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = ages_by_age_range[x[i]]; 
  }
  return out;
}


double pmax(double x, double y) {
  return (x >= y) ? x : y;
}

double max0(double x) {
  return (x > 0) ? x : 0;
}

int max0(int x) {
  return (x > 0) ? x : 0;
}

double do_lito_yr(int yr, double x) {
  switch(yr) {
  case 2001: {
  {
    double y = LITO_MAX_OFFSET_2001;
    double r = LITO_1ST_TAPER_2001;
    double b1 = LITO_1ST_THRESH_2001;
    return (x < b1) ? y : max0(y + r * (x - b1));
  }
}
    break;
  case 2002: {
    {
      double y = LITO_MAX_OFFSET_2002;
      double r = LITO_1ST_TAPER_2002;
      double b1 = LITO_1ST_THRESH_2002;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2003: {
    {
      double y = LITO_MAX_OFFSET_2003;
      double r = LITO_1ST_TAPER_2003;
      double b1 = LITO_1ST_THRESH_2003;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2004: {
    {
      double y = LITO_MAX_OFFSET_2004;
      double r = LITO_1ST_TAPER_2004;
      double b1 = LITO_1ST_THRESH_2004;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2005: {
    {
      double y = LITO_MAX_OFFSET_2005;
      double r = LITO_1ST_TAPER_2005;
      double b1 = LITO_1ST_THRESH_2005;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2006: {
    {
      double y = LITO_MAX_OFFSET_2006;
      double r = LITO_1ST_TAPER_2006;
      double b1 = LITO_1ST_THRESH_2006;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2007: {
    {
      double y = LITO_MAX_OFFSET_2007;
      double r = LITO_1ST_TAPER_2007;
      double b1 = LITO_1ST_THRESH_2007;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2008: {
    {
      double y = LITO_MAX_OFFSET_2008;
      double r = LITO_1ST_TAPER_2008;
      double b1 = LITO_1ST_THRESH_2008;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2009: {
    {
      double y = LITO_MAX_OFFSET_2009;
      double r = LITO_1ST_TAPER_2009;
      double b1 = LITO_1ST_THRESH_2009;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2010: {
    {
      double y = LITO_MAX_OFFSET_2010;
      double r = LITO_1ST_TAPER_2010;
      double b1 = LITO_1ST_THRESH_2010;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2011: {
    {
      double y = LITO_MAX_OFFSET_2011;
      double r = LITO_1ST_TAPER_2011;
      double b1 = LITO_1ST_THRESH_2011;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2012: {
    {
      double y = LITO_MAX_OFFSET_2012;
      double r = LITO_1ST_TAPER_2012;
      double b1 = LITO_1ST_THRESH_2012;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2013: {
    {
      double y = LITO_MAX_OFFSET_2013;
      double r = LITO_1ST_TAPER_2013;
      double b1 = LITO_1ST_THRESH_2013;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2014: {
    {
      double y = LITO_MAX_OFFSET_2014;
      double r = LITO_1ST_TAPER_2014;
      double b1 = LITO_1ST_THRESH_2014;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2015: {
    {
      double y = LITO_MAX_OFFSET_2015;
      double r = LITO_1ST_TAPER_2015;
      double b1 = LITO_1ST_THRESH_2015;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2016: {
    {
      double y = LITO_MAX_OFFSET_2016;
      double r = LITO_1ST_TAPER_2016;
      double b1 = LITO_1ST_THRESH_2016;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2017: {
    {
      double y = LITO_MAX_OFFSET_2017;
      double r = LITO_1ST_TAPER_2017;
      double b1 = LITO_1ST_THRESH_2017;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2018: {
    {
      double y = LITO_MAX_OFFSET_2018;
      double r = LITO_1ST_TAPER_2018;
      double b1 = LITO_1ST_THRESH_2018;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2019: {
    {
      double y = LITO_MAX_OFFSET_2019;
      double r = LITO_1ST_TAPER_2019;
      double b1 = LITO_1ST_THRESH_2019;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2020: {
    {
      double y = LITO_MAX_OFFSET_2020;
      double r = LITO_1ST_TAPER_2020;
      double b1 = LITO_1ST_THRESH_2020;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2021: {
    {
      double y = LITO_MAX_OFFSET_2021;
      double r = LITO_1ST_TAPER_2021;
      double b1 = LITO_1ST_THRESH_2021;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2022: {
    {
      double y = LITO_MAX_OFFSET_2022;
      double r = LITO_1ST_TAPER_2022;
      double b1 = LITO_1ST_THRESH_2022;
      return (x < b1) ? y : max0(y + r * (x - b1));
    }
  }
    break;
  case 2023: {
    {
      double y = LITO_MAX_OFFSET_2023;
      
      double b1 = LITO_1ST_THRESH_2023;
      double b2 = LITO_2ND_THRESH_2023;
      double r1 = LITO_1ST_TAPER_2023;
      double r2 = LITO_2ND_TAPER_2023;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2024: {
    {
      double y = LITO_MAX_OFFSET_2024;
      
      double b1 = LITO_1ST_THRESH_2024;
      double b2 = LITO_2ND_THRESH_2024;
      double r1 = LITO_1ST_TAPER_2024;
      double r2 = LITO_2ND_TAPER_2024;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2025: {
    {
      double y = LITO_MAX_OFFSET_2025;
      
      double b1 = LITO_1ST_THRESH_2025;
      double b2 = LITO_2ND_THRESH_2025;
      double r1 = LITO_1ST_TAPER_2025;
      double r2 = LITO_2ND_TAPER_2025;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2026: {
    {
      double y = LITO_MAX_OFFSET_2026;
      
      double b1 = LITO_1ST_THRESH_2026;
      double b2 = LITO_2ND_THRESH_2026;
      double r1 = LITO_1ST_TAPER_2026;
      double r2 = LITO_2ND_TAPER_2026;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2027: {
    {
      double y = LITO_MAX_OFFSET_2027;
      
      double b1 = LITO_1ST_THRESH_2027;
      double b2 = LITO_2ND_THRESH_2027;
      double r1 = LITO_1ST_TAPER_2027;
      double r2 = LITO_2ND_TAPER_2027;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2028: {
    {
      double y = LITO_MAX_OFFSET_2028;
      
      double b1 = LITO_1ST_THRESH_2028;
      double b2 = LITO_2ND_THRESH_2028;
      double r1 = LITO_1ST_TAPER_2028;
      double r2 = LITO_2ND_TAPER_2028;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2029: {
    {
      double y = LITO_MAX_OFFSET_2029;
      
      double b1 = LITO_1ST_THRESH_2029;
      double b2 = LITO_2ND_THRESH_2029;
      double r1 = LITO_1ST_TAPER_2029;
      double r2 = LITO_2ND_TAPER_2029;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  case 2030: {
    {
      double y = LITO_MAX_OFFSET_2030;
      
      double b1 = LITO_1ST_THRESH_2030;
      double b2 = LITO_2ND_THRESH_2030;
      double r1 = LITO_1ST_THRESH_2030;
      double r2 = LITO_2ND_TAPER_2030;
      if (x > b1) {
        return y + r1 * (x - b1);
      } else {
        return y + r1 * (b2 - b1) + r2 * (x - b2);
      }
    }
  }
    break;
  }
  return 0;
}

void apply_lito(int yr, double & taxi, double xd) {
  double lito = do_lito_yr(yr, xd);
  if (taxi <= lito) {
    taxi = 0;
  } else {
    taxi -= lito;
  }
}

void apply_lito(double & taxi, int x, double y, double b1, double r1) {
  double lito = (x < b1) ? y : max0(y + r1 * (x - b1));
  if (taxi <= lito) {
    taxi = 0;
  } else {
    taxi -= lito;
  }
}

void apply_lito(double & taxi, int x, double y, double b1, double r1, double b2, double r2) {
  double lito = y;
  if (x > b1) {
    if (x < b2) {
      lito += r1 * (x - b1);
    } else {
      lito += r1 * (b2 - b1) + r2 * (x - b2);
    }
  }
  
  if (taxi <= lito) {
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
    double lito = do_lito_yr(yr, xd);
    out[i] = lito;
  }
  return out;
}

constexpr double LMITO_1ST_OFFSET = 255;
constexpr double LMITO_2ND_LEVEL = 1080;
constexpr double LMITO_THRESHOLDS[4] = {37e3, 48e3, 90e3, 126e3};
constexpr double LMITO_TAPER_RATES[4] = {0.075, 0, -0.03, 0};

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
    // xa = above threshold
    // We express rates in terms of marginal rates *within* brackets, 
    // so if xd is larger than the next bracket, we apply this rate 
    // to the gap between this threshold and the next. First, however,
    // we need to check whether this is the last bracket "b + 1 == nb"
    double xa = (b + 1 < nb && xd >= bracks[b + 1]) ? (bracks[b + 1] - bracks[b]) : (xd - bracks[b]);
    out += xa * rates[b];
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


double do_1_sapto_sf(int &x, int &y, int age, bool is_married, Sapto S) {
  if (age < S.pension_age) {
    // ineligible
    return 0;
  }
  
  // x is rebate income
  // y is spouse rebate income
  double z = x + y;
  int max_offset = is_married ? S.mxo_couple : S.mxo_single;
  int lwr_thresh = is_married ? S.lwr_couple : S.lwr_single;
  double taper = S.taper;
  // int upr_thresh = is_married ? S.upr_couple : S.upr_single;
  
  double o = max_offset + taper * max0(z - lwr_thresh);
  
  // The transfer of unused SAPTO is very complex and frankly unknown, even
  // within govt.  This lines up 'better' than known models.
  if (is_married) {
    double partner_unused_sapto = max_offset + taper * max0(y - lwr_thresh);
    double lito = S.lito_max_offset;
    
    // https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
    // Following the lettering there
    double AA, BB, CC, DD, GG, HH, II, JJ = 0;
    AA = x;
    BB = max_offset / 2;
    CC = BB + partner_unused_sapto;
    DD = CC + lito;
    // https://www.ato.gov.au/law/view/document?DocID=TXR/TR9331/NAT/ATO/00001&PiT=99991231235958
    GG = S.tax_free_thresh + DD / S.first_tax_rate;
    HH = max0(AA - GG);
    II = HH * (-taper);
    
    JJ = max0(CC - II);
    o = (x < GG) ? CC : JJ;
  }
  return o;
}

// apply_sapto(yr, taxi, xd, yd, agei, is_married);
void apply_sapto(int yr, double & taxi, int x, double y, int age, bool is_married) {
  double sapto = do_1_sapto_sf(x, (int)y, age, is_married, SaptoSince2000[yr - 2000]);
  
  if (sapto >= taxi) {
    taxi = 0;
  } else {
    taxi -= sapto;
  }
}

void apply_sapto(double & taxi, int x, double y, int age, bool is_married, Sapto S) {
  double sapto = do_1_sapto_sf(x, y, age, is_married, S);
  
  if (sapto >= taxi) {
    taxi = 0;
  } else {
    taxi -= sapto;
  }
}

void apply_sapto(double & taxi, Person P, Sapto S) {
  double sapto = do_1_sapto_sf(P.xi, P.yi, P.agei, P.is_married, S);
  if (sapto >= taxi) {
    taxi = 0;
  } else {
    taxi -= sapto;
  }
}



void apply_lmito(double & taxi, double xd) {
  double lmito = do_1_lmito(xd);
  if (lmito >= taxi) {
    taxi = 0;
  } else {
    taxi -= lmito;
  }
}

double do_1_ML(Person P, Medicare M) {
  double z = P.xi + P.yi;
  bool pensioner = M.has_sapto_thr && (P.agei >= M.sapto_age);
  
  double lwr_threshold = 
    pensioner ? 
    (P.is_family ? M.lwr_family_sapto : M.lwr_single_sapto) :
    (P.is_family ? M.lwr_family : M.lwr_single);
  
  if (z < lwr_threshold) {
    return 0;
  }
  
  double upr_threshold = 
    pensioner ? 
    (P.is_family ? M.upr_family_sapto : M.upr_single_sapto) :
    (P.is_family ? M.upr_family : M.upr_single);
  
  double upr_over_lwr = upr_threshold / lwr_threshold;
  if (P.n_child) {
    lwr_threshold += M.lwr_thr_up_per_child * P.n_child;
    upr_threshold *= upr_over_lwr;
    upr_threshold = floor(upr_threshold);
  }
  if (z < upr_threshold) {
    return M.taper * (z - lwr_threshold);
  } else {
    return M.rate * z;
  }
}



int get_i_N(IntegerVector x, R_xlen_t i, R_xlen_t N) {
  return (x.length() == N) ? x[i] : x[0];
}

// ds_pers_super_cont == Non_emp_spr_amt
// coalesce to zero as appropriate
int c0(IntegerVector x, R_xlen_t xn, R_xlen_t i, R_xlen_t N) {
  if (xn == N) {
    return x[i] == NA_INTEGER ? 0 : x[i];
  }
  if (xn == 1) {
    return x[0] == NA_INTEGER ? 0 : x[0];
  }
  
  return 0;
}

const double top_marginal_rates_since_1990[43] = 
  {0.48, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47,  
   0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.45, 0.45, 0.45, 0.45, 
   0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45,  
   0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45, 0.45
  };

inline double top_marginal_rate(int yr) {
  // approximate! need to include accurate medicare levy 
  return top_marginal_rates_since_1990[yr - 1990] + 0.02;  
}

// [[Rcpp::export(rng = false)]]
IntegerVector do_rebate_income(SEXP rebateIncome, 
                               IntegerVector ic_taxable_income_loss, 
                               IntegerVector it_rept_empl_super_cont, 
                               IntegerVector sc_empl_cont,
                               IntegerVector ds_pers_super_cont,
                               IntegerVector it_invest_loss,
                               IntegerVector is_net_rent, 
                               IntegerVector it_rept_fringe_benefit, 
                               int yr) {
  if (TYPEOF(rebateIncome) == INTSXP) {
    IntegerVector out = rebateIncome;
    return out;
  }
  
  R_xlen_t N = ic_taxable_income_loss.length();
  R_xlen_t it_rept_empl_super_cont_length = it_rept_empl_super_cont.length();
  R_xlen_t sc_empl_cont_length = sc_empl_cont.length();
  R_xlen_t ds_pers_super_cont_length = ds_pers_super_cont.length();
  R_xlen_t it_invest_loss_length = it_invest_loss.length();
  R_xlen_t is_net_rent_length = is_net_rent.length();
  R_xlen_t it_rept_fringe_benefit_length = it_rept_fringe_benefit.length();
  
  IntegerVector out = no_init(N);
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = ic_taxable_income_loss[i];
    out[i] += c0(it_rept_empl_super_cont, it_rept_empl_super_cont_length, i, N);
    out[i] += c0(sc_empl_cont, sc_empl_cont_length, i, N);
    out[i] += c0(ds_pers_super_cont, ds_pers_super_cont_length, i, N);
    out[i] += c0(it_invest_loss, it_invest_loss_length, i, N);
    out[i] += c0(is_net_rent, is_net_rent_length, i, N);
    out[i] += c0(it_rept_fringe_benefit, it_rept_fringe_benefit_length, i, N) * (1 - top_marginal_rate(yr));
  }
  return out;
}

void check_len(IntegerVector x, R_xlen_t N, std::string msg) {
  if (x.length() != N) {
    Rcerr << msg << " had length = " << x.length() << "\n";
    stop("Internal error: (do_income_tax_sf): lengths differ.");
  }
}


//' @name do_income_tax
//' @title Internal function for income tax.
//' @description Accepts a sample file-like List and a tax year and returns
//' a double vector.
//' 
//' @param c_age_30_june Age of taxpayer at 30 June of the financial year.
//' @param is_net_rent Net rent amount.
//' @param it_invest_loss Net financial income loss.
//' @param rebateIncome (NULL, rebate income).
//' @noRd

// [[Rcpp::export(rng = false)]]
DoubleVector do_income_tax_sf(int yr,
                              R_xlen_t N,
                              IntegerVector ic_taxable_income_loss, 
                              IntegerVector c_age_30_june,
                              SEXP rebateIncome, 
                              IntegerVector is_net_rent,
                              IntegerVector it_property_loss,
                              IntegerVector it_rept_empl_super_cont,
                              IntegerVector sc_empl_cont,
                              IntegerVector it_rept_fringe_benefit,
                              IntegerVector ds_pers_super_cont,
                              IntegerVector it_invest_loss,
                              IntegerVector spc_rebate_income,
                              IntegerVector partner_status,
                              IntegerVector n_dependants) {
  DoubleVector out = no_init(N);
  
  // index for yr -- relative to 2010
  check_yr_validity(yr);
  
  if (N != ic_taxable_income_loss.length()) {
    stop("(do_income_tax_sf): N != ic_taxable_income_loss.length()");
  }
  check_len(is_net_rent, N, "is_net_rent");
  check_len(it_property_loss, N, "it_property_loss");
  check_len(it_rept_empl_super_cont, N, "it_rept_empl_super_cont");
  check_len(sc_empl_cont, N, "sc_empl_cont");
  check_len(it_rept_fringe_benefit, N, "it_rept_fringe_benefit");
  check_len(ds_pers_super_cont, N, "ds_pers_super_cont");
  check_len(it_invest_loss, N, "it_invest_loss");
  check_len(spc_rebate_income, N, "spc_rebate_income");
  check_len(partner_status, N, "partner_status");
  check_len(n_dependants, N, "n_dependants");
  
  IntegerVector rebate_income = 
    do_rebate_income(rebateIncome, 
                     ic_taxable_income_loss, 
                     it_rept_empl_super_cont, 
                     sc_empl_cont,
                     ds_pers_super_cont,
                     it_invest_loss,
                     is_net_rent, 
                     it_rept_fringe_benefit, 
                     yr);
  
  switch (yr) {
  case 1990: {
      {
        for (R_xlen_t i = 0; i < N; ++i) {
          int xi = ic_taxable_income_loss[i];
          int rebate_incomei = rebate_income[i];
          int n_dependantsi = get_i_N(n_dependants, i, N);
          double xd = (double)xi;
          double yd = (double)get_i_N(spc_rebate_income, i, N);
          bool is_married = yd > 0 || get_i_N(partner_status, i, N);
          bool is_family = is_married || n_dependantsi;
          double taxi = 0;
          // ordinary tax
          taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1990, ORD_TAX_RATES_1990);
          // Offsets
          
          // Medicare levy
          double ml = do_1_medicare_levy_1990(xd, yd, is_family, n_dependantsi);
          // Budget levies
          out[i] = taxi + ml;
        }
      }
    }
    break;
  case 1991: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1991, ORD_TAX_RATES_1991);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1991(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1992: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1992, ORD_TAX_RATES_1992);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1992(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1993: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1993, ORD_TAX_RATES_1993);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1993(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1994: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1994, ORD_TAX_RATES_1994);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1994(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1995: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1995, ORD_TAX_RATES_1995);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1995(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1996: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1996, ORD_TAX_RATES_1996);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1996(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1997: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1997, ORD_TAX_RATES_1997);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1997(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1998: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1998, ORD_TAX_RATES_1998);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1998(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 1999: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_1999, ORD_TAX_RATES_1999);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_1999(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2000: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2000, ORD_TAX_RATES_2000);
        // Offsets
        
        // Medicare levy
        double ml = do_1_medicare_levy_2000(xd, yd, is_family, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2001: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2001, ORD_TAX_RATES_2001);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2001(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2002: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2002, ORD_TAX_RATES_2002);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2002(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2003: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2003, ORD_TAX_RATES_2003);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2003(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2004: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2004, ORD_TAX_RATES_2004);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2004(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2005: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2005, ORD_TAX_RATES_2005);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2005(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2006: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2006, ORD_TAX_RATES_2006);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2006(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2007: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2007, ORD_TAX_RATES_2007);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2007(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2008: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2008, ORD_TAX_RATES_2008);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2008(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2009: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2009, ORD_TAX_RATES_2009);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2009(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2010: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2010, ORD_TAX_RATES_2010);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2010(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2011: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2011, ORD_TAX_RATES_2011);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2011(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        // flood levy
        taxi += 0.005 * (max0(xd - 50e3) * max0(xd - 100e3));
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2012: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2012, ORD_TAX_RATES_2012);
        // Offsets
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2012(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2013: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2013, ORD_TAX_RATES_2013);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          // apply_sapto(int yr, double & taxi, double x, double y, int age, bool is_married)
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2013(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2014: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2014, ORD_TAX_RATES_2014);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2014(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2015: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2015, ORD_TAX_RATES_2015);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2015(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        // temporary budget repair levy
        taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xd - TEMP_BUDGET_REPAIR_LEVY_THRESH);
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2016: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2016, ORD_TAX_RATES_2016);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2016(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        // temporary budget repair levy
        taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xd - TEMP_BUDGET_REPAIR_LEVY_THRESH);
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2017: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2017, ORD_TAX_RATES_2017);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2017(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        // temporary budget repair levy
        taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xd - TEMP_BUDGET_REPAIR_LEVY_THRESH);
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2018: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2018, ORD_TAX_RATES_2018);
        // Offsets
        // SAPTO
        
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2018(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2019: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2019, ORD_TAX_RATES_2019);
        // Offsets
        // SAPTO
        // apply_sapto(yr, taxi, xd, yd, is_married, agei);
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        
        // Medicare levy
        double ml = do_1_medicare_levy_2019(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2020: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2020, ORD_TAX_RATES_2020);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2020(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2021: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2021, ORD_TAX_RATES_2021);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2021(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2022: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2022, ORD_TAX_RATES_2022);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        apply_lmito(taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2022(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2023: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2023, ORD_TAX_RATES_2023);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2023(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2024: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2024, ORD_TAX_RATES_2024);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2024(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2025: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2025, ORD_TAX_RATES_2025);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2025(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2026: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2026, ORD_TAX_RATES_2026);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2026(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2027: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2027, ORD_TAX_RATES_2027);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2027(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2028: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2028, ORD_TAX_RATES_2028);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2028(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2029: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2029, ORD_TAX_RATES_2029);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2029(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  case 2030: {
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        int xi = ic_taxable_income_loss[i];
        int rebate_incomei = rebate_income[i];
        int n_dependantsi = get_i_N(n_dependants, i, N);
        double xd = (double)xi;
        double yd = (double)get_i_N(spc_rebate_income, i, N);
        bool is_married = yd > 0 || get_i_N(partner_status, i, N);
        bool is_family = is_married || n_dependantsi > 0;
        int agei = get_i_N(c_age_30_june, i, N);
        bool pensioner_eligible = agei >= 65;
        double taxi = 0;
        // ordinary tax
        taxi += income_taxi_nb(xd, ORD_TAX_BRACK_2030, ORD_TAX_RATES_2030);
        // Offsets
        // SAPTO
        if (pensioner_eligible) {
          apply_sapto(yr, taxi, rebate_incomei, yd, agei, is_married);
        }
        apply_lito(yr, taxi, xd);
        
        // Medicare levy
        double ml = do_1_medicare_levy_2030(xd, yd, is_family, pensioner_eligible, n_dependantsi);
        // Budget levies
        out[i] = taxi + ml;
      }
    }
  }
    break;
  }
  return out;
}





