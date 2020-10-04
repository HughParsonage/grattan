
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

double amin(double x, double y) {
  return (x <= y) ? x : y;
}

int amin(int x, int y) {
  return (x <= y) ? x : y;
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

void apply_lito(double & taxi, int x, int y, double b1, double r1, double b2, double r2) {
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
constexpr int LMITO_THRESHOLDS[4] = {37000, 48000, 90000, 126000};
constexpr double LMITO_TAPER_RATES[4] = {0.075, 0, -0.03, 0};

double do_1_lmito(int x) {
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

template <int nb>
double income_taxi_nb(int& xd, const double (&bracks)[nb], const double (&rates)[nb]) {
  double out = 0;
  for (int b = 0; b < nb; ++b) {
    if (xd < bracks[b]) {
      break;
    }
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
  double max_offset = is_married ? S.mxo_couple : S.mxo_single;
  double lwr_thresh = is_married ? S.lwr_couple : S.lwr_single;
  double taper = S.taper;

   double o = x < lwr_thresh ? max_offset : max0(max_offset + taper * (x - lwr_thresh));
  if (!is_married) {
    return o;
  }
  
  // The transfer of unused SAPTO is very complex and frankly unknown, even
  // within govt.  This lines up 'better' than known models.

  // If the spouse's income is so high that no spouse SAPTO is 
  // transferrable, then we just fall back to the original 
  constexpr double MAX_THR_SPOUSE_XFER_MARRIED = 1602 / SAPTO_S12_TAPER + SAPTO_S12_THRESH;
  if (y > MAX_THR_SPOUSE_XFER_MARRIED) {
    return o;
  }
  
  double sp_unused_sapto = 
  (y < SAPTO_S12_THRESH) ? max_offset : max0(max_offset - SAPTO_S12_TAPER * (y - SAPTO_S12_THRESH));
  
  // https://www.ato.gov.au/individuals/income-and-deductions/in-detail/transferring-the-seniors-and-pensioners-tax-offset/
  // Following the lettering there
  double A = S.mxo_couple;
  double B = A + sp_unused_sapto;
  double C = B + S.lito_max_offset;
  double D = C / S.first_tax_rate;
  double E = D + S.tax_free_thresh;
  double adj_rebate_threshold = E;
  if (E > S.lito_1st_thresh) {
    double G = S.second_tax_rate - S.lito_1st_taper; // 0.34
    double H = G - S.first_tax_rate;                 // 0.15
    double I = H * S.lito_1st_thresh;                // 5550
    double J = S.first_tax_rate * S.tax_free_thresh; // 3458
    double K = J + S.lito_max_offset;                // 3903
    double L = K + max_offset;
    double M = L + sp_unused_sapto;
    double N = I + M;
    double O = G;
    double P = N / O;                                // 37226
    adj_rebate_threshold = P;
  }
  if (x < adj_rebate_threshold) {
    return B;
  }
  
  double DD = x - adj_rebate_threshold;
  double EE = DD * taper;
  double FF = B + EE;
  
  return max0(FF);
}

void apply_sapto(double & taxi, int x, int y, int age, bool is_married, Sapto S) {
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

// [[Rcpp::export(rng = false)]]
DoubleVector do_sapto(IntegerVector x, IntegerVector y, 
                      IntegerVector Age,
                      LogicalVector isMarried, 
                      double max_single = 2230, 
                      double max_couple = 1602, 
                      double lwr_single = 32279,
                      double lwr_couple = 28974,
                      double taper = -0.125,
                      double tax_free_thresh = 18200,
                      double first_tax_rate = 0.19,
                      double second_tax_rate = 0.325,
                      double lito_max_offset = 445,
                      double lito_1st_thresh = 37e3,
                      double lito_1st_taper = -0.015) {
  Sapto S;
  S.mxo_single = max_single;
  S.mxo_couple = max_couple;
  S.first_tax_rate = first_tax_rate;
  S.second_tax_rate = second_tax_rate;
  S.tax_free_thresh = tax_free_thresh;
  S.lwr_couple = lwr_couple;
  S.pension_age = 65;
  S.taper = taper;
  S.lito_max_offset = lito_max_offset;
  S.lito_1st_thresh = lito_1st_thresh;
  S.lito_1st_taper = lito_1st_taper;
  
  R_xlen_t N = x.length();
  R_xlen_t AN = Age.length();
  R_xlen_t MN = isMarried.length();
  DoubleVector out = no_init(N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int agei = (AN == N) ? Age[i] : Age[0];
    bool is_married = (MN == N) ? isMarried[i] : isMarried[0];
    double sapto = do_1_sapto_sf(x[i], y[i], agei, is_married, S);
    out[i] = sapto;
  }
  return out;
}


void apply_lmito(double & taxi, int x) {
  double lmito = do_1_lmito(x);
  if (lmito >= taxi) {
    taxi = 0;
  } else {
    taxi -= lmito;
  }
}

double do_1_ML(Person P, Medicare M) {
  bool sapto = P.agei >= 65;
  double lower_threshold = sapto ? M.lwr_single_sapto : M.lwr_single;
  if (P.xi < lower_threshold) {
    return 0;
  }
  if (P.is_family) {
    // subs.8(5) of Act
    double upr_over_lwr = M.upr_family / ((double)M.lwr_family);
    double lower_family_threshold = (sapto ? M.lwr_family_sapto : M.lwr_family) + P.n_child * M.lwr_thr_up_per_child;
    double upper_family_threshold = upr_over_lwr * lower_family_threshold;
    double family_income = P.xi + P.yi;
    // # Levy in the case of small incomes (s.7 of Act)
    if (family_income <= upper_family_threshold) {
      double income_share = P.yi > 0 ? (P.xi / family_income) : 1.0;
      double o1 = max0(M.taper * (family_income - lower_family_threshold));
      double o2 = M.rate * family_income;
      double o = (o1 < o2) ? o1 : o2;
      return income_share * o;
    }
  }
  
  double o1 = M.taper * (P.xi - lower_threshold);
  double o2 = M.rate * P.xi;
  return (o1 < o2) ? o1 : o2;
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

int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
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
  
  
  
  
  switch(yr) {
  case 1984: {
      Medicare M1984;
      M1984.lwr_single = ML_LWR_THRESHOLD_SINGLE_1984;
      M1984.upr_single = ML_UPR_THRESHOLD_SINGLE_1984;
      M1984.lwr_family = ML_LWR_THRESHOLD_FAMILY_1984;
      M1984.upr_family = ML_UPR_THRESHOLD_FAMILY_1984;
      M1984.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1984;
      M1984.taper = ML_TAPER_1984;
      M1984.rate = ML_RATE_1984;
      M1984.has_sapto_thr = 0;
      M1984.sapto_age = 65;
      {
        for (R_xlen_t i = 0; i < N; ++i) {
          Person P;
          int xi = ic_taxable_income_loss[i];
          double taxi = 0;
          
          P.xi = xi;
          P.yi = c0(spc_rebate_income[i]);
          P.agei = c_age_30_june[i];
          P.is_married = partner_status[i];
          P.n_child = n_dependants[i];
          P.is_family = P.is_married || P.n_child;
          
          taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1984, ORD_TAX_RATES_1984);
          
          // Medicare levy
          double ml = do_1_ML(P, M1984);
          taxi += ml;
          
          
          // finally
          out[i] = taxi;
        }
        
      } // inner case 1984
    } // outer case 1984
    break;
    
  case 1985: {
    Medicare M1985;
    M1985.lwr_single = ML_LWR_THRESHOLD_SINGLE_1985;
    M1985.upr_single = ML_UPR_THRESHOLD_SINGLE_1985;
    M1985.lwr_family = ML_LWR_THRESHOLD_FAMILY_1985;
    M1985.upr_family = ML_UPR_THRESHOLD_FAMILY_1985;
    M1985.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1985;
    M1985.taper = ML_TAPER_1985;
    M1985.rate = ML_RATE_1985;
    M1985.has_sapto_thr = 0;
    M1985.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1985, ORD_TAX_RATES_1985);
        
        // Medicare levy
        double ml = do_1_ML(P, M1985);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1985
  } // outer case 1985
    break;
    
  case 1986: {
    Medicare M1986;
    M1986.lwr_single = ML_LWR_THRESHOLD_SINGLE_1986;
    M1986.upr_single = ML_UPR_THRESHOLD_SINGLE_1986;
    M1986.lwr_family = ML_LWR_THRESHOLD_FAMILY_1986;
    M1986.upr_family = ML_UPR_THRESHOLD_FAMILY_1986;
    M1986.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1986;
    M1986.taper = ML_TAPER_1986;
    M1986.rate = ML_RATE_1986;
    M1986.has_sapto_thr = 0;
    M1986.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1986, ORD_TAX_RATES_1986);
        
        // Medicare levy
        double ml = do_1_ML(P, M1986);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1986
  } // outer case 1986
    break;
    
  case 1987: {
    Medicare M1987;
    M1987.lwr_single = ML_LWR_THRESHOLD_SINGLE_1987;
    M1987.upr_single = ML_UPR_THRESHOLD_SINGLE_1987;
    M1987.lwr_family = ML_LWR_THRESHOLD_FAMILY_1987;
    M1987.upr_family = ML_UPR_THRESHOLD_FAMILY_1987;
    M1987.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1987;
    M1987.taper = ML_TAPER_1987;
    M1987.rate = ML_RATE_1987;
    M1987.has_sapto_thr = 0;
    M1987.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1987, ORD_TAX_RATES_1987);
        
        // Medicare levy
        double ml = do_1_ML(P, M1987);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1987
  } // outer case 1987
    break;
    
  case 1988: {
    Medicare M1988;
    M1988.lwr_single = ML_LWR_THRESHOLD_SINGLE_1988;
    M1988.upr_single = ML_UPR_THRESHOLD_SINGLE_1988;
    M1988.lwr_family = ML_LWR_THRESHOLD_FAMILY_1988;
    M1988.upr_family = ML_UPR_THRESHOLD_FAMILY_1988;
    M1988.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1988;
    M1988.taper = ML_TAPER_1988;
    M1988.rate = ML_RATE_1988;
    M1988.has_sapto_thr = 0;
    M1988.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1988, ORD_TAX_RATES_1988);
        
        // Medicare levy
        double ml = do_1_ML(P, M1988);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1988
  } // outer case 1988
    break;
    
  case 1989: {
    Medicare M1989;
    M1989.lwr_single = ML_LWR_THRESHOLD_SINGLE_1989;
    M1989.upr_single = ML_UPR_THRESHOLD_SINGLE_1989;
    M1989.lwr_family = ML_LWR_THRESHOLD_FAMILY_1989;
    M1989.upr_family = ML_UPR_THRESHOLD_FAMILY_1989;
    M1989.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1989;
    M1989.taper = ML_TAPER_1989;
    M1989.rate = ML_RATE_1989;
    M1989.has_sapto_thr = 0;
    M1989.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1989, ORD_TAX_RATES_1989);
        
        // Medicare levy
        double ml = do_1_ML(P, M1989);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1989
  } // outer case 1989
    break;
    
  case 1990: {
    Medicare M1990;
    M1990.lwr_single = ML_LWR_THRESHOLD_SINGLE_1990;
    M1990.upr_single = ML_UPR_THRESHOLD_SINGLE_1990;
    M1990.lwr_family = ML_LWR_THRESHOLD_FAMILY_1990;
    M1990.upr_family = ML_UPR_THRESHOLD_FAMILY_1990;
    M1990.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1990;
    M1990.taper = ML_TAPER_1990;
    M1990.rate = ML_RATE_1990;
    M1990.has_sapto_thr = 0;
    M1990.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1990, ORD_TAX_RATES_1990);
        
        // Medicare levy
        double ml = do_1_ML(P, M1990);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1990
  } // outer case 1990
    break;
    
  case 1991: {
    Medicare M1991;
    M1991.lwr_single = ML_LWR_THRESHOLD_SINGLE_1991;
    M1991.upr_single = ML_UPR_THRESHOLD_SINGLE_1991;
    M1991.lwr_family = ML_LWR_THRESHOLD_FAMILY_1991;
    M1991.upr_family = ML_UPR_THRESHOLD_FAMILY_1991;
    M1991.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1991;
    M1991.taper = ML_TAPER_1991;
    M1991.rate = ML_RATE_1991;
    M1991.has_sapto_thr = 0;
    M1991.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1991, ORD_TAX_RATES_1991);
        
        // Medicare levy
        double ml = do_1_ML(P, M1991);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1991
  } // outer case 1991
    break;
    
  case 1992: {
    Medicare M1992;
    M1992.lwr_single = ML_LWR_THRESHOLD_SINGLE_1992;
    M1992.upr_single = ML_UPR_THRESHOLD_SINGLE_1992;
    M1992.lwr_family = ML_LWR_THRESHOLD_FAMILY_1992;
    M1992.upr_family = ML_UPR_THRESHOLD_FAMILY_1992;
    M1992.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1992;
    M1992.taper = ML_TAPER_1992;
    M1992.rate = ML_RATE_1992;
    M1992.has_sapto_thr = 0;
    M1992.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1992, ORD_TAX_RATES_1992);
        
        // Medicare levy
        double ml = do_1_ML(P, M1992);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1992
  } // outer case 1992
    break;
    
  case 1993: {
    Medicare M1993;
    M1993.lwr_single = ML_LWR_THRESHOLD_SINGLE_1993;
    M1993.upr_single = ML_UPR_THRESHOLD_SINGLE_1993;
    M1993.lwr_family = ML_LWR_THRESHOLD_FAMILY_1993;
    M1993.upr_family = ML_UPR_THRESHOLD_FAMILY_1993;
    M1993.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1993;
    M1993.taper = ML_TAPER_1993;
    M1993.rate = ML_RATE_1993;
    M1993.has_sapto_thr = 0;
    M1993.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1993, ORD_TAX_RATES_1993);
        
        // Medicare levy
        double ml = do_1_ML(P, M1993);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1993
  } // outer case 1993
    break;
    
  case 1994: {
    Medicare M1994;
    M1994.lwr_single = ML_LWR_THRESHOLD_SINGLE_1994;
    M1994.upr_single = ML_UPR_THRESHOLD_SINGLE_1994;
    M1994.lwr_family = ML_LWR_THRESHOLD_FAMILY_1994;
    M1994.upr_family = ML_UPR_THRESHOLD_FAMILY_1994;
    M1994.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1994;
    M1994.taper = ML_TAPER_1994;
    M1994.rate = ML_RATE_1994;
    M1994.has_sapto_thr = 0;
    M1994.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1994, ORD_TAX_RATES_1994);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_1994, LITO_1ST_THRESH_1994, LITO_1ST_TAPER_1994);
        
        // Medicare levy
        double ml = do_1_ML(P, M1994);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1994
  } // outer case 1994
    break;
    
  case 1995: {
    Medicare M1995;
    M1995.lwr_single = ML_LWR_THRESHOLD_SINGLE_1995;
    M1995.upr_single = ML_UPR_THRESHOLD_SINGLE_1995;
    M1995.lwr_family = ML_LWR_THRESHOLD_FAMILY_1995;
    M1995.upr_family = ML_UPR_THRESHOLD_FAMILY_1995;
    M1995.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1995;
    M1995.taper = ML_TAPER_1995;
    M1995.rate = ML_RATE_1995;
    M1995.has_sapto_thr = 0;
    M1995.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1995, ORD_TAX_RATES_1995);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_1995, LITO_1ST_THRESH_1995, LITO_1ST_TAPER_1995);
        
        // Medicare levy
        double ml = do_1_ML(P, M1995);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1995
  } // outer case 1995
    break;
    
  case 1996: {
    Medicare M1996;
    M1996.lwr_single = ML_LWR_THRESHOLD_SINGLE_1996;
    M1996.upr_single = ML_UPR_THRESHOLD_SINGLE_1996;
    M1996.lwr_family = ML_LWR_THRESHOLD_FAMILY_1996;
    M1996.upr_family = ML_UPR_THRESHOLD_FAMILY_1996;
    M1996.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1996;
    M1996.taper = ML_TAPER_1996;
    M1996.rate = ML_RATE_1996;
    M1996.has_sapto_thr = 0;
    M1996.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1996, ORD_TAX_RATES_1996);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_1996, LITO_1ST_THRESH_1996, LITO_1ST_TAPER_1996);
        
        // Medicare levy
        double ml = do_1_ML(P, M1996);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1996
  } // outer case 1996
    break;
    
  case 1997: {
    Medicare M1997;
    M1997.lwr_single = ML_LWR_THRESHOLD_SINGLE_1997;
    M1997.upr_single = ML_UPR_THRESHOLD_SINGLE_1997;
    M1997.lwr_family = ML_LWR_THRESHOLD_FAMILY_1997;
    M1997.upr_family = ML_UPR_THRESHOLD_FAMILY_1997;
    M1997.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1997;
    M1997.taper = ML_TAPER_1997;
    M1997.rate = ML_RATE_1997;
    M1997.has_sapto_thr = 0;
    M1997.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1997, ORD_TAX_RATES_1997);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_1997, LITO_1ST_THRESH_1997, LITO_1ST_TAPER_1997);
        
        // Medicare levy
        double ml = do_1_ML(P, M1997);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1997
  } // outer case 1997
    break;
    
  case 1998: {
    Medicare M1998;
    M1998.lwr_single = ML_LWR_THRESHOLD_SINGLE_1998;
    M1998.upr_single = ML_UPR_THRESHOLD_SINGLE_1998;
    M1998.lwr_family = ML_LWR_THRESHOLD_FAMILY_1998;
    M1998.upr_family = ML_UPR_THRESHOLD_FAMILY_1998;
    M1998.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1998;
    M1998.taper = ML_TAPER_1998;
    M1998.rate = ML_RATE_1998;
    M1998.has_sapto_thr = 0;
    M1998.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1998, ORD_TAX_RATES_1998);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_1998, LITO_1ST_THRESH_1998, LITO_1ST_TAPER_1998);
        
        // Medicare levy
        double ml = do_1_ML(P, M1998);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1998
  } // outer case 1998
    break;
    
  case 1999: {
    Medicare M1999;
    M1999.lwr_single = ML_LWR_THRESHOLD_SINGLE_1999;
    M1999.upr_single = ML_UPR_THRESHOLD_SINGLE_1999;
    M1999.lwr_family = ML_LWR_THRESHOLD_FAMILY_1999;
    M1999.upr_family = ML_UPR_THRESHOLD_FAMILY_1999;
    M1999.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_1999;
    M1999.taper = ML_TAPER_1999;
    M1999.rate = ML_RATE_1999;
    M1999.has_sapto_thr = 0;
    M1999.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_1999, ORD_TAX_RATES_1999);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_1999, LITO_1ST_THRESH_1999, LITO_1ST_TAPER_1999);
        
        // Medicare levy
        double ml = do_1_ML(P, M1999);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 1999
  } // outer case 1999
    break;
    
  case 2000: {
    // Create a Sapto struct for this year
    Sapto Sapto2000;
    Sapto2000.year = 2000;
    Sapto2000.pension_age = 65;
    Sapto2000.mxo_single = SAPTO_MAX_SINGLE_2000;
    Sapto2000.mxo_couple = SAPTO_MAX_MARRIED_2000;
    Sapto2000.lwr_single = SAPTO_LWR_SINGLE_2000;
    Sapto2000.lwr_couple = SAPTO_LWR_MARRIED_2000;
    Sapto2000.upr_single = SAPTO_LWR_SINGLE_2000 + SAPTO_MAX_SINGLE_2000 / SAPTO_TAPER_2000;
    Sapto2000.upr_couple = SAPTO_LWR_MARRIED_2000 + SAPTO_MAX_MARRIED_2000 / SAPTO_TAPER_2000;
    Sapto2000.taper = SAPTO_TAPER_2000;
    Sapto2000.first_tax_rate = ORD_TAX_RATES_2000[1];
    Sapto2000.tax_free_thresh = ORD_TAX_BRACK_2000[1];
    Sapto2000.lito_max_offset = LITO_MAX_OFFSET_2000;
    
    Medicare M2000;
    M2000.lwr_single = ML_LWR_THRESHOLD_SINGLE_2000;
    M2000.upr_single = ML_UPR_THRESHOLD_SINGLE_2000;
    M2000.lwr_family = ML_LWR_THRESHOLD_FAMILY_2000;
    M2000.upr_family = ML_UPR_THRESHOLD_FAMILY_2000;
    M2000.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_2000;
    M2000.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_2000;
    M2000.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_2000;
    M2000.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_2000;
    M2000.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2000;
    M2000.taper = ML_TAPER_2000;
    M2000.rate = ML_RATE_2000;
    M2000.has_sapto_thr = 0;
    M2000.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2000, ORD_TAX_RATES_2000);
        apply_sapto(taxi, P, Sapto2000);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2000, LITO_1ST_THRESH_2000, LITO_1ST_TAPER_2000);
        
        // Medicare levy
        double ml = do_1_ML(P, M2000);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2000
  } // outer case 2000
    break;
    
  case 2001: {
    // Create a Sapto struct for this year
    Sapto Sapto2001;
    Sapto2001.year = 2001;
    Sapto2001.pension_age = 65;
    Sapto2001.mxo_single = SAPTO_MAX_SINGLE_2001;
    Sapto2001.mxo_couple = SAPTO_MAX_MARRIED_2001;
    Sapto2001.lwr_single = SAPTO_LWR_SINGLE_2001;
    Sapto2001.lwr_couple = SAPTO_LWR_MARRIED_2001;
    Sapto2001.upr_single = SAPTO_LWR_SINGLE_2001 + SAPTO_MAX_SINGLE_2001 / SAPTO_TAPER_2001;
    Sapto2001.upr_couple = SAPTO_LWR_MARRIED_2001 + SAPTO_MAX_MARRIED_2001 / SAPTO_TAPER_2001;
    Sapto2001.taper = SAPTO_TAPER_2001;
    Sapto2001.first_tax_rate = ORD_TAX_RATES_2001[1];
    Sapto2001.tax_free_thresh = ORD_TAX_BRACK_2001[1];
    Sapto2001.lito_max_offset = LITO_MAX_OFFSET_2001;
    
    Medicare M2001;
    M2001.lwr_single = ML_LWR_THRESHOLD_SINGLE_2001;
    M2001.upr_single = ML_UPR_THRESHOLD_SINGLE_2001;
    M2001.lwr_family = ML_LWR_THRESHOLD_FAMILY_2001;
    M2001.upr_family = ML_UPR_THRESHOLD_FAMILY_2001;
    M2001.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2001;
    M2001.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2001;
    M2001.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2001;
    M2001.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2001;
    M2001.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2001;
    M2001.taper = ML_TAPER_2001;
    M2001.rate = ML_RATE_2001;
    M2001.has_sapto_thr = 1;
    M2001.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2001, ORD_TAX_RATES_2001);
        apply_sapto(taxi, P, Sapto2001);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2001, LITO_1ST_THRESH_2001, LITO_1ST_TAPER_2001);
        
        // Medicare levy
        double ml = do_1_ML(P, M2001);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2001
  } // outer case 2001
    break;
    
  case 2002: {
    // Create a Sapto struct for this year
    Sapto Sapto2002;
    Sapto2002.year = 2002;
    Sapto2002.pension_age = 65;
    Sapto2002.mxo_single = SAPTO_MAX_SINGLE_2002;
    Sapto2002.mxo_couple = SAPTO_MAX_MARRIED_2002;
    Sapto2002.lwr_single = SAPTO_LWR_SINGLE_2002;
    Sapto2002.lwr_couple = SAPTO_LWR_MARRIED_2002;
    Sapto2002.upr_single = SAPTO_LWR_SINGLE_2002 + SAPTO_MAX_SINGLE_2002 / SAPTO_TAPER_2002;
    Sapto2002.upr_couple = SAPTO_LWR_MARRIED_2002 + SAPTO_MAX_MARRIED_2002 / SAPTO_TAPER_2002;
    Sapto2002.taper = SAPTO_TAPER_2002;
    Sapto2002.first_tax_rate = ORD_TAX_RATES_2002[1];
    Sapto2002.tax_free_thresh = ORD_TAX_BRACK_2002[1];
    Sapto2002.lito_max_offset = LITO_MAX_OFFSET_2002;
    
    Medicare M2002;
    M2002.lwr_single = ML_LWR_THRESHOLD_SINGLE_2002;
    M2002.upr_single = ML_UPR_THRESHOLD_SINGLE_2002;
    M2002.lwr_family = ML_LWR_THRESHOLD_FAMILY_2002;
    M2002.upr_family = ML_UPR_THRESHOLD_FAMILY_2002;
    M2002.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2002;
    M2002.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2002;
    M2002.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2002;
    M2002.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2002;
    M2002.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2002;
    M2002.taper = ML_TAPER_2002;
    M2002.rate = ML_RATE_2002;
    M2002.has_sapto_thr = 1;
    M2002.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2002, ORD_TAX_RATES_2002);
        apply_sapto(taxi, P, Sapto2002);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2002, LITO_1ST_THRESH_2002, LITO_1ST_TAPER_2002);
        
        // Medicare levy
        double ml = do_1_ML(P, M2002);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2002
  } // outer case 2002
    break;
    
  case 2003: {
    // Create a Sapto struct for this year
    Sapto Sapto2003;
    Sapto2003.year = 2003;
    Sapto2003.pension_age = 65;
    Sapto2003.mxo_single = SAPTO_MAX_SINGLE_2003;
    Sapto2003.mxo_couple = SAPTO_MAX_MARRIED_2003;
    Sapto2003.lwr_single = SAPTO_LWR_SINGLE_2003;
    Sapto2003.lwr_couple = SAPTO_LWR_MARRIED_2003;
    Sapto2003.upr_single = SAPTO_LWR_SINGLE_2003 + SAPTO_MAX_SINGLE_2003 / SAPTO_TAPER_2003;
    Sapto2003.upr_couple = SAPTO_LWR_MARRIED_2003 + SAPTO_MAX_MARRIED_2003 / SAPTO_TAPER_2003;
    Sapto2003.taper = SAPTO_TAPER_2003;
    Sapto2003.first_tax_rate = ORD_TAX_RATES_2003[1];
    Sapto2003.tax_free_thresh = ORD_TAX_BRACK_2003[1];
    Sapto2003.lito_max_offset = LITO_MAX_OFFSET_2003;
    
    Medicare M2003;
    M2003.lwr_single = ML_LWR_THRESHOLD_SINGLE_2003;
    M2003.upr_single = ML_UPR_THRESHOLD_SINGLE_2003;
    M2003.lwr_family = ML_LWR_THRESHOLD_FAMILY_2003;
    M2003.upr_family = ML_UPR_THRESHOLD_FAMILY_2003;
    M2003.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2003;
    M2003.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2003;
    M2003.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2003;
    M2003.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2003;
    M2003.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2003;
    M2003.taper = ML_TAPER_2003;
    M2003.rate = ML_RATE_2003;
    M2003.has_sapto_thr = 1;
    M2003.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2003, ORD_TAX_RATES_2003);
        apply_sapto(taxi, P, Sapto2003);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2003, LITO_1ST_THRESH_2003, LITO_1ST_TAPER_2003);
        
        // Medicare levy
        double ml = do_1_ML(P, M2003);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2003
  } // outer case 2003
    break;
    
  case 2004: {
    // Create a Sapto struct for this year
    Sapto Sapto2004;
    Sapto2004.year = 2004;
    Sapto2004.pension_age = 65;
    Sapto2004.mxo_single = SAPTO_MAX_SINGLE_2004;
    Sapto2004.mxo_couple = SAPTO_MAX_MARRIED_2004;
    Sapto2004.lwr_single = SAPTO_LWR_SINGLE_2004;
    Sapto2004.lwr_couple = SAPTO_LWR_MARRIED_2004;
    Sapto2004.upr_single = SAPTO_LWR_SINGLE_2004 + SAPTO_MAX_SINGLE_2004 / SAPTO_TAPER_2004;
    Sapto2004.upr_couple = SAPTO_LWR_MARRIED_2004 + SAPTO_MAX_MARRIED_2004 / SAPTO_TAPER_2004;
    Sapto2004.taper = SAPTO_TAPER_2004;
    Sapto2004.first_tax_rate = ORD_TAX_RATES_2004[1];
    Sapto2004.tax_free_thresh = ORD_TAX_BRACK_2004[1];
    Sapto2004.lito_max_offset = LITO_MAX_OFFSET_2004;
    
    Medicare M2004;
    M2004.lwr_single = ML_LWR_THRESHOLD_SINGLE_2004;
    M2004.upr_single = ML_UPR_THRESHOLD_SINGLE_2004;
    M2004.lwr_family = ML_LWR_THRESHOLD_FAMILY_2004;
    M2004.upr_family = ML_UPR_THRESHOLD_FAMILY_2004;
    M2004.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2004;
    M2004.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2004;
    M2004.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2004;
    M2004.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2004;
    M2004.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2004;
    M2004.taper = ML_TAPER_2004;
    M2004.rate = ML_RATE_2004;
    M2004.has_sapto_thr = 1;
    M2004.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2004, ORD_TAX_RATES_2004);
        apply_sapto(taxi, P, Sapto2004);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2004, LITO_1ST_THRESH_2004, LITO_1ST_TAPER_2004);
        
        // Medicare levy
        double ml = do_1_ML(P, M2004);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2004
  } // outer case 2004
    break;
    
  case 2005: {
    // Create a Sapto struct for this year
    Sapto Sapto2005;
    Sapto2005.year = 2005;
    Sapto2005.pension_age = 65;
    Sapto2005.mxo_single = SAPTO_MAX_SINGLE_2005;
    Sapto2005.mxo_couple = SAPTO_MAX_MARRIED_2005;
    Sapto2005.lwr_single = SAPTO_LWR_SINGLE_2005;
    Sapto2005.lwr_couple = SAPTO_LWR_MARRIED_2005;
    Sapto2005.upr_single = SAPTO_LWR_SINGLE_2005 + SAPTO_MAX_SINGLE_2005 / SAPTO_TAPER_2005;
    Sapto2005.upr_couple = SAPTO_LWR_MARRIED_2005 + SAPTO_MAX_MARRIED_2005 / SAPTO_TAPER_2005;
    Sapto2005.taper = SAPTO_TAPER_2005;
    Sapto2005.first_tax_rate = ORD_TAX_RATES_2005[1];
    Sapto2005.tax_free_thresh = ORD_TAX_BRACK_2005[1];
    Sapto2005.lito_max_offset = LITO_MAX_OFFSET_2005;
    
    Medicare M2005;
    M2005.lwr_single = ML_LWR_THRESHOLD_SINGLE_2005;
    M2005.upr_single = ML_UPR_THRESHOLD_SINGLE_2005;
    M2005.lwr_family = ML_LWR_THRESHOLD_FAMILY_2005;
    M2005.upr_family = ML_UPR_THRESHOLD_FAMILY_2005;
    M2005.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2005;
    M2005.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2005;
    M2005.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2005;
    M2005.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2005;
    M2005.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2005;
    M2005.taper = ML_TAPER_2005;
    M2005.rate = ML_RATE_2005;
    M2005.has_sapto_thr = 1;
    M2005.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2005, ORD_TAX_RATES_2005);
        apply_sapto(taxi, P, Sapto2005);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2005, LITO_1ST_THRESH_2005, LITO_1ST_TAPER_2005);
        
        // Medicare levy
        double ml = do_1_ML(P, M2005);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2005
  } // outer case 2005
    break;
    
  case 2006: {
    // Create a Sapto struct for this year
    Sapto Sapto2006;
    Sapto2006.year = 2006;
    Sapto2006.pension_age = 65;
    Sapto2006.mxo_single = SAPTO_MAX_SINGLE_2006;
    Sapto2006.mxo_couple = SAPTO_MAX_MARRIED_2006;
    Sapto2006.lwr_single = SAPTO_LWR_SINGLE_2006;
    Sapto2006.lwr_couple = SAPTO_LWR_MARRIED_2006;
    Sapto2006.upr_single = SAPTO_LWR_SINGLE_2006 + SAPTO_MAX_SINGLE_2006 / SAPTO_TAPER_2006;
    Sapto2006.upr_couple = SAPTO_LWR_MARRIED_2006 + SAPTO_MAX_MARRIED_2006 / SAPTO_TAPER_2006;
    Sapto2006.taper = SAPTO_TAPER_2006;
    Sapto2006.first_tax_rate = ORD_TAX_RATES_2006[1];
    Sapto2006.tax_free_thresh = ORD_TAX_BRACK_2006[1];
    Sapto2006.lito_max_offset = LITO_MAX_OFFSET_2006;
    
    Medicare M2006;
    M2006.lwr_single = ML_LWR_THRESHOLD_SINGLE_2006;
    M2006.upr_single = ML_UPR_THRESHOLD_SINGLE_2006;
    M2006.lwr_family = ML_LWR_THRESHOLD_FAMILY_2006;
    M2006.upr_family = ML_UPR_THRESHOLD_FAMILY_2006;
    M2006.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2006;
    M2006.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2006;
    M2006.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2006;
    M2006.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2006;
    M2006.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2006;
    M2006.taper = ML_TAPER_2006;
    M2006.rate = ML_RATE_2006;
    M2006.has_sapto_thr = 1;
    M2006.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2006, ORD_TAX_RATES_2006);
        apply_sapto(taxi, P, Sapto2006);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2006, LITO_1ST_THRESH_2006, LITO_1ST_TAPER_2006);
        
        // Medicare levy
        double ml = do_1_ML(P, M2006);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2006
  } // outer case 2006
    break;
    
  case 2007: {
    // Create a Sapto struct for this year
    Sapto Sapto2007;
    Sapto2007.year = 2007;
    Sapto2007.pension_age = 65;
    Sapto2007.mxo_single = SAPTO_MAX_SINGLE_2007;
    Sapto2007.mxo_couple = SAPTO_MAX_MARRIED_2007;
    Sapto2007.lwr_single = SAPTO_LWR_SINGLE_2007;
    Sapto2007.lwr_couple = SAPTO_LWR_MARRIED_2007;
    Sapto2007.upr_single = SAPTO_LWR_SINGLE_2007 + SAPTO_MAX_SINGLE_2007 / SAPTO_TAPER_2007;
    Sapto2007.upr_couple = SAPTO_LWR_MARRIED_2007 + SAPTO_MAX_MARRIED_2007 / SAPTO_TAPER_2007;
    Sapto2007.taper = SAPTO_TAPER_2007;
    Sapto2007.first_tax_rate = ORD_TAX_RATES_2007[1];
    Sapto2007.tax_free_thresh = ORD_TAX_BRACK_2007[1];
    Sapto2007.lito_max_offset = LITO_MAX_OFFSET_2007;
    
    Medicare M2007;
    M2007.lwr_single = ML_LWR_THRESHOLD_SINGLE_2007;
    M2007.upr_single = ML_UPR_THRESHOLD_SINGLE_2007;
    M2007.lwr_family = ML_LWR_THRESHOLD_FAMILY_2007;
    M2007.upr_family = ML_UPR_THRESHOLD_FAMILY_2007;
    M2007.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2007;
    M2007.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2007;
    M2007.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2007;
    M2007.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2007;
    M2007.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2007;
    M2007.taper = ML_TAPER_2007;
    M2007.rate = ML_RATE_2007;
    M2007.has_sapto_thr = 1;
    M2007.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2007, ORD_TAX_RATES_2007);
        apply_sapto(taxi, P, Sapto2007);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2007, LITO_1ST_THRESH_2007, LITO_1ST_TAPER_2007);
        
        // Medicare levy
        double ml = do_1_ML(P, M2007);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2007
  } // outer case 2007
    break;
    
  case 2008: {
    // Create a Sapto struct for this year
    Sapto Sapto2008;
    Sapto2008.year = 2008;
    Sapto2008.pension_age = 65;
    Sapto2008.mxo_single = SAPTO_MAX_SINGLE_2008;
    Sapto2008.mxo_couple = SAPTO_MAX_MARRIED_2008;
    Sapto2008.lwr_single = SAPTO_LWR_SINGLE_2008;
    Sapto2008.lwr_couple = SAPTO_LWR_MARRIED_2008;
    Sapto2008.upr_single = SAPTO_LWR_SINGLE_2008 + SAPTO_MAX_SINGLE_2008 / SAPTO_TAPER_2008;
    Sapto2008.upr_couple = SAPTO_LWR_MARRIED_2008 + SAPTO_MAX_MARRIED_2008 / SAPTO_TAPER_2008;
    Sapto2008.taper = SAPTO_TAPER_2008;
    Sapto2008.first_tax_rate = ORD_TAX_RATES_2008[1];
    Sapto2008.tax_free_thresh = ORD_TAX_BRACK_2008[1];
    Sapto2008.lito_max_offset = LITO_MAX_OFFSET_2008;
    
    Medicare M2008;
    M2008.lwr_single = ML_LWR_THRESHOLD_SINGLE_2008;
    M2008.upr_single = ML_UPR_THRESHOLD_SINGLE_2008;
    M2008.lwr_family = ML_LWR_THRESHOLD_FAMILY_2008;
    M2008.upr_family = ML_UPR_THRESHOLD_FAMILY_2008;
    M2008.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2008;
    M2008.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2008;
    M2008.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2008;
    M2008.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2008;
    M2008.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2008;
    M2008.taper = ML_TAPER_2008;
    M2008.rate = ML_RATE_2008;
    M2008.has_sapto_thr = 1;
    M2008.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2008, ORD_TAX_RATES_2008);
        apply_sapto(taxi, P, Sapto2008);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2008, LITO_1ST_THRESH_2008, LITO_1ST_TAPER_2008);
        
        // Medicare levy
        double ml = do_1_ML(P, M2008);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2008
  } // outer case 2008
    break;
    
  case 2009: {
    // Create a Sapto struct for this year
    Sapto Sapto2009;
    Sapto2009.year = 2009;
    Sapto2009.pension_age = 65;
    Sapto2009.mxo_single = SAPTO_MAX_SINGLE_2009;
    Sapto2009.mxo_couple = SAPTO_MAX_MARRIED_2009;
    Sapto2009.lwr_single = SAPTO_LWR_SINGLE_2009;
    Sapto2009.lwr_couple = SAPTO_LWR_MARRIED_2009;
    Sapto2009.upr_single = SAPTO_LWR_SINGLE_2009 + SAPTO_MAX_SINGLE_2009 / SAPTO_TAPER_2009;
    Sapto2009.upr_couple = SAPTO_LWR_MARRIED_2009 + SAPTO_MAX_MARRIED_2009 / SAPTO_TAPER_2009;
    Sapto2009.taper = SAPTO_TAPER_2009;
    Sapto2009.first_tax_rate = ORD_TAX_RATES_2009[1];
    Sapto2009.tax_free_thresh = ORD_TAX_BRACK_2009[1];
    Sapto2009.lito_max_offset = LITO_MAX_OFFSET_2009;
    
    Medicare M2009;
    M2009.lwr_single = ML_LWR_THRESHOLD_SINGLE_2009;
    M2009.upr_single = ML_UPR_THRESHOLD_SINGLE_2009;
    M2009.lwr_family = ML_LWR_THRESHOLD_FAMILY_2009;
    M2009.upr_family = ML_UPR_THRESHOLD_FAMILY_2009;
    M2009.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2009;
    M2009.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2009;
    M2009.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2009;
    M2009.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2009;
    M2009.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2009;
    M2009.taper = ML_TAPER_2009;
    M2009.rate = ML_RATE_2009;
    M2009.has_sapto_thr = 1;
    M2009.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2009, ORD_TAX_RATES_2009);
        apply_sapto(taxi, P, Sapto2009);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2009, LITO_1ST_THRESH_2009, LITO_1ST_TAPER_2009);
        
        // Medicare levy
        double ml = do_1_ML(P, M2009);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2009
  } // outer case 2009
    break;
    
  case 2010: {
    // Create a Sapto struct for this year
    Sapto Sapto2010;
    Sapto2010.year = 2010;
    Sapto2010.pension_age = 65;
    Sapto2010.mxo_single = SAPTO_MAX_SINGLE_2010;
    Sapto2010.mxo_couple = SAPTO_MAX_MARRIED_2010;
    Sapto2010.lwr_single = SAPTO_LWR_SINGLE_2010;
    Sapto2010.lwr_couple = SAPTO_LWR_MARRIED_2010;
    Sapto2010.upr_single = SAPTO_LWR_SINGLE_2010 + SAPTO_MAX_SINGLE_2010 / SAPTO_TAPER_2010;
    Sapto2010.upr_couple = SAPTO_LWR_MARRIED_2010 + SAPTO_MAX_MARRIED_2010 / SAPTO_TAPER_2010;
    Sapto2010.taper = SAPTO_TAPER_2010;
    Sapto2010.first_tax_rate = ORD_TAX_RATES_2010[1];
    Sapto2010.tax_free_thresh = ORD_TAX_BRACK_2010[1];
    Sapto2010.lito_max_offset = LITO_MAX_OFFSET_2010;
    
    Medicare M2010;
    M2010.lwr_single = ML_LWR_THRESHOLD_SINGLE_2010;
    M2010.upr_single = ML_UPR_THRESHOLD_SINGLE_2010;
    M2010.lwr_family = ML_LWR_THRESHOLD_FAMILY_2010;
    M2010.upr_family = ML_UPR_THRESHOLD_FAMILY_2010;
    M2010.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2010;
    M2010.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2010;
    M2010.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2010;
    M2010.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2010;
    M2010.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2010;
    M2010.taper = ML_TAPER_2010;
    M2010.rate = ML_RATE_2010;
    M2010.has_sapto_thr = 1;
    M2010.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2010, ORD_TAX_RATES_2010);
        apply_sapto(taxi, P, Sapto2010);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2010, LITO_1ST_THRESH_2010, LITO_1ST_TAPER_2010);
        
        // Medicare levy
        double ml = do_1_ML(P, M2010);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2010
  } // outer case 2010
    break;
    
  case 2011: {
    // Create a Sapto struct for this year
    Sapto Sapto2011;
    Sapto2011.year = 2011;
    Sapto2011.pension_age = 65;
    Sapto2011.mxo_single = SAPTO_MAX_SINGLE_2011;
    Sapto2011.mxo_couple = SAPTO_MAX_MARRIED_2011;
    Sapto2011.lwr_single = SAPTO_LWR_SINGLE_2011;
    Sapto2011.lwr_couple = SAPTO_LWR_MARRIED_2011;
    Sapto2011.upr_single = SAPTO_LWR_SINGLE_2011 + SAPTO_MAX_SINGLE_2011 / SAPTO_TAPER_2011;
    Sapto2011.upr_couple = SAPTO_LWR_MARRIED_2011 + SAPTO_MAX_MARRIED_2011 / SAPTO_TAPER_2011;
    Sapto2011.taper = SAPTO_TAPER_2011;
    Sapto2011.first_tax_rate = ORD_TAX_RATES_2011[1];
    Sapto2011.tax_free_thresh = ORD_TAX_BRACK_2011[1];
    Sapto2011.lito_max_offset = LITO_MAX_OFFSET_2011;
    
    Medicare M2011;
    M2011.lwr_single = ML_LWR_THRESHOLD_SINGLE_2011;
    M2011.upr_single = ML_UPR_THRESHOLD_SINGLE_2011;
    M2011.lwr_family = ML_LWR_THRESHOLD_FAMILY_2011;
    M2011.upr_family = ML_UPR_THRESHOLD_FAMILY_2011;
    M2011.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2011;
    M2011.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2011;
    M2011.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2011;
    M2011.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2011;
    M2011.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2011;
    M2011.taper = ML_TAPER_2011;
    M2011.rate = ML_RATE_2011;
    M2011.has_sapto_thr = 1;
    M2011.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2011, ORD_TAX_RATES_2011);
        apply_sapto(taxi, P, Sapto2011);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2011, LITO_1ST_THRESH_2011, LITO_1ST_TAPER_2011);
        
        // Medicare levy
        double ml = do_1_ML(P, M2011);
        taxi += ml;
        
        // Budget levies
        // flood levy
        taxi += FLOOD_LEVY_TAPER_2011 * (max0(xi - FLOOD_LEVY_1ST_THRESH_2011) * max0(xi - FLOOD_LEVY_2ND_THRESH_2011));
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2011
  } // outer case 2011
    break;
    
  case 2012: {
    // Create a Sapto struct for this year
    Sapto Sapto2012;
    Sapto2012.year = 2012;
    Sapto2012.pension_age = 65;
    Sapto2012.mxo_single = SAPTO_MAX_SINGLE_2012;
    Sapto2012.mxo_couple = SAPTO_MAX_MARRIED_2012;
    Sapto2012.lwr_single = SAPTO_LWR_SINGLE_2012;
    Sapto2012.lwr_couple = SAPTO_LWR_MARRIED_2012;
    Sapto2012.upr_single = SAPTO_LWR_SINGLE_2012 + SAPTO_MAX_SINGLE_2012 / SAPTO_TAPER_2012;
    Sapto2012.upr_couple = SAPTO_LWR_MARRIED_2012 + SAPTO_MAX_MARRIED_2012 / SAPTO_TAPER_2012;
    Sapto2012.taper = SAPTO_TAPER_2012;
    Sapto2012.first_tax_rate = ORD_TAX_RATES_2012[1];
    Sapto2012.tax_free_thresh = ORD_TAX_BRACK_2012[1];
    Sapto2012.lito_max_offset = LITO_MAX_OFFSET_2012;
    
    Medicare M2012;
    M2012.lwr_single = ML_LWR_THRESHOLD_SINGLE_2012;
    M2012.upr_single = ML_UPR_THRESHOLD_SINGLE_2012;
    M2012.lwr_family = ML_LWR_THRESHOLD_FAMILY_2012;
    M2012.upr_family = ML_UPR_THRESHOLD_FAMILY_2012;
    M2012.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2012;
    M2012.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2012;
    M2012.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2012;
    M2012.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2012;
    M2012.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2012;
    M2012.taper = ML_TAPER_2012;
    M2012.rate = ML_RATE_2012;
    M2012.has_sapto_thr = 1;
    M2012.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2012, ORD_TAX_RATES_2012);
        apply_sapto(taxi, P, Sapto2012);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2012, LITO_1ST_THRESH_2012, LITO_1ST_TAPER_2012);
        
        // Medicare levy
        double ml = do_1_ML(P, M2012);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2012
  } // outer case 2012
    break;
    
  case 2013: {
    // Create a Sapto struct for this year
    Sapto Sapto2013;
    Sapto2013.year = 2013;
    Sapto2013.pension_age = 65;
    Sapto2013.mxo_single = SAPTO_MAX_SINGLE_2013;
    Sapto2013.mxo_couple = SAPTO_MAX_MARRIED_2013;
    Sapto2013.lwr_single = SAPTO_LWR_SINGLE_2013;
    Sapto2013.lwr_couple = SAPTO_LWR_MARRIED_2013;
    Sapto2013.upr_single = SAPTO_LWR_SINGLE_2013 + SAPTO_MAX_SINGLE_2013 / SAPTO_TAPER_2013;
    Sapto2013.upr_couple = SAPTO_LWR_MARRIED_2013 + SAPTO_MAX_MARRIED_2013 / SAPTO_TAPER_2013;
    Sapto2013.taper = SAPTO_TAPER_2013;
    Sapto2013.first_tax_rate = ORD_TAX_RATES_2013[1];
    Sapto2013.tax_free_thresh = ORD_TAX_BRACK_2013[1];
    Sapto2013.lito_max_offset = LITO_MAX_OFFSET_2013;
    
    Medicare M2013;
    M2013.lwr_single = ML_LWR_THRESHOLD_SINGLE_2013;
    M2013.upr_single = ML_UPR_THRESHOLD_SINGLE_2013;
    M2013.lwr_family = ML_LWR_THRESHOLD_FAMILY_2013;
    M2013.upr_family = ML_UPR_THRESHOLD_FAMILY_2013;
    M2013.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2013;
    M2013.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2013;
    M2013.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2013;
    M2013.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2013;
    M2013.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2013;
    M2013.taper = ML_TAPER_2013;
    M2013.rate = ML_RATE_2013;
    M2013.has_sapto_thr = 1;
    M2013.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2013, ORD_TAX_RATES_2013);
        apply_sapto(taxi, P, Sapto2013);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2013, LITO_1ST_THRESH_2013, LITO_1ST_TAPER_2013);
        
        // Medicare levy
        double ml = do_1_ML(P, M2013);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2013
  } // outer case 2013
    break;
    
  case 2014: {
    // Create a Sapto struct for this year
    Sapto Sapto2014;
    Sapto2014.year = 2014;
    Sapto2014.pension_age = 65;
    Sapto2014.mxo_single = SAPTO_MAX_SINGLE_2014;
    Sapto2014.mxo_couple = SAPTO_MAX_MARRIED_2014;
    Sapto2014.lwr_single = SAPTO_LWR_SINGLE_2014;
    Sapto2014.lwr_couple = SAPTO_LWR_MARRIED_2014;
    Sapto2014.upr_single = SAPTO_LWR_SINGLE_2014 + SAPTO_MAX_SINGLE_2014 / SAPTO_TAPER_2014;
    Sapto2014.upr_couple = SAPTO_LWR_MARRIED_2014 + SAPTO_MAX_MARRIED_2014 / SAPTO_TAPER_2014;
    Sapto2014.taper = SAPTO_TAPER_2014;
    Sapto2014.first_tax_rate = ORD_TAX_RATES_2014[1];
    Sapto2014.tax_free_thresh = ORD_TAX_BRACK_2014[1];
    Sapto2014.lito_max_offset = LITO_MAX_OFFSET_2014;
    
    Medicare M2014;
    M2014.lwr_single = ML_LWR_THRESHOLD_SINGLE_2014;
    M2014.upr_single = ML_UPR_THRESHOLD_SINGLE_2014;
    M2014.lwr_family = ML_LWR_THRESHOLD_FAMILY_2014;
    M2014.upr_family = ML_UPR_THRESHOLD_FAMILY_2014;
    M2014.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2014;
    M2014.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2014;
    M2014.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2014;
    M2014.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2014;
    M2014.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2014;
    M2014.taper = ML_TAPER_2014;
    M2014.rate = ML_RATE_2014;
    M2014.has_sapto_thr = 1;
    M2014.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2014, ORD_TAX_RATES_2014);
        apply_sapto(taxi, P, Sapto2014);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2014, LITO_1ST_THRESH_2014, LITO_1ST_TAPER_2014);
        
        // Medicare levy
        double ml = do_1_ML(P, M2014);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2014
  } // outer case 2014
    break;
    
  case 2015: {
    // Create a Sapto struct for this year
    Sapto Sapto2015;
    Sapto2015.year = 2015;
    Sapto2015.pension_age = 65;
    Sapto2015.mxo_single = SAPTO_MAX_SINGLE_2015;
    Sapto2015.mxo_couple = SAPTO_MAX_MARRIED_2015;
    Sapto2015.lwr_single = SAPTO_LWR_SINGLE_2015;
    Sapto2015.lwr_couple = SAPTO_LWR_MARRIED_2015;
    Sapto2015.upr_single = SAPTO_LWR_SINGLE_2015 + SAPTO_MAX_SINGLE_2015 / SAPTO_TAPER_2015;
    Sapto2015.upr_couple = SAPTO_LWR_MARRIED_2015 + SAPTO_MAX_MARRIED_2015 / SAPTO_TAPER_2015;
    Sapto2015.taper = SAPTO_TAPER_2015;
    Sapto2015.first_tax_rate = ORD_TAX_RATES_2015[1];
    Sapto2015.tax_free_thresh = ORD_TAX_BRACK_2015[1];
    Sapto2015.lito_max_offset = LITO_MAX_OFFSET_2015;
    
    Medicare M2015;
    M2015.lwr_single = ML_LWR_THRESHOLD_SINGLE_2015;
    M2015.upr_single = ML_UPR_THRESHOLD_SINGLE_2015;
    M2015.lwr_family = ML_LWR_THRESHOLD_FAMILY_2015;
    M2015.upr_family = ML_UPR_THRESHOLD_FAMILY_2015;
    M2015.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2015;
    M2015.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2015;
    M2015.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2015;
    M2015.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2015;
    M2015.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2015;
    M2015.taper = ML_TAPER_2015;
    M2015.rate = ML_RATE_2015;
    M2015.has_sapto_thr = 1;
    M2015.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2015, ORD_TAX_RATES_2015);
        apply_sapto(taxi, P, Sapto2015);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2015, LITO_1ST_THRESH_2015, LITO_1ST_TAPER_2015);
        
        // Medicare levy
        double ml = do_1_ML(P, M2015);
        taxi += ml;
        
        // Budget levies
        // temporary budget repair levy
        taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xi - TEMP_BUDGET_REPAIR_LEVY_THRESH);
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2015
  } // outer case 2015
    break;
    
  case 2016: {
    // Create a Sapto struct for this year
    Sapto Sapto2016;
    Sapto2016.year = 2016;
    Sapto2016.pension_age = 65;
    Sapto2016.mxo_single = SAPTO_MAX_SINGLE_2016;
    Sapto2016.mxo_couple = SAPTO_MAX_MARRIED_2016;
    Sapto2016.lwr_single = SAPTO_LWR_SINGLE_2016;
    Sapto2016.lwr_couple = SAPTO_LWR_MARRIED_2016;
    Sapto2016.upr_single = SAPTO_LWR_SINGLE_2016 + SAPTO_MAX_SINGLE_2016 / SAPTO_TAPER_2016;
    Sapto2016.upr_couple = SAPTO_LWR_MARRIED_2016 + SAPTO_MAX_MARRIED_2016 / SAPTO_TAPER_2016;
    Sapto2016.taper = SAPTO_TAPER_2016;
    Sapto2016.first_tax_rate = ORD_TAX_RATES_2016[1];
    Sapto2016.tax_free_thresh = ORD_TAX_BRACK_2016[1];
    Sapto2016.lito_max_offset = LITO_MAX_OFFSET_2016;
    
    Medicare M2016;
    M2016.lwr_single = ML_LWR_THRESHOLD_SINGLE_2016;
    M2016.upr_single = ML_UPR_THRESHOLD_SINGLE_2016;
    M2016.lwr_family = ML_LWR_THRESHOLD_FAMILY_2016;
    M2016.upr_family = ML_UPR_THRESHOLD_FAMILY_2016;
    M2016.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2016;
    M2016.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2016;
    M2016.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2016;
    M2016.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2016;
    M2016.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2016;
    M2016.taper = ML_TAPER_2016;
    M2016.rate = ML_RATE_2016;
    M2016.has_sapto_thr = 1;
    M2016.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2016, ORD_TAX_RATES_2016);
        apply_sapto(taxi, P, Sapto2016);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2016, LITO_1ST_THRESH_2016, LITO_1ST_TAPER_2016);
        
        // Medicare levy
        double ml = do_1_ML(P, M2016);
        taxi += ml;
        
        // Budget levies
        // temporary budget repair levy
        taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xi - TEMP_BUDGET_REPAIR_LEVY_THRESH);
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2016
  } // outer case 2016
    break;
    
  case 2017: {
    // Create a Sapto struct for this year
    Sapto Sapto2017;
    Sapto2017.year = 2017;
    Sapto2017.pension_age = 65;
    Sapto2017.mxo_single = SAPTO_MAX_SINGLE_2017;
    Sapto2017.mxo_couple = SAPTO_MAX_MARRIED_2017;
    Sapto2017.lwr_single = SAPTO_LWR_SINGLE_2017;
    Sapto2017.lwr_couple = SAPTO_LWR_MARRIED_2017;
    Sapto2017.upr_single = SAPTO_LWR_SINGLE_2017 + SAPTO_MAX_SINGLE_2017 / SAPTO_TAPER_2017;
    Sapto2017.upr_couple = SAPTO_LWR_MARRIED_2017 + SAPTO_MAX_MARRIED_2017 / SAPTO_TAPER_2017;
    Sapto2017.taper = SAPTO_TAPER_2017;
    Sapto2017.first_tax_rate = ORD_TAX_RATES_2017[1];
    Sapto2017.tax_free_thresh = ORD_TAX_BRACK_2017[1];
    Sapto2017.lito_max_offset = LITO_MAX_OFFSET_2017;
    
    Medicare M2017;
    M2017.lwr_single = ML_LWR_THRESHOLD_SINGLE_2017;
    M2017.upr_single = ML_UPR_THRESHOLD_SINGLE_2017;
    M2017.lwr_family = ML_LWR_THRESHOLD_FAMILY_2017;
    M2017.upr_family = ML_UPR_THRESHOLD_FAMILY_2017;
    M2017.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2017;
    M2017.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2017;
    M2017.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2017;
    M2017.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2017;
    M2017.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2017;
    M2017.taper = ML_TAPER_2017;
    M2017.rate = ML_RATE_2017;
    M2017.has_sapto_thr = 1;
    M2017.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2017, ORD_TAX_RATES_2017);
        apply_sapto(taxi, P, Sapto2017);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2017, LITO_1ST_THRESH_2017, LITO_1ST_TAPER_2017);
        
        // Medicare levy
        double ml = do_1_ML(P, M2017);
        taxi += ml;
        
        // Budget levies
        // temporary budget repair levy
        taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xi - TEMP_BUDGET_REPAIR_LEVY_THRESH);
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2017
  } // outer case 2017
    break;
    
  case 2018: {
    // Create a Sapto struct for this year
    Sapto Sapto2018;
    Sapto2018.year = 2018;
    Sapto2018.pension_age = 65;
    Sapto2018.mxo_single = SAPTO_MAX_SINGLE_2018;
    Sapto2018.mxo_couple = SAPTO_MAX_MARRIED_2018;
    Sapto2018.lwr_single = SAPTO_LWR_SINGLE_2018;
    Sapto2018.lwr_couple = SAPTO_LWR_MARRIED_2018;
    Sapto2018.upr_single = SAPTO_LWR_SINGLE_2018 + SAPTO_MAX_SINGLE_2018 / SAPTO_TAPER_2018;
    Sapto2018.upr_couple = SAPTO_LWR_MARRIED_2018 + SAPTO_MAX_MARRIED_2018 / SAPTO_TAPER_2018;
    Sapto2018.taper = SAPTO_TAPER_2018;
    Sapto2018.first_tax_rate = ORD_TAX_RATES_2018[1];
    Sapto2018.tax_free_thresh = ORD_TAX_BRACK_2018[1];
    Sapto2018.lito_max_offset = LITO_MAX_OFFSET_2018;
    
    Medicare M2018;
    M2018.lwr_single = ML_LWR_THRESHOLD_SINGLE_2018;
    M2018.upr_single = ML_UPR_THRESHOLD_SINGLE_2018;
    M2018.lwr_family = ML_LWR_THRESHOLD_FAMILY_2018;
    M2018.upr_family = ML_UPR_THRESHOLD_FAMILY_2018;
    M2018.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2018;
    M2018.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2018;
    M2018.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2018;
    M2018.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2018;
    M2018.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2018;
    M2018.taper = ML_TAPER_2018;
    M2018.rate = ML_RATE_2018;
    M2018.has_sapto_thr = 1;
    M2018.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2018, ORD_TAX_RATES_2018);
        apply_sapto(taxi, P, Sapto2018);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2018, LITO_1ST_THRESH_2018, LITO_1ST_TAPER_2018);
        
        // Medicare levy
        double ml = do_1_ML(P, M2018);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2018
  } // outer case 2018
    break;
    
  case 2019: {
    // Create a Sapto struct for this year
    Sapto Sapto2019;
    Sapto2019.year = 2019;
    Sapto2019.pension_age = 65;
    Sapto2019.mxo_single = SAPTO_MAX_SINGLE_2019;
    Sapto2019.mxo_couple = SAPTO_MAX_MARRIED_2019;
    Sapto2019.lwr_single = SAPTO_LWR_SINGLE_2019;
    Sapto2019.lwr_couple = SAPTO_LWR_MARRIED_2019;
    Sapto2019.upr_single = SAPTO_LWR_SINGLE_2019 + SAPTO_MAX_SINGLE_2019 / SAPTO_TAPER_2019;
    Sapto2019.upr_couple = SAPTO_LWR_MARRIED_2019 + SAPTO_MAX_MARRIED_2019 / SAPTO_TAPER_2019;
    Sapto2019.taper = SAPTO_TAPER_2019;
    Sapto2019.first_tax_rate = ORD_TAX_RATES_2019[1];
    Sapto2019.tax_free_thresh = ORD_TAX_BRACK_2019[1];
    Sapto2019.lito_max_offset = LITO_MAX_OFFSET_2019;
    
    Medicare M2019;
    M2019.lwr_single = ML_LWR_THRESHOLD_SINGLE_2019;
    M2019.upr_single = ML_UPR_THRESHOLD_SINGLE_2019;
    M2019.lwr_family = ML_LWR_THRESHOLD_FAMILY_2019;
    M2019.upr_family = ML_UPR_THRESHOLD_FAMILY_2019;
    M2019.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2019;
    M2019.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2019;
    M2019.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2019;
    M2019.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2019;
    M2019.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2019;
    M2019.taper = ML_TAPER_2019;
    M2019.rate = ML_RATE_2019;
    M2019.has_sapto_thr = 1;
    M2019.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2019, ORD_TAX_RATES_2019);
        apply_sapto(taxi, P, Sapto2019);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2019, LITO_1ST_THRESH_2019, LITO_1ST_TAPER_2019);
        apply_lmito(taxi, xi);
        
        // Medicare levy
        double ml = do_1_ML(P, M2019);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2019
  } // outer case 2019
    break;
    
  case 2020: {
    // Create a Sapto struct for this year
    Sapto Sapto2020;
    Sapto2020.year = 2020;
    Sapto2020.pension_age = 65;
    Sapto2020.mxo_single = SAPTO_MAX_SINGLE_2020;
    Sapto2020.mxo_couple = SAPTO_MAX_MARRIED_2020;
    Sapto2020.lwr_single = SAPTO_LWR_SINGLE_2020;
    Sapto2020.lwr_couple = SAPTO_LWR_MARRIED_2020;
    Sapto2020.upr_single = SAPTO_LWR_SINGLE_2020 + SAPTO_MAX_SINGLE_2020 / SAPTO_TAPER_2020;
    Sapto2020.upr_couple = SAPTO_LWR_MARRIED_2020 + SAPTO_MAX_MARRIED_2020 / SAPTO_TAPER_2020;
    Sapto2020.taper = SAPTO_TAPER_2020;
    Sapto2020.first_tax_rate = ORD_TAX_RATES_2020[1];
    Sapto2020.tax_free_thresh = ORD_TAX_BRACK_2020[1];
    Sapto2020.lito_max_offset = LITO_MAX_OFFSET_2020;
    
    Medicare M2020;
    M2020.lwr_single = ML_LWR_THRESHOLD_SINGLE_2020;
    M2020.upr_single = ML_UPR_THRESHOLD_SINGLE_2020;
    M2020.lwr_family = ML_LWR_THRESHOLD_FAMILY_2020;
    M2020.upr_family = ML_UPR_THRESHOLD_FAMILY_2020;
    M2020.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2020;
    M2020.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2020;
    M2020.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2020;
    M2020.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2020;
    M2020.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2020;
    M2020.taper = ML_TAPER_2020;
    M2020.rate = ML_RATE_2020;
    M2020.has_sapto_thr = 1;
    M2020.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2020, ORD_TAX_RATES_2020);
        apply_sapto(taxi, P, Sapto2020);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2020, LITO_1ST_THRESH_2020, LITO_1ST_TAPER_2020);
        apply_lmito(taxi, xi);
        
        // Medicare levy
        double ml = do_1_ML(P, M2020);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2020
  } // outer case 2020
    break;
    
  case 2021: {
    // Create a Sapto struct for this year
    Sapto Sapto2021;
    Sapto2021.year = 2021;
    Sapto2021.pension_age = 65;
    Sapto2021.mxo_single = SAPTO_MAX_SINGLE_2021;
    Sapto2021.mxo_couple = SAPTO_MAX_MARRIED_2021;
    Sapto2021.lwr_single = SAPTO_LWR_SINGLE_2021;
    Sapto2021.lwr_couple = SAPTO_LWR_MARRIED_2021;
    Sapto2021.upr_single = SAPTO_LWR_SINGLE_2021 + SAPTO_MAX_SINGLE_2021 / SAPTO_TAPER_2021;
    Sapto2021.upr_couple = SAPTO_LWR_MARRIED_2021 + SAPTO_MAX_MARRIED_2021 / SAPTO_TAPER_2021;
    Sapto2021.taper = SAPTO_TAPER_2021;
    Sapto2021.first_tax_rate = ORD_TAX_RATES_2021[1];
    Sapto2021.tax_free_thresh = ORD_TAX_BRACK_2021[1];
    Sapto2021.lito_max_offset = LITO_MAX_OFFSET_2021;
    
    Medicare M2021;
    M2021.lwr_single = ML_LWR_THRESHOLD_SINGLE_2021;
    M2021.upr_single = ML_UPR_THRESHOLD_SINGLE_2021;
    M2021.lwr_family = ML_LWR_THRESHOLD_FAMILY_2021;
    M2021.upr_family = ML_UPR_THRESHOLD_FAMILY_2021;
    M2021.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2021;
    M2021.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2021;
    M2021.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2021;
    M2021.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2021;
    M2021.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2021;
    M2021.taper = ML_TAPER_2021;
    M2021.rate = ML_RATE_2021;
    M2021.has_sapto_thr = 1;
    M2021.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2021, ORD_TAX_RATES_2021);
        apply_sapto(taxi, P, Sapto2021);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2021, LITO_1ST_THRESH_2021, LITO_1ST_TAPER_2021);
        apply_lmito(taxi, xi);
        
        // Medicare levy
        double ml = do_1_ML(P, M2021);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2021
  } // outer case 2021
    break;
    
  case 2022: {
    // Create a Sapto struct for this year
    Sapto Sapto2022;
    Sapto2022.year = 2022;
    Sapto2022.pension_age = 65;
    Sapto2022.mxo_single = SAPTO_MAX_SINGLE_2022;
    Sapto2022.mxo_couple = SAPTO_MAX_MARRIED_2022;
    Sapto2022.lwr_single = SAPTO_LWR_SINGLE_2022;
    Sapto2022.lwr_couple = SAPTO_LWR_MARRIED_2022;
    Sapto2022.upr_single = SAPTO_LWR_SINGLE_2022 + SAPTO_MAX_SINGLE_2022 / SAPTO_TAPER_2022;
    Sapto2022.upr_couple = SAPTO_LWR_MARRIED_2022 + SAPTO_MAX_MARRIED_2022 / SAPTO_TAPER_2022;
    Sapto2022.taper = SAPTO_TAPER_2022;
    Sapto2022.first_tax_rate = ORD_TAX_RATES_2022[1];
    Sapto2022.tax_free_thresh = ORD_TAX_BRACK_2022[1];
    Sapto2022.lito_max_offset = LITO_MAX_OFFSET_2022;
    
    Medicare M2022;
    M2022.lwr_single = ML_LWR_THRESHOLD_SINGLE_2022;
    M2022.upr_single = ML_UPR_THRESHOLD_SINGLE_2022;
    M2022.lwr_family = ML_LWR_THRESHOLD_FAMILY_2022;
    M2022.upr_family = ML_UPR_THRESHOLD_FAMILY_2022;
    M2022.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2022;
    M2022.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2022;
    M2022.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2022;
    M2022.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2022;
    M2022.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2022;
    M2022.taper = ML_TAPER_2022;
    M2022.rate = ML_RATE_2022;
    M2022.has_sapto_thr = 1;
    M2022.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2022, ORD_TAX_RATES_2022);
        apply_sapto(taxi, P, Sapto2022);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2022, LITO_1ST_THRESH_2022, LITO_1ST_TAPER_2022);
        apply_lmito(taxi, xi);
        
        // Medicare levy
        double ml = do_1_ML(P, M2022);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2022
  } // outer case 2022
    break;
    
  case 2023: {
    // Create a Sapto struct for this year
    Sapto Sapto2023;
    Sapto2023.year = 2023;
    Sapto2023.pension_age = 65;
    Sapto2023.mxo_single = SAPTO_MAX_SINGLE_2023;
    Sapto2023.mxo_couple = SAPTO_MAX_MARRIED_2023;
    Sapto2023.lwr_single = SAPTO_LWR_SINGLE_2023;
    Sapto2023.lwr_couple = SAPTO_LWR_MARRIED_2023;
    Sapto2023.upr_single = SAPTO_LWR_SINGLE_2023 + SAPTO_MAX_SINGLE_2023 / SAPTO_TAPER_2023;
    Sapto2023.upr_couple = SAPTO_LWR_MARRIED_2023 + SAPTO_MAX_MARRIED_2023 / SAPTO_TAPER_2023;
    Sapto2023.taper = SAPTO_TAPER_2023;
    Sapto2023.first_tax_rate = ORD_TAX_RATES_2023[1];
    Sapto2023.tax_free_thresh = ORD_TAX_BRACK_2023[1];
    Sapto2023.lito_max_offset = LITO_MAX_OFFSET_2023;
    
    Medicare M2023;
    M2023.lwr_single = ML_LWR_THRESHOLD_SINGLE_2023;
    M2023.upr_single = ML_UPR_THRESHOLD_SINGLE_2023;
    M2023.lwr_family = ML_LWR_THRESHOLD_FAMILY_2023;
    M2023.upr_family = ML_UPR_THRESHOLD_FAMILY_2023;
    M2023.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2023;
    M2023.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2023;
    M2023.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2023;
    M2023.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2023;
    M2023.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2023;
    M2023.taper = ML_TAPER_2023;
    M2023.rate = ML_RATE_2023;
    M2023.has_sapto_thr = 1;
    M2023.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2023, ORD_TAX_RATES_2023);
        apply_sapto(taxi, P, Sapto2023);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2023, LITO_1ST_THRESH_2023, LITO_1ST_TAPER_2023);
        
        // Medicare levy
        double ml = do_1_ML(P, M2023);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2023
  } // outer case 2023
    break;
    
  case 2024: {
    // Create a Sapto struct for this year
    Sapto Sapto2024;
    Sapto2024.year = 2024;
    Sapto2024.pension_age = 65;
    Sapto2024.mxo_single = SAPTO_MAX_SINGLE_2024;
    Sapto2024.mxo_couple = SAPTO_MAX_MARRIED_2024;
    Sapto2024.lwr_single = SAPTO_LWR_SINGLE_2024;
    Sapto2024.lwr_couple = SAPTO_LWR_MARRIED_2024;
    Sapto2024.upr_single = SAPTO_LWR_SINGLE_2024 + SAPTO_MAX_SINGLE_2024 / SAPTO_TAPER_2024;
    Sapto2024.upr_couple = SAPTO_LWR_MARRIED_2024 + SAPTO_MAX_MARRIED_2024 / SAPTO_TAPER_2024;
    Sapto2024.taper = SAPTO_TAPER_2024;
    Sapto2024.first_tax_rate = ORD_TAX_RATES_2024[1];
    Sapto2024.tax_free_thresh = ORD_TAX_BRACK_2024[1];
    Sapto2024.lito_max_offset = LITO_MAX_OFFSET_2024;
    
    Medicare M2024;
    M2024.lwr_single = ML_LWR_THRESHOLD_SINGLE_2024;
    M2024.upr_single = ML_UPR_THRESHOLD_SINGLE_2024;
    M2024.lwr_family = ML_LWR_THRESHOLD_FAMILY_2024;
    M2024.upr_family = ML_UPR_THRESHOLD_FAMILY_2024;
    M2024.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2024;
    M2024.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2024;
    M2024.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2024;
    M2024.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2024;
    M2024.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2024;
    M2024.taper = ML_TAPER_2024;
    M2024.rate = ML_RATE_2024;
    M2024.has_sapto_thr = 1;
    M2024.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2024, ORD_TAX_RATES_2024);
        apply_sapto(taxi, P, Sapto2024);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2024, LITO_1ST_THRESH_2024, LITO_1ST_TAPER_2024, LITO_2ND_THRESH_2024, LITO_2ND_TAPER_2024);
        
        // Medicare levy
        double ml = do_1_ML(P, M2024);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2024
  } // outer case 2024
    break;
    
  case 2025: {
    // Create a Sapto struct for this year
    Sapto Sapto2025;
    Sapto2025.year = 2025;
    Sapto2025.pension_age = 65;
    Sapto2025.mxo_single = SAPTO_MAX_SINGLE_2025;
    Sapto2025.mxo_couple = SAPTO_MAX_MARRIED_2025;
    Sapto2025.lwr_single = SAPTO_LWR_SINGLE_2025;
    Sapto2025.lwr_couple = SAPTO_LWR_MARRIED_2025;
    Sapto2025.upr_single = SAPTO_LWR_SINGLE_2025 + SAPTO_MAX_SINGLE_2025 / SAPTO_TAPER_2025;
    Sapto2025.upr_couple = SAPTO_LWR_MARRIED_2025 + SAPTO_MAX_MARRIED_2025 / SAPTO_TAPER_2025;
    Sapto2025.taper = SAPTO_TAPER_2025;
    Sapto2025.first_tax_rate = ORD_TAX_RATES_2025[1];
    Sapto2025.tax_free_thresh = ORD_TAX_BRACK_2025[1];
    Sapto2025.lito_max_offset = LITO_MAX_OFFSET_2025;
    
    Medicare M2025;
    M2025.lwr_single = ML_LWR_THRESHOLD_SINGLE_2025;
    M2025.upr_single = ML_UPR_THRESHOLD_SINGLE_2025;
    M2025.lwr_family = ML_LWR_THRESHOLD_FAMILY_2025;
    M2025.upr_family = ML_UPR_THRESHOLD_FAMILY_2025;
    M2025.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2025;
    M2025.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2025;
    M2025.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2025;
    M2025.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2025;
    M2025.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2025;
    M2025.taper = ML_TAPER_2025;
    M2025.rate = ML_RATE_2025;
    M2025.has_sapto_thr = 1;
    M2025.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2025, ORD_TAX_RATES_2025);
        apply_sapto(taxi, P, Sapto2025);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2025, LITO_1ST_THRESH_2025, LITO_1ST_TAPER_2025, LITO_2ND_THRESH_2025, LITO_2ND_TAPER_2025);
        
        // Medicare levy
        double ml = do_1_ML(P, M2025);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2025
  } // outer case 2025
    break;
    
  case 2026: {
    // Create a Sapto struct for this year
    Sapto Sapto2026;
    Sapto2026.year = 2026;
    Sapto2026.pension_age = 65;
    Sapto2026.mxo_single = SAPTO_MAX_SINGLE_2026;
    Sapto2026.mxo_couple = SAPTO_MAX_MARRIED_2026;
    Sapto2026.lwr_single = SAPTO_LWR_SINGLE_2026;
    Sapto2026.lwr_couple = SAPTO_LWR_MARRIED_2026;
    Sapto2026.upr_single = SAPTO_LWR_SINGLE_2026 + SAPTO_MAX_SINGLE_2026 / SAPTO_TAPER_2026;
    Sapto2026.upr_couple = SAPTO_LWR_MARRIED_2026 + SAPTO_MAX_MARRIED_2026 / SAPTO_TAPER_2026;
    Sapto2026.taper = SAPTO_TAPER_2026;
    Sapto2026.first_tax_rate = ORD_TAX_RATES_2026[1];
    Sapto2026.tax_free_thresh = ORD_TAX_BRACK_2026[1];
    Sapto2026.lito_max_offset = LITO_MAX_OFFSET_2026;
    
    Medicare M2026;
    M2026.lwr_single = ML_LWR_THRESHOLD_SINGLE_2026;
    M2026.upr_single = ML_UPR_THRESHOLD_SINGLE_2026;
    M2026.lwr_family = ML_LWR_THRESHOLD_FAMILY_2026;
    M2026.upr_family = ML_UPR_THRESHOLD_FAMILY_2026;
    M2026.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2026;
    M2026.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2026;
    M2026.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2026;
    M2026.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2026;
    M2026.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2026;
    M2026.taper = ML_TAPER_2026;
    M2026.rate = ML_RATE_2026;
    M2026.has_sapto_thr = 1;
    M2026.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2026, ORD_TAX_RATES_2026);
        apply_sapto(taxi, P, Sapto2026);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2026, LITO_1ST_THRESH_2026, LITO_1ST_TAPER_2026, LITO_2ND_THRESH_2026, LITO_2ND_TAPER_2026);
        
        // Medicare levy
        double ml = do_1_ML(P, M2026);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2026
  } // outer case 2026
    break;
    
  case 2027: {
    // Create a Sapto struct for this year
    Sapto Sapto2027;
    Sapto2027.year = 2027;
    Sapto2027.pension_age = 65;
    Sapto2027.mxo_single = SAPTO_MAX_SINGLE_2027;
    Sapto2027.mxo_couple = SAPTO_MAX_MARRIED_2027;
    Sapto2027.lwr_single = SAPTO_LWR_SINGLE_2027;
    Sapto2027.lwr_couple = SAPTO_LWR_MARRIED_2027;
    Sapto2027.upr_single = SAPTO_LWR_SINGLE_2027 + SAPTO_MAX_SINGLE_2027 / SAPTO_TAPER_2027;
    Sapto2027.upr_couple = SAPTO_LWR_MARRIED_2027 + SAPTO_MAX_MARRIED_2027 / SAPTO_TAPER_2027;
    Sapto2027.taper = SAPTO_TAPER_2027;
    Sapto2027.first_tax_rate = ORD_TAX_RATES_2027[1];
    Sapto2027.tax_free_thresh = ORD_TAX_BRACK_2027[1];
    Sapto2027.lito_max_offset = LITO_MAX_OFFSET_2027;
    
    Medicare M2027;
    M2027.lwr_single = ML_LWR_THRESHOLD_SINGLE_2027;
    M2027.upr_single = ML_UPR_THRESHOLD_SINGLE_2027;
    M2027.lwr_family = ML_LWR_THRESHOLD_FAMILY_2027;
    M2027.upr_family = ML_UPR_THRESHOLD_FAMILY_2027;
    M2027.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2027;
    M2027.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2027;
    M2027.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2027;
    M2027.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2027;
    M2027.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2027;
    M2027.taper = ML_TAPER_2027;
    M2027.rate = ML_RATE_2027;
    M2027.has_sapto_thr = 1;
    M2027.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2027, ORD_TAX_RATES_2027);
        apply_sapto(taxi, P, Sapto2027);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2027, LITO_1ST_THRESH_2027, LITO_1ST_TAPER_2027, LITO_2ND_THRESH_2027, LITO_2ND_TAPER_2027);
        
        // Medicare levy
        double ml = do_1_ML(P, M2027);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2027
  } // outer case 2027
    break;
    
  case 2028: {
    // Create a Sapto struct for this year
    Sapto Sapto2028;
    Sapto2028.year = 2028;
    Sapto2028.pension_age = 65;
    Sapto2028.mxo_single = SAPTO_MAX_SINGLE_2028;
    Sapto2028.mxo_couple = SAPTO_MAX_MARRIED_2028;
    Sapto2028.lwr_single = SAPTO_LWR_SINGLE_2028;
    Sapto2028.lwr_couple = SAPTO_LWR_MARRIED_2028;
    Sapto2028.upr_single = SAPTO_LWR_SINGLE_2028 + SAPTO_MAX_SINGLE_2028 / SAPTO_TAPER_2028;
    Sapto2028.upr_couple = SAPTO_LWR_MARRIED_2028 + SAPTO_MAX_MARRIED_2028 / SAPTO_TAPER_2028;
    Sapto2028.taper = SAPTO_TAPER_2028;
    Sapto2028.first_tax_rate = ORD_TAX_RATES_2028[1];
    Sapto2028.tax_free_thresh = ORD_TAX_BRACK_2028[1];
    Sapto2028.lito_max_offset = LITO_MAX_OFFSET_2028;
    
    Medicare M2028;
    M2028.lwr_single = ML_LWR_THRESHOLD_SINGLE_2028;
    M2028.upr_single = ML_UPR_THRESHOLD_SINGLE_2028;
    M2028.lwr_family = ML_LWR_THRESHOLD_FAMILY_2028;
    M2028.upr_family = ML_UPR_THRESHOLD_FAMILY_2028;
    M2028.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2028;
    M2028.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2028;
    M2028.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2028;
    M2028.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2028;
    M2028.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2028;
    M2028.taper = ML_TAPER_2028;
    M2028.rate = ML_RATE_2028;
    M2028.has_sapto_thr = 1;
    M2028.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2028, ORD_TAX_RATES_2028);
        apply_sapto(taxi, P, Sapto2028);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2028, LITO_1ST_THRESH_2028, LITO_1ST_TAPER_2028, LITO_2ND_THRESH_2028, LITO_2ND_TAPER_2028);
        
        // Medicare levy
        double ml = do_1_ML(P, M2028);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2028
  } // outer case 2028
    break;
    
  case 2029: {
    // Create a Sapto struct for this year
    Sapto Sapto2029;
    Sapto2029.year = 2029;
    Sapto2029.pension_age = 65;
    Sapto2029.mxo_single = SAPTO_MAX_SINGLE_2029;
    Sapto2029.mxo_couple = SAPTO_MAX_MARRIED_2029;
    Sapto2029.lwr_single = SAPTO_LWR_SINGLE_2029;
    Sapto2029.lwr_couple = SAPTO_LWR_MARRIED_2029;
    Sapto2029.upr_single = SAPTO_LWR_SINGLE_2029 + SAPTO_MAX_SINGLE_2029 / SAPTO_TAPER_2029;
    Sapto2029.upr_couple = SAPTO_LWR_MARRIED_2029 + SAPTO_MAX_MARRIED_2029 / SAPTO_TAPER_2029;
    Sapto2029.taper = SAPTO_TAPER_2029;
    Sapto2029.first_tax_rate = ORD_TAX_RATES_2029[1];
    Sapto2029.tax_free_thresh = ORD_TAX_BRACK_2029[1];
    Sapto2029.lito_max_offset = LITO_MAX_OFFSET_2029;
    
    Medicare M2029;
    M2029.lwr_single = ML_LWR_THRESHOLD_SINGLE_2029;
    M2029.upr_single = ML_UPR_THRESHOLD_SINGLE_2029;
    M2029.lwr_family = ML_LWR_THRESHOLD_FAMILY_2029;
    M2029.upr_family = ML_UPR_THRESHOLD_FAMILY_2029;
    M2029.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2029;
    M2029.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2029;
    M2029.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2029;
    M2029.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2029;
    M2029.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2029;
    M2029.taper = ML_TAPER_2029;
    M2029.rate = ML_RATE_2029;
    M2029.has_sapto_thr = 1;
    M2029.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2029, ORD_TAX_RATES_2029);
        apply_sapto(taxi, P, Sapto2029);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2029, LITO_1ST_THRESH_2029, LITO_1ST_TAPER_2029, LITO_2ND_THRESH_2029, LITO_2ND_TAPER_2029);
        
        // Medicare levy
        double ml = do_1_ML(P, M2029);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2029
  } // outer case 2029
    break;
    
  case 2030: {
    // Create a Sapto struct for this year
    Sapto Sapto2030;
    Sapto2030.year = 2030;
    Sapto2030.pension_age = 65;
    Sapto2030.mxo_single = SAPTO_MAX_SINGLE_2030;
    Sapto2030.mxo_couple = SAPTO_MAX_MARRIED_2030;
    Sapto2030.lwr_single = SAPTO_LWR_SINGLE_2030;
    Sapto2030.lwr_couple = SAPTO_LWR_MARRIED_2030;
    Sapto2030.upr_single = SAPTO_LWR_SINGLE_2030 + SAPTO_MAX_SINGLE_2030 / SAPTO_TAPER_2030;
    Sapto2030.upr_couple = SAPTO_LWR_MARRIED_2030 + SAPTO_MAX_MARRIED_2030 / SAPTO_TAPER_2030;
    Sapto2030.taper = SAPTO_TAPER_2030;
    Sapto2030.first_tax_rate = ORD_TAX_RATES_2030[1];
    Sapto2030.tax_free_thresh = ORD_TAX_BRACK_2030[1];
    Sapto2030.lito_max_offset = LITO_MAX_OFFSET_2030;
    
    Medicare M2030;
    M2030.lwr_single = ML_LWR_THRESHOLD_SINGLE_2030;
    M2030.upr_single = ML_UPR_THRESHOLD_SINGLE_2030;
    M2030.lwr_family = ML_LWR_THRESHOLD_FAMILY_2030;
    M2030.upr_family = ML_UPR_THRESHOLD_FAMILY_2030;
    M2030.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_2030;
    M2030.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_2030;
    M2030.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_2030;
    M2030.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_2030;
    M2030.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_2030;
    M2030.taper = ML_TAPER_2030;
    M2030.rate = ML_RATE_2030;
    M2030.has_sapto_thr = 1;
    M2030.sapto_age = 65;
    {
      for (R_xlen_t i = 0; i < N; ++i) {
        Person P;
        int xi = ic_taxable_income_loss[i];
        double taxi = 0;
        
        P.xi = xi;
        P.yi = c0(spc_rebate_income[i]);
        P.agei = c_age_30_june[i];
        P.is_married = partner_status[i];
        P.n_child = n_dependants[i];
        P.is_family = P.is_married || P.n_child;
        
        taxi += income_taxi_nb(xi, ORD_TAX_BRACK_2030, ORD_TAX_RATES_2030);
        apply_sapto(taxi, P, Sapto2030);
        apply_lito(taxi, xi, LITO_MAX_OFFSET_2030, LITO_1ST_THRESH_2030, LITO_1ST_TAPER_2030, LITO_2ND_THRESH_2030, LITO_2ND_TAPER_2030);
        
        // Medicare levy
        double ml = do_1_ML(P, M2030);
        taxi += ml;
        
        
        // finally
        out[i] = taxi;
      }
      
    } // inner case 2030
  } // outer case 2030
    break;
  }
  
  
  
  return out;
}





