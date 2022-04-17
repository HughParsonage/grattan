#include "grattan.h"



// s12 of Income Tax Assessment (1936 Act) Regulation 2015
// specfies 6000 and 0.15 in the regulations
// http://classic.austlii.edu.au/au/legis/cth/consol_reg/ita1936ar2015352/s12.html
const int SAPTO_S12_THRESH = 6000;
const double SAPTO_S12_TAPER = 0.15;
const double SAPTO_TAPER = 0.125;




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

static void apply_lito(double * taxi, int x, double y, double b1, double r1) {
  double lito = (x < b1) ? y : max0(y + r1 * (x - b1));
  if (*taxi <= lito) {
    *taxi = 0;
  } else {
    *taxi -= lito;
  }
}

static void apply_lito(double * taxi, int x, int y, double b1, double r1, double b2, double r2) {
  double lito = y;
  if (x > b1) {
    if (x < b2) {
      lito += r1 * (x - b1);
    } else {
      lito += r1 * (b2 - b1) + r2 * (x - b2);
    }
  }
  
  if (*taxi <= lito) {
    *taxi = 0;
  } else {
    *taxi -= lito;
  }
}

const double LMITO_1ST_OFFSET = 255;
const double LMITO_2ND_LEVEL = 1080;
const int LMITO_THRESHOLDS[4] = {37000, 48000, 90000, 126000};
const double LMITO_TAPER_RATES[4] = {0.075, 0, -0.03, 0};

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


static double do_ordinary_PIT(Person P, int * bracks, double * rates, int nb) {
  int xd = P.xi;
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

static double do_1_ML(Person P, Medicare M) {
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
    if (family_income <= lower_family_threshold) {
      return 0;
    }
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

static double do_1_sapto_sf(int x, int y, int age, bool is_married, Sapto S) {
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
  const double MAX_THR_SPOUSE_XFER_MARRIED = 1602.0 / SAPTO_S12_TAPER + SAPTO_S12_THRESH;
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

static void apply_sapto(double * taxi, Person P, Sapto S) {
  double sapto = do_1_sapto_sf(P.xi, P.yi, P.agei, P.is_married, S);
  if (sapto >= *taxi) {
    *taxi = 0;
  } else {
    *taxi -= sapto;
  }
}

static double tax1(Person P, System S) {
  
  double taxi = do_ordinary_PIT(P, S.BRACKETS, S.RATES, S.nb);
  taxi += do_1_ML(P, S.M);
  return taxi;
}

int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
}

System yr2System(int yr) {
  System Sys;
  
  Sys.M = yr2Medicare(yr);
  return Sys;
}

SEXP Cincome_tax(SEXP Yr, SEXP IcTaxableIncome, SEXP Age, SEXP IsMarried, SEXP nDependants,
                 SEXP SpcRebateIncome) {
  if (xlength(Yr) != 1) {
    error("Yr must be length-one."); // # nocov
  }
  R_xlen_t N = xlength(IcTaxableIncome);
  isEquiInt(IcTaxableIncome, Age);
  isEquiInt(IcTaxableIncome, IsMarried);
  isEquiInt(IcTaxableIncome, nDependants);
  isEquiInt(IcTaxableIncome, SpcRebateIncome);
  const int yr = asInteger(Yr);
  const int * ic_taxable_income_loss = INTEGER(IcTaxableIncome);
  const int * spc_rebate_income = INTEGER(SpcRebateIncome);
  const int * c_age_30_june = INTEGER(Age);
  const int * is_married = INTEGER(IsMarried);
  const int * n_dependants = INTEGER(nDependants);
  
  Medicare M = yr2Medicare(yr);
  System Sys;
  
  for (R_xlen_t i = 0; i < N; ++i) {
    Person P;
    P.xi = ic_taxable_income_loss[i];
    P.yi = c0(spc_rebate_income[i]);
    P.agei = c_age_30_june[i];
    P.is_married = is_married[i];
    P.n_child = n_dependants[i];
    P.is_family = P.is_married || P.n_child || P.yi;
    
  }
  
  return R_NilValue;
}




