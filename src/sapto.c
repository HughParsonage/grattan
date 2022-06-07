#include "grattan.h"

// s12 of Income Tax Assessment (1936 Act) Regulation 2015
// specfies 6000 and 0.15 in the regulations
// http://classic.austlii.edu.au/au/legis/cth/consol_reg/ita1936ar2015352/s12.html
int SAPTO_S12_THRESH = 6000;

double SAPTO_S12_TAPER = 0.15;
double SAPTO_TAPER = 0.125;



static double do_1_sapto_sf(int x, int y, int age, bool is_married, Sapto S) {
  // x is rebate income
  // y is spouse rebate income
  if (age < S.pension_age) {
    // ineligible
    return 0;
  }
  
  
  int max_offset = is_married ? S.mxo_couple : S.mxo_single;
  int lwr_thresh = is_married ? S.lwr_couple : S.lwr_single;
  double taper = S.taper;
  
  double o = x < lwr_thresh ? max_offset : dmax0(max_offset - taper * (x - lwr_thresh));
  if (!is_married) {
    return o;
  }
  
  // The transfer of unused SAPTO is very complex and frankly unknown, even
  // within govt.  This lines up 'better' than known models.
  
  // If the spouse's income is so high that no spouse SAPTO is 
  // transferrable, then we just fall back to the original 
  const int MAX_THR_SPOUSE_XFER_MARRIED = ceil(1602.0 / SAPTO_S12_TAPER + SAPTO_S12_THRESH);
  if (y > MAX_THR_SPOUSE_XFER_MARRIED) {
    return o;
  }
  
  double sp_unused_sapto = 
    (y < SAPTO_S12_THRESH) ? max_offset : dmax0(max_offset - SAPTO_S12_TAPER * (y - SAPTO_S12_THRESH));
  
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
  
  return dmax0(FF);
}

void apply_sapto(double * taxi, Person P, Sapto S) {
  double sapto = do_1_sapto_sf(P.ri, P.yi, P.agei, P.is_married, S);
  if (sapto >= *taxi) {
    *taxi = 0;
  } else {
    *taxi -= sapto;
  }
}

bool bw01(double x) {
  return !ISNAN(x) && x >= 0 && x <= 1;
}



int lwr_threshold(int mxo, int ord_thresh1, double ord_rate1, int max_lito) {
  double o = max_lito + mxo;
  o /= ord_rate1;
  o += ord_thresh1;
  return ceil(o); 
}

static bool valid_sapto_rel(int mxo, int lwr, int upr,
                            int ord_thresh1, double ord_rate1,
                            int max_lito, double taper) {
  int expected_lwr = lwr_threshold(mxo, ord_thresh1, ord_rate1, max_lito);
  if (expected_lwr != lwr) {
    return false;
  }
  if (taper == 0.125) {
    int expected_upr = lwr + (mxo << 3);
    if (expected_upr != upr) {
      return false;
    }
  } else {
    int expected_upr = ceil(lwr + ((double)mxo) / taper);
    if (expected_upr != upr) {
      return false;
    }
  }
  return true;
}

void validate_sapto(Sapto * S, int fix) {
  int year = S->year;
  if (year < MIN_YEAR) {
    error("Sapto.year = %d but must be %d or later", year, MIN_YEAR);
  }
  
  double pension_age = S->pension_age;
  if (ISNAN(pension_age)) {
    error("pension_age was NaN.");
  }
  if (R_finite(pension_age)) {
    if (pension_age > 150) {
      if (fix) {
        warning("`Sapto.pension_age = %f` and so will be set to positive infinity");
        S->pension_age = R_PosInf;
      } else {
        error("`Sapto.pension_age = %f` which is an unlikely value.");
      }
    }
  }
  
  int mxo_single = S->mxo_single;
  int mxo_couple = S->mxo_couple;
  
  int lwr_single = S->lwr_single;
  int lwr_couple = S->lwr_couple;
  
  int upr_single = S->upr_single;
  int upr_couple = S->upr_couple;
  double taper = S->taper;
  if (taper < 0) {
    if (fix) {
      warning("Sapto.taper < 0 and so sign will be reversed.");  
      S->taper = -taper;
    } else {
      error("S.taper < 0.");
    }
  }
  
  if (upr_single <= lwr_single) {
    S->upr_single = S->mxo_single / S->taper;
  }
  if (upr_couple <= lwr_couple) {
    S->upr_couple = S->mxo_couple / S->taper;
  }
  double first_tax_rate = S->first_tax_rate;
  double second_tax_rate = S->second_tax_rate;
  int tax_free_thresh = S->tax_free_thresh;
  int tax_2nd_thresh = S->tax_2nd_thresh;
  double lito_max_offset = S->lito_max_offset;
  double lito_1st_thresh = S->lito_1st_thresh;
  double lito_1st_taper = S->lito_1st_taper;
  
  
  if (!bw01(second_tax_rate)) {
    error("Sapto.second_tax_rate not in [0, 1]");
  }
  if (!bw01(first_tax_rate) || first_tax_rate > second_tax_rate) {
    error("Sapto.first_tax_rate must be between 0 and S.second_tax_rate");
  }
  
  
  
}

SEXP Csapto_dat(SEXP Yr, SEXP ee) {
  int y = asInteger(Yr);
  int e = asInteger(ee);
  System Sys = yr2System(y);
  switch(e) {
  case 0:
    return ScalarReal(Sys.S.pension_age);
  case 1:
    return ScalarInteger(Sys.S.mxo_single);
  case 2:
    return ScalarInteger(Sys.S.mxo_couple);
  case 3:
    return ScalarInteger(Sys.S.lwr_single);
  case 4:
    return ScalarInteger(Sys.S.lwr_couple);
  case 5:
    return ScalarInteger(Sys.S.upr_single);
  case 6:
    return ScalarInteger(Sys.S.upr_couple);
  }
  return ScalarReal(0.125);
}

static bool FamilyStatus_is_single(SEXP x, R_xlen_t i) {
  const char * xi = CHAR(STRING_ELT(x, i));
  return xi[0] == 's';
}

static unsigned char code_OnSaptoCd(SEXP x, R_xlen_t i) {
  const char * xi = CHAR(STRING_ELT(x, i));
  return xi[0];
}

static void set_on_sapto_cd(unsigned char * on_sapto_cd, R_xlen_t N, 
                            SEXP FamilyStatus, SEXP OnSaptoCd) {
  if (xlength(FamilyStatus) == 1 &&
      xlength(OnSaptoCd) == 1) {
    unsigned char family_status0 = FamilyStatus_is_single(FamilyStatus, 0) ? 'A' : 'D';
    unsigned char on_sapto_cd0 = code_OnSaptoCd(OnSaptoCd, 0);
    if (family_status0 != on_sapto_cd0) {
      REprintf("family status and on_sapto_cd differ and so will on_sapto_cd = '%uc'"
                 " will be used.", on_sapto_cd0);
    }
    memset(on_sapto_cd, on_sapto_cd0, N);
    return;
  }
  if (xlength(OnSaptoCd) == N) {
    int nThread = 1;
    FORLOOP({
      on_sapto_cd[i] = code_OnSaptoCd(FamilyStatus, i);
    })
      return;
  }
  if (xlength(FamilyStatus) == N) {
    int nThread = 1;
    FORLOOP({
      on_sapto_cd[i] = FamilyStatus_is_single(FamilyStatus, i) ? 'A' : 'D';
    })
    return;
  }
  error("FamilyStatus and OnSaptoCd had different lengths.");
}

SEXP Csapto(SEXP RebateIncome, SEXP Yr, SEXP Fill, SEXP SaptoEligible, SEXP SpcRebateIncome, SEXP FamilyStatus,
            SEXP OnSaptoCd) {
  R_xlen_t N = xlength(RebateIncome);
  int yr = asInteger(Yr);
  System Sys = yr2System(yr);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  int nThread = 1;
  if (!Sys.has_sapto) {
    double fill = asReal(Fill);
    FORLOOP({
      ansp[i] = fill;
    })
    UNPROTECT(1);
    return ans;
  }
  Sapto S = Sys.S; 
  const int * xp = INTEGER(RebateIncome);
  const int * se = LOGICAL(SaptoEligible);
  const bool nse = xlength(SaptoEligible) == N;
  const int * sp = INTEGER(SpcRebateIncome);
  const bool nsp = xlength(SpcRebateIncome) == N;
  unsigned char * is_married = malloc(sizeof(char) * N);
  if (is_married == NULL) {
    UNPROTECT(1);
    return R_NilValue;
  }
  FORLOOP({
    ansp[i] = do_1_sapto_sf(xp[i], nsp ? sp[i] : sp[0], (nse ? se[i] : se[0]) ? 67 : 42, is_married[i], S);
  })
  UNPROTECT(1);
  return ans;
}




