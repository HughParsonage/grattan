#include "grattan.h"

// s12 of Income Tax Assessment (1936 Act) Regulation 2015
// specfies 6000 and 0.15 in the regulations
// http://classic.austlii.edu.au/au/legis/cth/consol_reg/ita1936ar2015352/s12.html
int SAPTO_S12_THRESH = 6000;

double SAPTO_S12_TAPER = 0.15;
double SAPTO_TAPER = 0.125;

void set_sapto_cutoffs(Sapto *S) {
  if (!R_FINITE(S->taper)) {
    error("sapto_taper must be finite.");
  }
  if (S->taper == 0) {
    S->upr_single = S->upr_couple = S->upr_illness = R_PosInf;
    return;
  }
  S->upr_single = trunc(S->lwr_single + S->mxo_single / S->taper);
  S->upr_couple = 2 * trunc(S->lwr_couple + S->mxo_couple / S->taper);
  S->upr_illness = 2 * trunc(S->lwr_illness + S->mxo_illness / S->taper);
}

static SEXP sapto_cutoff_scalar(double cutoff) {
  // Preserve the integer fields of existing annual systems. Uncapped and
  // large combined-income thresholds use the full numeric range.
  return cutoff >= -INT_MAX && cutoff <= INT_MAX ? ScalarInteger((int)cutoff) : ScalarReal(cutoff);
}

SEXP Sapto2Sexp(Sapto S) {
  SEXP ans = PROTECT(allocVector(VECSXP, SAPTO_LEN));
  SEXP nms = PROTECT(allocVector(STRSXP, SAPTO_LEN));
  SET_VECTOR_ELT(ans, 0, ScalarInteger(S.year));
  SET_STRING_ELT(nms, 0, mkCharCE("year", CE_UTF8));
  SET_VECTOR_ELT(ans, 1, ScalarReal(S.pension_age));
  SET_STRING_ELT(nms, 1, mkCharCE("pension_age", CE_UTF8));
  SET_VECTOR_ELT(ans, 2, ScalarInteger(S.mxo_single));
  SET_STRING_ELT(nms, 2, mkCharCE("max_offset", CE_UTF8));
  SET_VECTOR_ELT(ans, 3, ScalarInteger(S.mxo_couple));
  SET_STRING_ELT(nms, 3, mkCharCE("max_offset_married", CE_UTF8));
  SET_VECTOR_ELT(ans, 4, ScalarInteger(S.lwr_single));
  SET_STRING_ELT(nms, 4, mkCharCE("lower_threshold", CE_UTF8));
  SET_VECTOR_ELT(ans, 5, ScalarInteger(S.lwr_couple));
  SET_STRING_ELT(nms, 5, mkCharCE("lower_threshold_married", CE_UTF8));
  SET_VECTOR_ELT(ans, 6, sapto_cutoff_scalar(S.upr_single));
  SET_STRING_ELT(nms, 6, mkCharCE("upr_single", CE_UTF8));
  SET_VECTOR_ELT(ans, 7, sapto_cutoff_scalar(S.upr_couple));
  SET_STRING_ELT(nms, 7, mkCharCE("upr_couple", CE_UTF8));
  SET_VECTOR_ELT(ans, 8, ScalarReal(S.taper));
  SET_STRING_ELT(nms, 8, mkCharCE("taper", CE_UTF8));
  SET_VECTOR_ELT(ans, 9, ScalarReal(S.first_tax_rate));
  SET_STRING_ELT(nms, 9, mkCharCE("first_tax_rate", CE_UTF8));
  SET_VECTOR_ELT(ans, 10, ScalarReal(S.second_tax_rate));
  SET_STRING_ELT(nms, 10, mkCharCE("second_tax_rate", CE_UTF8));
  SET_VECTOR_ELT(ans, 11, ScalarReal(S.tax_free_thresh));
  SET_STRING_ELT(nms, 11, mkCharCE("tax_free_thresh", CE_UTF8));
  SET_VECTOR_ELT(ans, 12, ScalarReal(S.tax_2nd_thresh));
  SET_STRING_ELT(nms, 12, mkCharCE("tax_2nd_thresh", CE_UTF8));
  SET_VECTOR_ELT(ans, 13, ScalarReal(S.lito_max_offset));
  SET_STRING_ELT(nms, 13, mkCharCE("lito_max_offset", CE_UTF8));
  SET_VECTOR_ELT(ans, 14, ScalarReal(S.lito_1st_thresh));
  SET_STRING_ELT(nms, 14, mkCharCE("lito_1st_thresh", CE_UTF8));
  SET_VECTOR_ELT(ans, 15, ScalarReal(S.lito_1st_taper));
  SET_STRING_ELT(nms, 15, mkCharCE("lito_1st_taper", CE_UTF8));
  SET_VECTOR_ELT(ans, 16, ScalarInteger(S.mxo_illness));
  SET_STRING_ELT(nms, 16, mkCharCE("max_offset_illness", CE_UTF8));
  SET_VECTOR_ELT(ans, 17, ScalarInteger(S.lwr_illness));
  SET_STRING_ELT(nms, 17, mkCharCE("lower_threshold_illness", CE_UTF8));
  SET_VECTOR_ELT(ans, 18, sapto_cutoff_scalar(S.upr_illness));
  SET_STRING_ELT(nms, 18, mkCharCE("upr_illness", CE_UTF8));
  setAttrib(ans, R_NamesSymbol, nms);
  UNPROTECT(2);
  return ans;
}

static int smax_offset(Sapto S, unsigned int on_sapto_cd) {
  switch(on_sapto_cd) {
  case SAPTO_A:
    return S.mxo_single;
  case SAPTO_B:
    return S.mxo_illness;
  case SAPTO_C:
    return S.mxo_illness;
  case SAPTO_D:
    return S.mxo_couple;
  case SAPTO_E:
    return S.mxo_couple;
  default:
    return S.mxo_single;
  }
}

static int slwr_thresh(Sapto S, unsigned int on_sapto_cd) {
  switch(on_sapto_cd) {
  case SAPTO_A:
    return S.lwr_single;
  case SAPTO_B:
    return S.lwr_illness;
  case SAPTO_C:
    return S.lwr_illness;
  case SAPTO_D:
    return S.lwr_couple;
  case SAPTO_E:
    return S.lwr_couple;
  default:
    return S.lwr_single;
  }
}

static double supr_thresh(Sapto S, unsigned int on_sapto_cd) {
  switch(on_sapto_cd) {
  case SAPTO_A:
    return S.upr_single;
  case SAPTO_B:
    return S.upr_illness;
  case SAPTO_C:
    return S.upr_illness;
  case SAPTO_D:
    return S.upr_couple;
  case SAPTO_E:
    return S.upr_couple;
  default:
    return S.upr_single;
  }
}

static unsigned char normalise_sapto_code(unsigned int code) {
  if (code >= SAPTO_A && code <= SAPTO_E) {
    return code;
  }
  return code >= 'A' && code <= 'E' ? code - 'A' + SAPTO_A : 0;
}

static bool sapto_income_eligible(int x, int y, unsigned int code, Sapto S) {
  if (code < SAPTO_A || code > SAPTO_E || x == NA_INTEGER || y == NA_INTEGER) {
    return false;
  }
  // The annual couple/illness ceilings are combined amounts. ITAA36
  // s160AAAA(4) tests half of combined income against the individual ceiling.
  int64_t income = code == SAPTO_A ? x : (int64_t)x + y;
  return income < supr_thresh(S, code);
}

// Only the portion of basic tax up to the base rebate can be transferred.
// Every statutory base is exhausted within the first two positive-rate bands.
static double senior_basic_tax(int taxable, Sapto S) {
  double first_band = dmax0(dmin(taxable, S.tax_2nd_thresh) - S.tax_free_thresh);
  return S.first_tax_rate * first_band +
    S.second_tax_rate * dmax0((double)taxable - S.tax_2nd_thresh);
}

static double transferred_threshold(double base, Sapto S) {
  double threshold = S.tax_free_thresh + (S.lito_max_offset + base) / S.first_tax_rate;
  if (S.year >= 2011 && threshold > S.lito_1st_thresh) {
    // July 2010 introduced the LITO crossing for couples. The 2012
    // amendment substituted the second marginal rate for the first.
    double marginal = S.year >= 2013 ? S.second_tax_rate : S.first_tax_rate;
    double rate = marginal + S.lito_1st_taper;
    threshold = (S.first_tax_rate * S.tax_free_thresh + S.lito_max_offset + base +
                 S.lito_1st_thresh * (rate - S.first_tax_rate)) / rate;
  }
  return ceil(threshold);
}

static double do_1_sapto_sf(int x, int y, int taxable, int spouse_taxable,
                             unsigned int on_sapto_cd, Sapto S) {
  unsigned char code = normalise_sapto_code(on_sapto_cd);
  if (!sapto_income_eligible(x, y, code, S)) {
    return 0;
  }
  double base = smax_offset(S, code);
  double threshold = slwr_thresh(S, code);
  bool paired = code == SAPTO_B || code == SAPTO_D;
  if (paired && S.taper > 0) {
    // Regulations 11-12 and former regs 150AD-150AE. The recipient must
    // already be shading out. The statutory test disregards other offsets.
    double original = base;
    double spouse_unused = dmax0(original - senior_basic_tax(spouse_taxable, S));
    double own_tax = senior_basic_tax(taxable, S);
    if (x > threshold && spouse_unused > 0) {
      if (S.year >= 2013 && spouse_taxable > SAPTO_S12_THRESH) {
        spouse_unused = dmax0(original - SAPTO_S12_TAPER * (spouse_taxable - SAPTO_S12_THRESH));
      }
      base += spouse_unused;
      if (S.year == 1997) {
        base = dmin(base, own_tax);
      }
    }
    // Reg 12(3)(a) reduces the transferor's base by the excess over basic
    // tax, even when the spouse's separate reg 12(3)(b) increase is nil.
    if (y > threshold) {
      base -= dmax0(original - own_tax);
    }
    if (base != original) {
      threshold = transferred_threshold(base, S);
    }
  }
  double amount = dmax0(base - S.taper * dmax0((double)x - threshold));
  // The low income aged persons rebate began at half rate in 1996-97.
  if (S.year == 1997) {
    amount *= 0.5;
  }
  return S.year <= 2000 ? amount : ceil(amount);
}

bool sapto_entitled(Person P, Sapto S) {
  // Explicit claim codes identify actual eligibility, including qualifying
  // pensioners below age-pension age. Age is used only when inferring codes.
  return do_1_sapto_sf(P.ri, P.yi, P.transfer_taxable, P.spouse_transfer_taxable, P.on_sapto_cd, S) >= 1;
}

bool apply_sapto(double * taxi, Person P, Sapto S) {
  double sapto = do_1_sapto_sf(P.ri, P.yi, P.transfer_taxable, P.spouse_transfer_taxable, P.on_sapto_cd, S);
  if (sapto >= *taxi) {
    *taxi = 0;
  } else {
    *taxi -= sapto;
  }
  return sapto >= 1;
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

// static bool valid_sapto_rel(int mxo, int lwr, int upr,
//                             int ord_thresh1, double ord_rate1,
//                             int max_lito, double taper) {
//   int expected_lwr = lwr_threshold(mxo, ord_thresh1, ord_rate1, max_lito);
//   if (expected_lwr != lwr) {
//     return false;
//   }
//   if (taper == 0.125) {
//     int expected_upr = lwr + (mxo << 3);
//     if (expected_upr != upr) {
//       return false;
//     }
//   } else {
//     int expected_upr = ceil(lwr + ((double)mxo) / taper);
//     if (expected_upr != upr) {
//       return false;
//     }
//   }
//   return true;
// }

void validate_sapto(Sapto * S, int fix) {
  int year = S->year;
  if (year < MIN_YEAR) {
    error("(validate_sapto)Sapto.year = %d but must be %d or later", year, MIN_YEAR);
  }
  
  double pension_age = S->pension_age;
  if (ISNAN(pension_age)) {
    error("(validate_sapto)pension_age was NaN.");
  }
  if (R_finite(pension_age)) {
    if (pension_age > 150) {
      if (fix) {
        warning("(validate_sapto)`Sapto.pension_age = %.1f` and so will be set to positive infinity",
                pension_age);
        S->pension_age = R_PosInf;
      } else {
        error("(validate_sapto)`Sapto.pension_age = %.1f` which is an unlikely value.",
              pension_age);
      }
    }
  }
  
  // int mxo_single = S->mxo_single;
  // int mxo_couple = S->mxo_couple;
  
  int lwr_single = S->lwr_single;
  int lwr_couple = S->lwr_couple;
  
  double taper = S->taper;
  if (taper < 0) {
    if (fix) {
      warning("(validate_sapto)Sapto.taper < 0 and so sign will be reversed.");  
      S->taper = -taper;
      set_sapto_cutoffs(S);
    } else {
      error("(validate_sapto)S.taper < 0.");
    }
  }
  
  double upr_single = S->upr_single;
  double upr_couple = S->upr_couple;
  if (upr_single <= lwr_single) {
    if (fix) {
      if (fix == 1) {
        warning("(validate_sapto)upr_single = %.0f, yet lwr_single = %d"
                  " and so upr_single will be reset to %.0f.",
                  upr_single, lwr_single, trunc(S->mxo_single / S->taper));
      }
      S->upr_single = trunc(S->mxo_single / S->taper);
    } else {
      error("(validate_sapto)upr_single = %.0f, yet lwr_single = %d",
            upr_single, lwr_single);
    }
  }
  if (upr_couple <= lwr_couple) {
    if (fix) {
      if (fix == 1) {
        warning("(validate_sapto)upr_couple = %.0f, yet lwr_couple = %d"
                  " and so upr_couple will be reset to %.0f.",
                  upr_couple, lwr_couple, trunc(S->mxo_couple / S->taper));
      }
      S->upr_couple = trunc(S->mxo_couple / S->taper);
    } else {
      error("(validate_sapto)upr_couple = %.0f, yet lwr_couple = %d",
            upr_couple, lwr_couple);
    }
  }
  
  double first_tax_rate = S->first_tax_rate;
  double second_tax_rate = S->second_tax_rate;
  int tax_free_thresh = S->tax_free_thresh;
  int tax_2nd_thresh = S->tax_2nd_thresh;
  if (tax_free_thresh > tax_2nd_thresh) {
    error("(validate_sapto)tax_free_thresh > tax_2nd_thresh");
  }
  
  double lito_max_offset = S->lito_max_offset;
  double lito_1st_thresh = S->lito_1st_thresh;
  double lito_1st_taper = S->lito_1st_taper;
  
  
  if (!bw01(second_tax_rate)) {
    error("(validate_sapto)Sapto.second_tax_rate not in [0, 1]");
  }
  if (!bw01(first_tax_rate) || first_tax_rate > second_tax_rate) {
    error("(validate_sapto)Sapto.first_tax_rate must be between 0 and S.second_tax_rate");
  }
  if (ISNAN(lito_max_offset)) {
    error("(validate_sapto)Sapto.lito_max_offset is NaN.");
  }
  if (ISNAN(lito_1st_thresh)) {
    error("(validate_sapto)Sapto.lito_1st_thresh is NaN.");
  }
  if (!bw01(lito_1st_taper)) {
    error("(validate_sapto)Sapto.lito_1st_taper must be between 0 and 1.");
  }
  
  
  
  
}



static unsigned char code_OnSaptoCd(SEXP x, R_xlen_t i) {
  switch(TYPEOF(x)) {
  case STRSXP: {
    const char * xi = CHAR(STRING_ELT(x, i));
    return STRING_ELT(x, i) == NA_STRING ? 0 : normalise_sapto_code(xi[0]);
  }
    break;
  case RAWSXP:
    return normalise_sapto_code(RAW_ELT(x, i));
    break;
  case INTSXP: {
    return normalise_sapto_code(INTEGER_ELT(x, i));
  }
  default:
    return 0;
  }
}

static SEXP raw_sapto_codes(SEXP x) {
  if (!isString(x) && !isInteger(x) && TYPEOF(x) != RAWSXP) {
    error("x was type '%s' but must be either RAWSXP or integer or character.", type2char(TYPEOF(x)));
  }
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * out = RAW(ans);
  // STRING_ELT/INTEGER_ELT may materialize ALTREP data on the R thread.
  for (R_xlen_t i = 0; i < N; ++i) {
    out[i] = code_OnSaptoCd(x, i);
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_asraw_OnSaptoCd(SEXP x, SEXP nthreads) {
  as_nThread(nthreads);
  return raw_sapto_codes(x);
}

SEXP Csapto(SEXP RebateIncome, SEXP Yr, SEXP Fill, 
            SEXP SaptoEligible, 
            SEXP SpcRebateIncome,
            SEXP TaxableIncome, SEXP SpouseTaxableIncome,
            SEXP OnSaptoCd) {
  R_xlen_t N = xlength(RebateIncome);
  if (!isInteger(Yr) || (xlength(Yr) != 1 && xlength(Yr) != N)) {
    error("fy.year must have length one or the number of rebate incomes.");
  }
  const int *years = INTEGER(Yr);
  bool scalar = xlength(Yr) == 1;
  bool loaded[MAX_YEAR - MIN_YEAR + 1] = {false};
  bool applies[MAX_YEAR - MIN_YEAR + 1] = {false};
  Sapto schedules[MAX_YEAR - MIN_YEAR + 1];
  for (R_xlen_t i = 0; i < xlength(Yr); ++i) {
    int yr = years[i];
    if (yr < MIN_YEAR || yr > MAX_YEAR) {
      error("Unsupported SAPTO year %d.", yr);
    }
    int j = yr - MIN_YEAR;
    if (!loaded[j]) {
      System sys = yr2System(yr);
      applies[j] = sys.has_sapto;
      schedules[j] = sys.S;
      loaded[j] = true;
    }
  }
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  int nThread = 1;
  if (xlength(Fill) != 1 || !(isReal(Fill) || isInteger(Fill))) {
    error("`fill` was a %s vector of length-%lld, but must be a length-one numeric vector.",
          type2char(TYPEOF(Fill)), (long long)xlength(Fill));
  }
  const double fill = asReal(Fill);
  const int * xp = INTEGER(RebateIncome);
  const int * taxable = INTEGER(TaxableIncome);
  const int * spouse_taxable = INTEGER(SpouseTaxableIncome);
  const bool single_taxable = xlength(TaxableIncome) == 1;
  const bool single_spouse_taxable = xlength(SpouseTaxableIncome) == 1;
  const int * se = LOGICAL(SaptoEligible);
  const bool nse = xlength(SaptoEligible) == N;
  const int * sp = INTEGER(SpcRebateIncome);
  const bool nsp = xlength(SpcRebateIncome) == N;
  const bool single_code = xlength(OnSaptoCd) == 1;
  if (!single_code && xlength(OnSaptoCd) != N) {
    error("on_sapto_cd must have length one or the length of rebate_income.");
  }
  if ((!single_taxable && xlength(TaxableIncome) != N) ||
      (!single_spouse_taxable && xlength(SpouseTaxableIncome) != N)) {
    error("Taxable incomes must have length one or the length of rebate_income.");
  }
  SEXP codes = PROTECT(TYPEOF(OnSaptoCd) == RAWSXP ? OnSaptoCd : raw_sapto_codes(OnSaptoCd));
  const unsigned char * on_sapto_cd = RAW(codes);
  FORLOOP({
    ansp[i] = fill;
    int j = years[scalar ? 0 : i] - MIN_YEAR;
    if (applies[j] && (nse ? se[i] : se[0])) {
      ansp[i] = do_1_sapto_sf(xp[i],
                              (nsp ? sp[i] : sp[0]), taxable[single_taxable ? 0 : i],
                              spouse_taxable[single_spouse_taxable ? 0 : i],
                              on_sapto_cd[single_code ? 0 : i],
                              schedules[j]);
    }
  })
    
  UNPROTECT(2);
  return ans;
}

static const int * pension_flag(SEXP x, R_xlen_t n) {
  if (isNull(x)) {
    return NULL;
  }
  if ((!isLogical(x) && !isInteger(x)) || (xlength(x) != 1 && xlength(x) != n)) {
    error("Incompatible pension-age flag.");
  }
  return isLogical(x) ? LOGICAL(x) : INTEGER(x);
}

SEXP C_sf2osc(SEXP Age, SEXP isMarried, SEXP Female, SEXP Yr, SEXP RSystem,
              SEXP PensionAge, SEXP SpouseAge) {
  R_xlen_t N = xlength(isMarried);
  const R_xlen_t n_year = xlength(Yr);
  if (!isInteger(Yr) || (n_year != 1 && n_year != N)) {
    error("yr must be an integer with length one or the number of records.");
  }
  const int * year = INTEGER(Yr);
  bool scalar_year = n_year == 1;
  double required_age[MAX_YEAR - MIN_YEAR + 1] = {0};
  bool initialized[MAX_YEAR - MIN_YEAR + 1] = {false};
  for (R_xlen_t i = 0; i < n_year; ++i) {
    int yr = year[i];
    if (yr < MIN_YEAR || yr > MAX_YEAR) {
      error("yr must be between %d and %d.", MIN_YEAR, MAX_YEAR);
    }
    if (!initialized[yr - MIN_YEAR]) {
      System Sys = Sexp2System(RSystem, yr);
      required_age[yr - MIN_YEAR] = Sys.has_sapto ? Sys.S.pension_age : R_PosInf;
      initialized[yr - MIN_YEAR] = true;
    }
  }
  if (isNull(Age)) {
    return R_NilValue;
  }
  const bool single_age = xlength(Age) == 1;
  const bool single_female = xlength(Female) == 1;
  if ((!isReal(Age) && !isInteger(Age)) || (!single_age && xlength(Age) != N) ||
      !isLogical(Female) || (!single_female && xlength(Female) != N)) {
    error("Incompatible age and sex vectors.");
  }
  const double * real_age = isReal(Age) ? REAL(Age) : NULL;
  const int * int_age = isInteger(Age) ? INTEGER(Age) : NULL;
  const int * female = LOGICAL(Female);
  const int * is_married = INTEGER(isMarried);
  const int * attained = pension_flag(PensionAge, N);
  const int * spouse_attained = pension_flag(SpouseAge, N);
  const bool single_attained = xlength(PensionAge) == 1;
  const bool single_spouse = xlength(SpouseAge) == 1;
  SEXP ans = PROTECT(allocVector(RAWSXP, N));
  unsigned char * restrict ansp = RAW(ans);
  int nThread = 1;
  FORLOOP({
    ansp[i] = 0;
    int yr = year[scalar_year ? 0 : i];
    double minimum = required_age[yr - MIN_YEAR];
    if (yr <= 2013 && female[single_female ? 0 : i] == TRUE && minimum == 65) {
      minimum = 60 + 0.5 * (imax(yr - 1994, 0) / 2);
    }
    R_xlen_t a = single_age ? 0 : i;
    int recorded = attained ? attained[single_attained ? 0 : i] : NA_INTEGER;
    bool eligible = (recorded == 0 || recorded == 1) ? recorded :
      (real_age ? R_FINITE(real_age[a]) && real_age[a] >= minimum
                : int_age[a] != NA_INTEGER && int_age[a] >= minimum);
    if (eligible && is_married[i] != NA_INTEGER) {
      ansp[i] = is_married[i] == 0 ? SAPTO_A :
        (spouse_attained && spouse_attained[single_spouse ? 0 : i] == 0 ? SAPTO_E : SAPTO_D);
    }
  })
  UNPROTECT(1);
  return ans;
}




