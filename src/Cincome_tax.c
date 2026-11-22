#include "grattan.h"
#include <float.h>

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
  return dmax0(out);
}

static double temp_budget_repair_levy(int x) {
  return TEMP_BUDGET_REPAIR_LEVY_RATE * dmax0((double)x - TEMP_BUDGET_REPAIR_LEVY_THRESH);
}

static double flood_levy(int x) {
  if (x <= 50000) {
    return 0;
  }
  if (x <= 100000) {
    return 0.005 * (x - 50000);
  }
  return 250 + 0.01 * (x - 100000);
}

static double do_ordinary_PIT(Person P, int const bracks[MAX_NBRACK], double const rates[MAX_NBRACK], int nb) {
  int xd = P.xi;
  double out = 0;
  for (int b = 0; b < nb - 1; ++b) {
    if (xd < bracks[b]) {
      return out;
    }
    // xa = above threshold
    // We express rates in terms of marginal rates *within* brackets, 
    // so if xd is larger than the next bracket, we apply this rate 
    // to the gap between this threshold and the next. First, however,
    // we need to check whether this is the last bracket "b + 1 == nb"
    int xa = (xd >= bracks[b + 1]) ? (bracks[b + 1] - bracks[b]) : (xd - bracks[b]);
    out += xa * rates[b];
  }
  if (xd > bracks[nb - 1]) {
    out += rates[nb - 1] * (xd - bracks[nb - 1]);
  }
  return out;
}




static double individual_ML(int income, const Medicare M, bool sapto) {
  const int lower_threshold = sapto && M.has_sapto_thr ? M.lwr_single_sapto : M.lwr_single;
  if (income <= lower_threshold) {
    return 0;
  }
  return dmin(M.taper * ((double)income - lower_threshold), M.rate * income);
}

static double family_ML_threshold(const Person P, const Medicare M, bool sapto) {
  return (sapto && M.has_sapto_thr ? M.lwr_family_sapto : M.lwr_family) +
    (double)P.n_child * M.lwr_thr_up_per_child;
}

static double family_ML_reduction(double family_income, double threshold, const Medicare M) {
  // Section 8(2): reduce the levy already limited by section 7.
  return dmax0(M.rate * threshold - (M.taper - M.rate) * (family_income - threshold));
}

static double do_1_ML(const Person P, const Medicare M, bool sapto, bool spouse_sapto) {
  const int income = imax(P.xi, 0);
  const int spouse_income = imax(P.yti, 0);
  double levy = individual_ML(income, M, sapto);
  const double spouse_levy = individual_ML(spouse_income, M, spouse_sapto);
  const double family_income = (double)income + spouse_income;

  if (M.max_levy > 0) {
    double ceiling = M.max_levy;
    // Medicare Levy Acts 1983 and 1984, section 8(2): share the ceiling
    // between spouses when combined income exceeds its full-rate boundary.
    if (P.is_married && M.rate * family_income > M.max_levy) {
      ceiling = M.max_levy / 2 + dmax0(M.max_levy / 2 - spouse_levy);
    }
    levy = dmin(levy, ceiling);
  }
  if (levy <= 0 || !P.is_family) {
    return levy;
  }

  const double threshold = family_ML_threshold(P, M, sapto);
  if (family_income <= threshold) {
    return 0;
  }
  double reduction = family_ML_reduction(family_income, threshold, M);
  if (P.is_married && spouse_levy > 0) {
    // Section 8(3) prorates a reduction only when both spouses owe levy.
    reduction *= income / family_income;

    // Section 8(4) transfers any reduction the spouse cannot use.
    const double spouse_threshold = family_ML_threshold(P, M, spouse_sapto);
    if (family_income > spouse_threshold) {
      const double spouse_reduction =
        family_ML_reduction(family_income, spouse_threshold, M) * spouse_income / family_income;
      reduction += dmax0(spouse_reduction - spouse_levy);
    }
  }
  return dmax0(levy - reduction);
}

static bool spouse_sapto_entitled(Person P, Sapto S) {
  if (P.on_sapto_cd != SAPTO_B && P.on_sapto_cd != SAPTO_D) {
    return false;
  }
  Person spouse = P;
  spouse.xi = P.yti;
  spouse.yti = P.xi;
  spouse.transfer_taxable = P.spouse_transfer_taxable;
  spouse.spouse_transfer_taxable = P.transfer_taxable;
  spouse.ri = P.yi;
  spouse.yi = P.ri;
  return sapto_entitled(spouse, S);
}


void apply_lmito(double * taxi, int x) {
  double lmito = do_1_lmito(x);
  if (lmito >= *taxi) {
    *taxi = 0;
  } else {
    *taxi -= lmito;
  }
}

int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
}

Person pp(int x, int y, int r, unsigned int age, bool is_married, unsigned int n_child) {
  Person P = 
    { .xi = x,
      .yi = y,
      .yti = y,
      .transfer_taxable = x,
      .spouse_transfer_taxable = y,
      .ri = r,
      .agei = age,
      .n_child = n_child,
      .on_sapto_cd = 0,
      .is_married = is_married,
      .is_family = n_child || is_married || y };
  return P;
}

unsigned int sapto_bitwise(unsigned char x) {
  switch(x) {
  case SAPTO_A:
  case 'A':
    return SAPTO_A;
  case SAPTO_B:
  case 'B':
    return SAPTO_B;
  case SAPTO_C:
  case 'C':
    return SAPTO_C;
  case SAPTO_D:
  case 'D':
    return SAPTO_D;
  case SAPTO_E:
  case 'E':
    return SAPTO_E;
  default:
    return 0;
  }
  return 0;
}

SEXP Cincome_tax(SEXP Yr,
                 SEXP IcTaxableIncome,
                 SEXP RebateIncome,
                 SEXP Age, 
                 SEXP IsMarried,
                 SEXP nDependants,
                 SEXP SpcRebateIncome, 
                 SEXP SpouseTaxableIncome,
                 SEXP OnSaptoCd,
                 SEXP iFrankCr,
                 SEXP IncomeOffsets,
                 SEXP FloodExempt,
                 SEXP TransferTaxable, SEXP SpouseTransferTaxable,
                 SEXP RSystem,
                 SEXP nthreads, 
                 SEXP Summary) {
  if (xlength(Yr) != 1) {
    error("Yr must be length-one."); // # nocov
  }
  if (!isNull(RSystem) && !isVectorList(RSystem)) {
    error("RSystem was type '%s', but must be NULL or a list.", type2char(TYPEOF(RSystem)));
  }
  int nThread = as_nThread(nthreads);
  if (!isInteger(Summary)) {
    error("Internal error(Cincome_tax):`Summary` was type '%s' but must be integer.", type2char(TYPEOF(Summary)));
  }
  const int summary = asInteger(Summary);
    
  R_xlen_t N = xlength(IcTaxableIncome);
  if (!isNull(IncomeOffsets) &&
      (!isReal(IncomeOffsets) || (xlength(IncomeOffsets) != 1 && xlength(IncomeOffsets) != N))) {
    error("IncomeOffsets must be NULL, one numeric amount or a numeric vector matching income.");
  }
  if (!isNull(FloodExempt)) {
    isEquiInt(IcTaxableIncome, FloodExempt, "FloodExempt");
  }
  if (!isInteger(Age) || (xlength(Age) != 1 && xlength(Age) != N)) {
    error("Age must be an integer scalar or match income.");
  }
  isEquiInt(IcTaxableIncome, RebateIncome, "RebateIncome");
  isEquiInt(IcTaxableIncome, IsMarried, "IsMarried");
  isEquiInt(IcTaxableIncome, nDependants, "nDependants");
  isEquiInt(IcTaxableIncome, SpcRebateIncome, "SpcRebateIncome");
  isEquiInt(IcTaxableIncome, SpouseTaxableIncome, "SpouseTaxableIncome");
  const int yr = asInteger(Yr);
  const int * ic_taxable_income_loss = INTEGER(IcTaxableIncome);
  const int * rebate_income = INTEGER(RebateIncome);
  const int * spc_rebate_income = INTEGER(SpcRebateIncome);
  const int * spouse_taxable_income = INTEGER(SpouseTaxableIncome);
  isEquiInt(IcTaxableIncome, TransferTaxable, "transfer taxable income");
  isEquiInt(IcTaxableIncome, SpouseTransferTaxable, "spouse transfer taxable income");
  const int * transfer = INTEGER(TransferTaxable);
  const int * spouse_transfer = INTEGER(SpouseTransferTaxable);
  const int * c_age_30_june = INTEGER(Age);
  const int * is_married = INTEGER(IsMarried);
  const int * n_dependants = INTEGER(nDependants);
  if (N != xlength(OnSaptoCd)) {
    error("xlength(OnSaptoCd) = %lld, yet N = %lld", (long long)xlength(OnSaptoCd), (long long)N);
  }
  const unsigned char * on_sapto_cdp = RAW(OnSaptoCd);
  
  const System Sys = Sexp2System(RSystem, yr);
  
  const double *offsets = isNull(IncomeOffsets) ? NULL : REAL(IncomeOffsets);
  const int *exempt = isNull(FloodExempt) ? NULL : INTEGER(FloodExempt);
  const int *franking = isInteger(iFrankCr) && xlength(iFrankCr) == N ? INTEGER(iFrankCr) : NULL;
  const bool scalar_age = xlength(Age) == 1;
  const bool scalar_offset = xlength(IncomeOffsets) == 1;
  SEXP ans = PROTECT(allocVector(REALSXP, summary ? 1 : N));
  double *out = REAL(ans);
  double total = 0;

  // Consume each person's fields together. Summary modes need only a scalar.
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+:total)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    Person P = {0};
    P.xi = ic_taxable_income_loss[i];
    P.ri = rebate_income[i];
    P.yi = spc_rebate_income[i];
    P.yti = imax(spouse_taxable_income[i], 0);
    P.transfer_taxable = transfer[i];
    P.spouse_transfer_taxable = spouse_transfer[i];
    P.agei = c_age_30_june[scalar_age ? 0 : i] & 127;
    P.n_child = n_dependants[i] & 15;
    P.is_married = is_married[i] || P.yti > 0;
    P.is_family = P.is_married || P.n_child;
    P.on_sapto_cd = sapto_bitwise(on_sapto_cdp[i]);

    double tax = do_ordinary_PIT(P, Sys.BRACKETS, Sys.RATES, Sys.nb);
    bool senior = Sys.has_sapto && apply_sapto(&tax, P, Sys.S);
    if (offsets) {
      tax = dmax0(tax - offsets[scalar_offset ? 0 : i]);
    }
    if (Sys.n_offsetn) {
      tax = apply_offsets(tax, P.xi, Sys.Offsets, Sys.n_offsetn);
    }
    tax += do_1_ML(P, Sys.M, senior, Sys.has_sapto && spouse_sapto_entitled(P, Sys.S));
    if (Sys.has_temp_budget_repair_levy) {
      tax += temp_budget_repair_levy(P.xi);
    }
    if (Sys.has_flood_levy && (!exempt || !exempt[i])) {
      tax += flood_levy(P.xi);
    }
    if (franking) {
      tax -= franking[i];
    }
    if (summary) {
      total += tax;
    } else {
      out[i] = tax;
    }
  }

  if (summary) {
    out[0] = summary == 2 ? total / (double)N : total;
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cwhole_dollar_tax(SEXP x) {
  if (!isReal(x)) {
    return coerceVector(x, INTSXP);
  }
  const R_xlen_t N = xlength(x);
  const double *tax = REAL(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int *out = INTEGER(ans);
  bool out_of_range = false;
  for (R_xlen_t i = 0; i < N; ++i) {
    double value = tax[i];
    if (ISNAN(value)) {
      out[i] = NA_INTEGER;
      continue;
    }
    double nearest = nearbyint(value);
    if (fabs(value - nearest) <= 2 * DBL_EPSILON * fmax(1, fabs(value))) {
      value = nearest;
    }
    if (value >= (double)INT_MAX + 1 || value <= (double)INT_MIN) {
      out[i] = NA_INTEGER;
      out_of_range = true;
    } else {
      out[i] = (int)value;
    }
  }
  if (out_of_range) {
    warning("NAs introduced by coercion to integer range");
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cordinary_tax(SEXP Yr, SEXP Income, SEXP RSystem) {
  if (xlength(Yr) != 1 || !isInteger(Income)) {
    error("Cordinary_tax requires a single year and integer incomes.");
  }
  const System Sys = Sexp2System(RSystem, asInteger(Yr));
  const R_xlen_t N = xlength(Income);
  const int * income = INTEGER(Income);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    Person P = {0};
    P.xi = income[i];
    REAL(ans)[i] = do_ordinary_PIT(P, Sys.BRACKETS, Sys.RATES, Sys.nb);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Cincome2022(SEXP x, SEXP y, SEXP rb, SEXP age, SEXP isMarried, SEXP nDependants,
                 SEXP nthreads) {
  // Retain this experimental entry point's fixed year while sharing the
  // assessment kernel, so its offset and Medicare rules cannot diverge.
  const System Sys = yr2System(2021);
  const R_xlen_t N = xlength(x);
  isEquiInt(x, age, "age");
  isEquiInt(x, isMarried, "is_married");
  SEXP year = PROTECT(ScalarInteger(2021));
  SEXP summary = PROTECT(ScalarInteger(0));
  SEXP claim = PROTECT(allocVector(RAWSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    RAW(claim)[i] = INTEGER(age)[i] >= Sys.S.pension_age ?
      (INTEGER(isMarried)[i] ? SAPTO_D : SAPTO_A) : 0;
  }
  SEXP ans = Cincome_tax(year, x, rb, age, isMarried, nDependants, y, y,
                        claim, R_NilValue, R_NilValue, R_NilValue, x, y, R_NilValue, nthreads, summary);
  UNPROTECT(3);
  return ans;
}

SEXP Cdo_medicare_levy(SEXP x, SEXP Year, SEXP y, SEXP Eligible, SEXP IsMarried, SEXP nDependants) {
  R_xlen_t N = xlength(x);
  isEquiInt(x, y, "Spouse_income");
  isEquiInt(x, Eligible, "sapto.eligible");
  isEquiInt(x, IsMarried, "is_married");
  isEquiInt(x, nDependants, "n_dependants");
  const R_xlen_t n_years = xlength(Year);
  if (!isInteger(Year) || (n_years != 1 && n_years != N)) {
    error("fy.year must have length one or match income.");
  }
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);
  const int * ep = INTEGER(Eligible);
  const int * mp = INTEGER(IsMarried);
  const int * cp = INTEGER(nDependants);
  const int * year = INTEGER(Year);
  const bool constant_year = n_years == 1;
  Medicare schedules[MAX_YEAR - MIN_YEAR + 1];
  bool loaded[MAX_YEAR - MIN_YEAR + 1] = {false};
  // Resolve each schedule and report invalid inputs on the R thread.
  for (R_xlen_t i = 0; i < n_years; ++i) {
    int yr = year[i];
    if (yr < MIN_YEAR || yr > MAX_YEAR) {
      error("Unsupported Medicare year %d.", yr);
    }
    int j = yr - MIN_YEAR;
    if (!loaded[j]) {
      schedules[j] = yr2Medicare(yr);
      loaded[j] = true;
    }
  }
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  int nThread = 1;
  
  FORLOOP({
    const Medicare M = schedules[year[constant_year ? 0 : i] - MIN_YEAR];
    Person P = {0};
    P.xi = xp[i];
    P.yi = yp[i];
    P.yti = imax(yp[i], 0);
    P.ri = xp[i];
    P.n_child = cp[i];
    P.is_married = mp[i] || P.yti > 0;
    P.is_family = P.n_child || P.is_married;
    // This API supplies the taxpayer's entitlement explicitly, but has no
    // spouse entitlement input: use the spouse's ordinary section 7 limit.
    ansp[i] = do_1_ML(P, M, ep[i] != 0, false);
  })
    UNPROTECT(1);
  return ans;
}




