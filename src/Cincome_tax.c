#include "grattan.h"

static void apply_lito1(double * taxi, int x, double y, double b1, double r1) {
  double lito = (x < b1) ? y : dmax0(y + r1 * (x - b1));
  if (*taxi <= lito) {
    *taxi = 0;
  } else {
    *taxi -= lito;
  }
}

static void apply_lito2(double * taxi, int x, int y, double b1, double r1, double b2, double r2) {
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
  return dmax0(out);
}

static double do_ordinary_PIT_pre(int x, int * bracks, double * tax_at, double * rate, int nb) {
  int w = 0;
  for (int b = 0; b < nb; ++b) {
    w += x > bracks[b];
  }
  return tax_at[w] + rate[w] * (x - bracks[w]);
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

static void do_ordinary_PITs5(double * restrict ansp, R_xlen_t N, 
                              const int * xp,
                              int BRACKS[5], double RATES[5], int nThread) {
  const double R4 = RATES[4];
  FORLOOP({
    ansp[i] = 0;
    int xi = xp[i];
    if (xi <= BRACKS[1]) {
      continue;
    }
    for (int t = 1; t < 5; ++t) {
      int t0 = BRACKS[t - 1];
      int t1 = BRACKS[t];
      double r0 = RATES[t - 1];
      if (xi < t1) {
        ansp[i] += r0 * (xi - t0);
        break;
      } else {
        ansp[i] += r0 * (t1 - t0);
        if (t == 4) {
          ansp[i] += R4 * (xi - t1);
        }
      }
    }
  })
}

static void do_ordinary_PITs(double * restrict ansp, R_xlen_t N, const int * xp, System Sys, int nThread) {
  if (Sys.nb != 5) {
    return;
  }
  int BRACKS[5] = {Sys.BRACKETS[0], Sys.BRACKETS[1], Sys.BRACKETS[2], Sys.BRACKETS[3], Sys.BRACKETS[4]};
  double RATES[5] = {Sys.RATES[0], Sys.RATES[1], Sys.RATES[2], Sys.RATES[3], Sys.RATES[4]};
  do_ordinary_PITs5(ansp, N, xp, BRACKS, RATES, nThread);
}

static double do_1_ML(const Person P, const Medicare M) {
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
      double o1 = dmax0(M.taper * (family_income - lower_family_threshold));
      double o2 = M.rate * family_income;
      double o = (o1 < o2) ? o1 : o2;
      return income_share * o;
    }
  }
  
  double o1 = M.taper * (P.xi - lower_threshold);
  double o2 = M.rate * P.xi;
  return (o1 < o2) ? o1 : o2;
}



void apply_lmito(double * taxi, int x) {
  double lmito = do_1_lmito(x);
  if (lmito >= *taxi) {
    *taxi = 0;
  } else {
    *taxi -= lmito;
  }
}


static void tax(double * taxi, Person P, const System Sys) {
  // if (Sys.has_sapto) {
  //   apply_sapto(taxi, P, Sys.S);
  // }
  // if (Sys.has_offset1) {
  //   apply_offset1(taxi, P, Sys.O1);
  // }
  // if (Sys.has_offset2) {
  //   apply_offset2(taxi, P, Sys.O2);
  // }
  // if (Sys.has_lmito) {
  //   apply_lmito(taxi, P.xi);
  // }
  // if (Sys.has_lito) {
  //   apply_lito(taxi, P, Sys.yr);
  // }
  // 
  // *taxi += do_1_ML(P, Sys.M);
  // 
  // if (Sys.has_temp_budget_repair_levy && P.xi >= TEMP_BUDGET_REPAIR_LEVY_THRESH) {
  //   *taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * (P.xi - TEMP_BUDGET_REPAIR_LEVY_THRESH);
  // }
  
  
}

int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
}

inline Person pp(int x, int y, int r, unsigned int age, bool is_married, int n_child) {
  const Person P = { .xi = x, .yi = y, .ri = r, .agei = age, .is_married = is_married, .n_child = n_child, .is_family = n_child || is_married || y };
  return P;
}



SEXP Cincome_tax(SEXP Yr,
                 SEXP IcTaxableIncome,
                 SEXP RebateIncome,
                 SEXP Age, 
                 SEXP IsMarried,
                 SEXP nDependants,
                 SEXP SpcRebateIncome, 
                 SEXP RSystem,
                 SEXP nthreads) {
  if (xlength(Yr) != 1) {
    error("Yr must be length-one."); // # nocov
  }
  if (!isNull(RSystem) && !isVectorList(RSystem)) {
    error("RSystem must be NULL or a list.");
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(IcTaxableIncome);
  isEquiInt(IcTaxableIncome, Age, "Age");
  isEquiInt(IcTaxableIncome, RebateIncome, "RebateIncome");
  isEquiInt(IcTaxableIncome, IsMarried, "IsMarried");
  isEquiInt(IcTaxableIncome, nDependants, "nDependants");
  isEquiInt(IcTaxableIncome, SpcRebateIncome, "SpcRebateIncome");
  const int yr = asInteger(Yr);
  const int * ic_taxable_income_loss = INTEGER(IcTaxableIncome);
  const int * rebate_income = INTEGER(RebateIncome);
  const int * spc_rebate_income = INTEGER(SpcRebateIncome);
  const int * c_age_30_june = INTEGER(Age);
  const int * is_married = INTEGER(IsMarried);
  const int * n_dependants = INTEGER(nDependants);
  
  const System Sys = Sexp2System(RSystem, yr);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  
  
  if (Sys.has_sapto) {
    FORLOOP({
      int xpi = ic_taxable_income_loss[i];
      int ypi = spc_rebate_income[i];
      unsigned int api = c_age_30_june[i] & 127;
      int rpi = rebate_income[i];
      unsigned int cpi = n_dependants[i] & 15;
      bool is_marriedi = is_married[i];
      bool is_familyi = is_married || cpi || ypi;
      const Person P = pp(xpi, ypi, rpi, api, is_marriedi, cpi);
      ansp[i] = do_ordinary_PIT(P, Sys.BRACKETS, Sys.RATES, Sys.nb);
      apply_sapto(&ansp[i], P, Sys.S);
    })
  } else {
    FORLOOP({
      int xpi = ic_taxable_income_loss[i];
      int ypi = spc_rebate_income[i];
      unsigned int api = c_age_30_june[i] & 127;
      int rpi = rebate_income[i];
      unsigned int cpi = n_dependants[i] & 15;
      bool is_marriedi = is_married[i];
      bool is_familyi = is_married || cpi || ypi;
      const Person P = pp(xpi, ypi, rpi, api, is_marriedi, cpi);
      ansp[i] = do_ordinary_PIT(P, Sys.BRACKETS, Sys.RATES, Sys.nb);
      // no sapto
    })
  }
  if (Sys.has_offset1) {
    FORLOOP({
      int xpi = ic_taxable_income_loss[i];
      apply_offset1(&ansp[i], xpi, Sys.O1);
    })
    
  }
  if (Sys.has_offset2) {
    FORLOOP({
      int xpi = ic_taxable_income_loss[i];
      apply_offset2(&ansp[i], xpi, Sys.O2);
    })
  }
  if (Sys.has_lmito) {
    FORLOOP({
      int xpi = ic_taxable_income_loss[i];
      apply_lmito(&ansp[i], xpi);
    })
    
  }
  if (Sys.has_lito) {
    FORLOOP({
      int xpi = ic_taxable_income_loss[i];
      apply_lito(&ansp[i], xpi, yr);
    })
  }
  FORLOOP({
    int xpi = ic_taxable_income_loss[i];
    int ypi = spc_rebate_income[i];
    unsigned int api = c_age_30_june[i] & 127;
    int rpi = rebate_income[i];
    unsigned int cpi = n_dependants[i] & 15;
    bool is_marriedi = is_married[i];
    bool is_familyi = is_married || cpi || ypi;
    const Person P = pp(xpi, ypi, rpi, api, is_marriedi, cpi);
    ansp[i] += do_1_ML(P, Sys.M);
  })
    
    
    UNPROTECT(1);
  return ans;
}



SEXP Cincome2022(SEXP x, SEXP y, SEXP rb, SEXP age, SEXP isMarried, SEXP nDependants,
                 SEXP nthreads) {
  
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);
  const int * rp = INTEGER(rb);
  const int * ap = INTEGER(age);
  const int * mp = INTEGER(isMarried);
  const int * cp = INTEGER(nDependants);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  const System Sys = yr2System(2021);
  
  FORLOOP({
    Person P;
    P.xi = xp[i];
    P.yi = yp[i];
    P.ri = rp[i];
    P.agei = ap[i];
    P.is_married = mp[i];
    P.n_child = cp[i];
    P.is_family = P.is_married || P.n_child || P.yi;
    ansp[i] = 0;
    double o = 0;
    int xpi = xp[i];
    int api = ap[i];
    if (P.xi <= 18200 || (api >= 65 && xpi <= 32279)) {
      continue;
    }
    if (xpi <= 37000) {
      o += 0.19 * (xpi - 18200);
    } else {
      o += 3572;
      if (xpi <= 90000) {
        o += 0.325 * (xpi - 37000);
      } else {
        o += 17225;
        if (xpi <= 180000) {
          o += 0.37 * (xpi - 90000);
        } else {
          o += 33300;
          o += 0.45 * (xpi - 180000);
        }
      }
    }
    
    double lmitoi = do_1_lmito(xpi);
    o -= lmitoi;
    
    if (xpi <= 66667) {
      double litoi = 700;
      if (xpi > 37500) {
        if (xpi <= 45000) {
          litoi -= 0.05 * (xpi - 37500);
        } else {
          litoi = 325 - 0.015 * (xpi - 45000);
        }
      }
      o -= litoi;
    }
    if (o < 0) {
      o = 0;
    }
    
    if (xpi <= 22801) {
      ansp[i] = o;
      continue;
    }
    
    if (false) {
      int ypi = yp[i];
      double o2 = 0.02 * xpi;
      if (Sys.has_sapto && api >= 65 && xpi <= 50119) {
        double sapto = (2230 - 0.125 * (xpi - 32279));
        o -= sapto;
        if (o < 0) {
          o = 0;
        }
        if (xpi > 36056) {
          double o1 = 0.1 * (xpi - 36056);
          o += (o1 < o2) ? o1 : o2;
        }
        ansp[i] = o;
        continue;
      }
      if (xpi > 22801) {
        double o1 = 0.1 * (xpi - 22801);
        o += (o1 < o2) ? o1 : o2;
      }
    } else {
      o += do_1_ML(P, Sys.M);
    }
    ansp[i] = o;
    
    
    
  })
    UNPROTECT(1);
  return ans;
  
}

SEXP Cdo_medicare_levy(SEXP x, SEXP Year, SEXP y, SEXP Eligible, SEXP IsMarried, SEXP nDependants) {
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);
  const int * ep = INTEGER(Eligible);
  const int * mp = INTEGER(IsMarried);
  const int * cp = INTEGER(nDependants);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  int nThread = 1;
  int yr = asInteger(Year);
  const Medicare M = yr2Medicare(yr);
  
  FORLOOP({
    ansp[i] = 0;
    
    Person P;
    P.xi = xp[i];
    P.yi = yp[i]; 
    P.ri = xp[i];
    P.n_child = cp[i];
    P.is_married = mp[i];
    P.is_family = P.n_child || P.yi || P.is_married;
    P.agei = ep[i] ? 70 : 42;
    ansp[i] = do_1_ML((const Person)P, M);
  })
    UNPROTECT(1);
  return ans;
}




