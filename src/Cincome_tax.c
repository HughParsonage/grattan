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


static double tax(Person P, System Sys) {
  double taxi = do_ordinary_PIT(P, Sys.BRACKETS, Sys.RATES, Sys.nb);
  
  if (Sys.has_sapto) {
    apply_sapto(&taxi, P, Sys.S);
  }
  if (Sys.has_offset1) {
    apply_offset1(&taxi, P, Sys.O1);
  }
  if (Sys.has_offset2) {
    apply_offset2(&taxi, P, Sys.O2);
  }
  if (Sys.has_lmito) {
    apply_lmito(&taxi, P.xi);
  }
  if (Sys.has_lito) {
    apply_lito(&taxi, P, Sys.yr);
  }
  
  taxi += do_1_ML(P, Sys.M);
  
  if (Sys.has_temp_budget_repair_levy && P.xi >= TEMP_BUDGET_REPAIR_LEVY_THRESH) {
    taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * (P.xi - TEMP_BUDGET_REPAIR_LEVY_THRESH);
  }

  return taxi;
}

int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
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
  if (!isNull(RSystem) && !isList(RSystem)) {
    error("RSystem must be NULL or a list.");
  }
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(IcTaxableIncome);
  isEquiInt(IcTaxableIncome, Age);
  isEquiInt(IcTaxableIncome, RebateIncome);
  isEquiInt(IcTaxableIncome, IsMarried);
  isEquiInt(IcTaxableIncome, nDependants);
  isEquiInt(IcTaxableIncome, SpcRebateIncome);
  const int yr = asInteger(Yr);
  const int * ic_taxable_income_loss = INTEGER(IcTaxableIncome);
  const int * rebate_income = INTEGER(RebateIncome);
  const int * spc_rebate_income = INTEGER(SpcRebateIncome);
  const int * c_age_30_june = INTEGER(Age);
  const int * is_married = INTEGER(IsMarried);
  const int * n_dependants = INTEGER(nDependants);
  
  System Sys = Sexp2System(RSystem, yr);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * ansp = REAL(ans);
  

FORLOOP({
    Person P;
    P.xi = ic_taxable_income_loss[i];
    P.yi = c0(spc_rebate_income[i]);
    P.ri = rebate_income[i];
    P.agei = c_age_30_june[i];
    P.is_married = is_married[i];
    P.n_child = n_dependants[i];
    P.is_family = P.is_married || P.n_child || P.yi;
    
    ansp[i] = tax(P, Sys);
    
})
  UNPROTECT(1);
  return ans;
}



SEXP Cincome2022(SEXP x, SEXP y, SEXP age, SEXP isMarried, SEXP nDependants,
                 SEXP nthreads) {
  
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  const int * yp = INTEGER(y);
  const int * ap = INTEGER(age);
  const int * mp = INTEGER(isMarried);
  const int * cp = INTEGER(nDependants);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  
  FORLOOP({
    Person P;
    P.xi = xp[i];
    P.yi = yp[i];
    P.ri = xp[i];
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
    
    int ypi = yp[i];
    double o2 = 0.02 * xpi;
    if (api >= 65 && xpi <= 50119) {
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
    ansp[i] = o;
    
    
    
  })
  UNPROTECT(1);
  return ans;
  
}




