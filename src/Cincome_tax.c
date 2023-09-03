#include "grattan.h"

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




static double do_1_ML(const Person P, const Medicare M) {
  bool sapto = P.agei >= 65;
  double lower_threshold = sapto ? M.lwr_single_sapto : M.lwr_single;
  if (P.xi < lower_threshold) {
    return 0;
  }
  if (P.is_family) {
    
    // subs.8(5) of Act
    unsigned int lower_family_threshold = (sapto ? M.lwr_family_sapto : M.lwr_family) + P.n_child * M.lwr_thr_up_per_child;
    unsigned int upper_family_threshold = lower_family_threshold + (lower_family_threshold >> 2);
    unsigned int family_income = P.xi + (unsigned int)P.yi;
    if (family_income <= lower_family_threshold) {
      return 0;
    }
    // # Levy in the case of small incomes (s.7 of Act)
    if (family_income <= upper_family_threshold) {
      double o1 = dmax0(M.taper * (family_income - lower_family_threshold));
      double o2 = M.rate * family_income;
      double o = (o1 < o2) ? o1 : o2;
      if (P.yi > 0) {
        // The fraction of
        return (o * P.xi) / family_income;
      }
      return o;
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

int c0(int x) {
  return x == NA_INTEGER ? 0 : x;
}

Person pp(int x, int y, int r, unsigned int age, bool is_married, unsigned int n_child) {
  Person P = 
    { .xi = x,
      .yi = y, 
      .ri = r,
      .agei = age,
      .n_child = n_child,
      .on_sapto_cd = 0,
      .is_married = is_married,
      .is_family = n_child || is_married || y };
  return P;
}

static unsigned int sapto_bitwise(unsigned char x) {
  switch(x) {
  case 'A':
    return SAPTO_A;
  case 'B':
    return SAPTO_B;
  case 'C':
    return SAPTO_C;
  case 'D':
    return SAPTO_D;
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
                 SEXP OnSaptoCd,
                 SEXP RSystem,
                 SEXP nthreads) {
  if (xlength(Yr) != 1) {
    error("Yr must be length-one."); // # nocov
  }
  if (!isNull(RSystem) && !isVectorList(RSystem)) {
    error("RSystem was type '%s', but must be NULL or a list.", type2char(TYPEOF(RSystem)));
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
  if (N != xlength(OnSaptoCd)) {
    error("xlength(OnSaptoCd) = %lld, yet N = %lld", xlength(OnSaptoCd), N);
  }
  const unsigned char * on_sapto_cdp = RAW(OnSaptoCd);
  
  const System Sys = Sexp2System(RSystem, yr);
  
  Person * PP = malloc(sizeof(Person) * N);
  if (PP == NULL) {
    return R_NilValue;
  }
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  FORLOOP({
    int xpi = ic_taxable_income_loss[i];
    int ypi = spc_rebate_income[i];
    unsigned int api = c_age_30_june[i] & 127;
    int rpi = rebate_income[i];
    unsigned int cpi = n_dependants[i] & 15;
    bool is_marriedi = is_married[i];
    unsigned char z = on_sapto_cdp[i];
    Person P;
    P.agei = api;
    P.is_family = is_marriedi || cpi || ypi;
    P.is_married = is_marriedi;
    P.n_child = cpi;
    P.on_sapto_cd = z;
    P.ri = rpi;
    P.xi = xpi;
    P.yi = ypi;
    PP[i] = P;
  })
    
  
  FORLOOP({
    ansp[i] = do_ordinary_PIT(PP[i], Sys.BRACKETS, Sys.RATES, Sys.nb);
  })
    
  
  if (Sys.has_sapto) {
    FORLOOP({
      apply_sapto(&ansp[i], PP[i], Sys.S);
    })
  }
  
  
  if (Sys.n_offsetn) {
    do_multiOffsets(ansp, N, Sys.Offsets, Sys.n_offsetn, ic_taxable_income_loss, nThread, true);
  }

  FORLOOP({
    const Person P = PP[i];
    ansp[i] += do_1_ML(P, Sys.M);
  })
  free(PP);
  UNPROTECT(1);
  return ans;
}



SEXP Cincome2022(SEXP x, SEXP y, SEXP rb, SEXP age, SEXP isMarried, SEXP nDependants,
                 SEXP nthreads) {
  
  AS_NTHREAD;
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
    int ypi = yp[i];
    P.xi = xp[i];
    P.yi = ypi;
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
    ansp[i] = do_1_ML(P, M);
  })
    UNPROTECT(1);
  return ans;
}




