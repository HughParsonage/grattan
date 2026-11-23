#include "grattan.h"

// lito1 ~=> single taper lito
// lito2 ~=> double taper lito

bool has_lito(int yr) {
  return yr >= 1994;
}

bool has_lmito(int yr) {
  return yr >= 2019 && yr <= 2022;
}

int max_lito1(int yr) {
  if (yr >= 2013) {
    return 445;
  }
  if (yr >= 2011) {
    return 1500;
  }
  if (yr <= 2003) {
    return 150;
  }
  if (yr <= 2006) {
    return 235;
  }
  if (yr == 2007) {
    return 600;
  }
  if (yr == 2008) {
    return 750;
  }
  if (yr == 2009) {
    return 1200;
  }
  return 0; // unknown
}

int threshold_lito1(int yr) {
  if (yr >= 2013) {
    return 37000;
  }
  if (yr <= 2002) {
    return 20700;
  }
  if (yr >= 2008) {
    return 30000;
  }
  if (yr == 2007) {
    return 25000;
  }
  if (yr >= 2004) {
    return 21600;
  }
  if (yr == 2003) {
    return 20700;
  }
  return 0;
}

// void apply_lito(double * tax, int x, int yr) {
//   if (yr < 1994) {
//     return;
//   }
//   
//   if (yr < 2021) {
//     Offset1 O;
//     O.taper_1st = yr < 2013 ? -0.04 : -0.015;
//     O.offset_1st = max_lito1(yr);
//     O.refundable = false;
//     O.thresh_1st = threshold_lito1(yr);
//     apply_offset1(tax, x, O);
//     return;
//   }
//   
//   Offset2 O;
//   O.offset_1st = 700;
//   O.refundable = false;
//   O.taper_1st = -0.05;
//   O.taper_2nd = -0.015;
//   O.thresh_1st = 37500;
//   O.thresh_2nd = 45000;
//   apply_offset2(tax, x, O);
// }

Offset1 LITO_ante2020(int yr) {
  Offset1 O;
  O.taper_1st = yr < 2013 ? -0.04 : -0.015;
  O.offset_1st = max_lito1(yr);
  O.refundable = false;
  O.thresh_1st = threshold_lito1(yr);
  return O;
}

Offset2 LITO_post2020(int yr) {
  Offset2 O;
  O.offset_1st = 700;
  O.refundable = false;
  O.taper_1st = -0.05;
  O.taper_2nd = -0.015;
  O.thresh_1st = 37500;
  O.thresh_2nd = 66667;
  return O;
}

static OffsetN LITO_Par(int yr) {
  System Sys = yr2System(yr);
  return Sys.Offsets[0];
}

static OffsetN LMITO_Par(int yr) {
  System Sys = yr2System(yr);
  return Sys.Offsets[1];
}

SEXP C_lito(SEXP x, SEXP Year, SEXP doLmito) {
  if (!isInteger(x) && !isReal(x)) {
    error("`x` was type '%s' but must be numeric.", type2char(TYPEOF(x)));
  }
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  if (!isInteger(Year) || (xlength(Year) != 1 && xlength(Year) != N)) {
    error("fy.year must have length one or the number of incomes.");
  }
  const int *years = INTEGER(Year);
  bool single_year = xlength(Year) == 1;
  bool lmito = asInteger(doLmito);
  const int *ix = isInteger(x) ? INTEGER(x) : NULL;
  const double *dx = isReal(x) ? REAL(x) : NULL;
  OffsetN schedules[MAX_YEAR - MIN_YEAR + 1];
  bool loaded[MAX_YEAR - MIN_YEAR + 1] = {false};
  for (R_xlen_t i = 0; i < N; ++i) {
    int yr = years[single_year ? 0 : i];
    if (yr == NA_INTEGER) {
      error("Missing financial year.");
    }
    if (lmito ? !has_lmito(yr) : !has_lito(yr)) {
      ansp[i] = 0;
      continue;
    }
    if (yr < MIN_YEAR || yr > MAX_YEAR) {
      error("Unsupported offset year %d.", yr);
    }
    int j = yr - MIN_YEAR;
    if (!loaded[j]) {
      schedules[j] = lmito ? LMITO_Par(yr) : LITO_Par(yr);
      loaded[j] = true;
    }
    if (ix) {
      ansp[i] = ix[i] < 0 && ix[i] != NA_INTEGER ? 0 : value_OffsetN(ix[i], schedules[j]);
    } else if (ISNAN(dx[i])) {
      ansp[i] = NA_REAL;
    } else {
      ansp[i] = dx[i] >= 0 && dx[i] < INT_MAX ? value_OffsetN((int)dx[i], schedules[j]) : 0;
    }
  }
  UNPROTECT(1);
  return ans;
}


