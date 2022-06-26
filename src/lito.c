#include "grattan.h"

// lito1 ~=> single taper lito
// lito2 ~=> double taper lito

bool has_lito(int yr) {
  return yr >= 1994;
}

bool has_lmito(int yr) {
  return yr >= 2019;
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
    return 18575;
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
  int yr = asInteger(Year);
  const bool do_lmito = asInteger(doLmito);
  bool offset_applies = do_lmito ? has_lmito(yr) : has_lito(yr);
  if (!offset_applies) {
    memset(ansp, 0, sizeof(double) * N);
    UNPROTECT(1);
    return ans;
  }
  OffsetN O = do_lmito ? LMITO_Par(yr) : LITO_Par(yr);
  if (isInteger(x)) {
    const int * xp = INTEGER(x);
    // double value_OffsetN(int x, const OffsetN O)
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = value_OffsetN(xp[i], O);
    }
  } else {
    const double * xp = REAL(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      ansp[i] = 0;
      if (xp[i] > 0 && xp[i] < INT_MAX) {
        ansp[i] = value_OffsetN(xp[i], O);
      }
    }
  }
  UNPROTECT(1);
  return ans;
}


