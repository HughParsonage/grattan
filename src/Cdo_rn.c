#include "grattan.h"

static R_xlen_t isntAsis(const int * xp, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_INTEGER) {
      return i + 1;
    }
  }
  return 0;
}

// Ensures x is the same length as along
SEXP Cdo_rn(SEXP x, SEXP along) {
  R_xlen_t N = xlength(along);
  int nThread = 1;
  switch(TYPEOF(x)) {
  case INTSXP: {
    
    if (xlength(x) != N) {
    if (xlength(x) == 1) {
      const int x0 = asInteger(x);
      if (x0 == 0 || x0 == NA_INTEGER) {
        return along;
      }
      SEXP ans = PROTECT(allocVector(INTSXP, N));
      int * restrict ansp = INTEGER(ans);
      FORLOOP({
        ansp[i] = x0;
      })
        UNPROTECT(1);
      return ans;
    } else {
      return along;
    }
  }
    const int * xp = INTEGER(x);
    R_xlen_t xp_isntAsis = isntAsis(xp, N);
    if (!xp_isntAsis) {
      return x;
    }
    
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (i < xp_isntAsis) {
        ansp[i] = xp[i];
        continue;
      }
      ansp[i] = xp[i] == NA_INTEGER ? 0 : xp[i];
    }
    UNPROTECT(1);
    return ans;
  }
    break;
  case REALSXP: {
    if (xlength(x) == 1) {
      const double x0 = asReal(x);
      if (ISNAN(x0) || x0 == 0 || x0 < INT_MIN || x0 > INT_MAX) {
        return along;
      } else {
        SEXP ans = PROTECT(allocVector(INTSXP, N));
        int * restrict ansp = INTEGER(ans);
        FORLOOP({
          ansp[i] = x0;
        })
        UNPROTECT(1);
        return ans;
      }
      const double * xp = REAL(x);
      SEXP ans = PROTECT(allocVector(INTSXP, N));
      int * restrict ansp = INTEGER(ans);
      FORLOOP({
        double xpi = xp[i];
        ansp[i] = 0;
        if (ISNAN(xpi)) {
          continue;
        }
        if (xpi > INT_MAX) {
          ansp[i] = INT_MAX;
          continue;
        }
        if (xpi <= NA_INTEGER) {
          ansp[i] = -INT_MAX;
          continue;
        }
        ansp[i] = xpi;
      })
      UNPROTECT(1);
      return ans;
    }
  }
    
  }
  return along;
}
