#include "grattan.h"

// must be an integer vector, without NAs, same length as along
// if double, compress range


static R_xlen_t first_NA(const int * xp, R_xlen_t N, int nThread) {
  if (N < 1000) {
    for (int i = 0; i < N; ++i) {
      if (xp[i] == NA_INTEGER) {
        return i+ 1;
      }
    }
    return 0;
  }
  for (int i = 0; i < 1000; ++i) {
    if (xp[i] == NA_INTEGER) {
      return i + 1;
    }
  }
  R_xlen_t j = N + 1;
#if defined _OPENMP && _OPENMP >= 201511 
#pragma omp parallel for reduction(min : j) schedule(static)
#endif
  for (R_xlen_t i = N; i >= 1000; --i) {
    if (xp[i] == NA_INTEGER) {
      j = i + 1;
    }
  }
  if (j == N + 1) {
    return 0;
  }
  return j;
}

SEXP Cdo_rn_int1(const int xp, R_xlen_t N, int nThread, SEXP along) {
  if (xp == 0 || xp == NA_INTEGER) {
    return along;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    ansp[i] = xp;
  })
  UNPROTECT(1);
  return ans;
}

SEXP Cdo_rn_intN(const int * xp, R_xlen_t N, int nThread, SEXP along) {
  R_xlen_t j = first_NA(xp, N, nThread);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    if (i >= j && xp[i] == NA_INTEGER) {
      ansp[i] = 0;
      continue;
    }
    ansp[i] = xp[i];
  })
    UNPROTECT(1);
  return ans;
}

static int clamp(const double xp) {
  if (xp <= -INT_MAX) {
    return -INT_MAX;
  }
  if (xp >= INT_MAX) {
    return INT_MAX;
  }
  return (int)xp;
}

SEXP Cdo_rn_dbl1(const double xp, R_xlen_t N, int nThread, SEXP along) {
  
  if (xp == 0 || ISNAN(xp)) {
    return along;
  }
  const int xxp = clamp(xp);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    ansp[i] = xxp;
  })
  UNPROTECT(1);
  return ans;
}

SEXP Cdo_rn_dbl(const double * xp, R_xlen_t N, int nThread, SEXP along) {
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    double xpi = xp[i];
    if (ISNAN(xpi)) {
      ansp[i] = 0;
      continue;
    }
    if (xpi > INT_MAX) {
      ansp[i] = INT_MAX;
      continue;
    }
    if (xpi < -INT_MAX) {
      ansp[i] = -INT_MAX;
      continue;
    }
    ansp[i] = xpi;
  })
  UNPROTECT(1);
  return ans;
}

// Ensures x is the same length as along
SEXP Cdo_rn(SEXP x, SEXP along, SEXP nthreads) {
  R_xlen_t N = xlength(along);
  if (xlength(x) != N && xlength(x) != 1) {
    return along;
  }
  int nThread = as_nThread(nthreads);
  
  switch(TYPEOF(x)) {
  case INTSXP: 
    if (xlength(x) == 1) {
      return Cdo_rn_int1(asInteger(x), N, nThread, along);
    } else {
      return Cdo_rn_intN(INTEGER(x), N, nThread, along);
    }
    break;
  case REALSXP: 
    if (xlength(x) == 1) {
      return Cdo_rn_dbl1(asReal(x), N, nThread, along);
    } else {
      return Cdo_rn_dbl(REAL(x), xlength(x), nThread, along);
    }
  default:
      return along;
  }
  return along;
}
