#include "grattan.h"

const unsigned char ages_by_age_range[12] = {72, 67, 62, 57, 52, 47, 42, 37, 32, 27, 22, 17};

static bool not_age_range(const int * xp, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] > 11) {
      return true;
    }
  }
  return false;
}

SEXP Cdecode_age_range(SEXP x, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  
  if (!isInteger(x)) {
    error("Unsupported type ('%s') to Cdecode_age_range", type2char(TYPEOF(x)));
  }
  const int * xp = INTEGER(x);
  if (not_age_range(xp, N)) {
    return x;
  }
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  FORLOOP({
    ansp[i] = 0;
    unsigned int xpi = xp[i];
    if (xpi < 12) {
      ansp[i] = ages_by_age_range[xpi];
    }
  })
  UNPROTECT(1);
  return ans;
}