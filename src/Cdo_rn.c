#include "grattan.h"

SEXP Cdo_rn(SEXP x, SEXP along) {
  R_xlen_t N = xlength(along);
  int nThread = 1;
  switch(TYPEOF(x)) {
  case INTSXP: {
    
    if (xlength(x) == N) {
      return x;
    }
    const int x0 = asInteger(x);
    SEXP ans = PROTECT(allocVector(INTSXP, N));
    int * restrict ansp = INTEGER(ans);
    FORLOOP({
      ansp[i] = x0;
    })
    UNPROTECT(1);
    return ans;
  }
    
  }
  return x;
}
