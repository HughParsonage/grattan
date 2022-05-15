#include "grattan.h"

SEXP COffset(SEXP x, double y, double a, double m) {
  R_xlen_t N = xlength(x);
  double b = y / m + a;
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * ansp = REAL(ans);
  if (isReal(x)) {
    const double * xp = REAL(x);
    for (int i = 0; i < N; ++i) {
      double xi = xp[i];
      if (xi > b) {
        ansp[i] = 0;
      } else {
        if (xi < a) {
          ansp[i] = y;
        } else {
          ansp[i] = m * (b - xi);
        }
      }
    }
  } else {
    const int * xp = INTEGER(x);
    for (int i = 0; i < N; ++i) {
      double xi = xp[i];
      if (xi > b) {
        ansp[i] = 0;
      } else {
        if (xi < a) {
          ansp[i] = y;
        } else {
          ansp[i] = m * (b - xi);
        }
      }
    }
  }
  UNPROTECT(1);
  return ans;
} 

void apply_offset1(double * tax, int x, Offset1 O) {
  double y = O.offset_1st;
  int b1 = O.thresh_1st;
  double r1 = O.taper_1st;
  double offset =  (x < b1) ? y : dmax0(y + r1 * (x - b1));
  
  if (*tax > offset || O.refundable) {
    *tax -= offset;
  } else {
    *tax = 0;
  }
}

void apply_offset2(double * tax, int x, Offset2 O) {
  double y = O.offset_1st;
  double offset = y;
  int b1 = O.thresh_1st;
  int b2 = O.thresh_2nd;
  double r1 = O.taper_1st;
  double r2 = O.taper_2nd;
      
  if (x > b1) {
    if (x < b2) {
      offset += r1 * (x - b1);
    } else {
      offset += r1 * (b2 - b1) + r2 * (x - b2);
    }
  }
  if (offset <= 0) {
    return;
  }
  
  if (*tax <= offset) {
    *tax = 0;
  } else {
    *tax -= offset;
  }
}

void apply_offsetn(double * tax, Person P, OffsetN O) {
  int nb = O.nb;
  double y = O.offset_1st;
  int xi = P.xi;
  for (int b = 0; b < nb; ++b) {
    if (xi < O.Thresholds[b]) {
      break;
    }
    double taper = O.Tapers[b];
    double above_threshold = xi - O.Thresholds[b];
    if ((b + 1) < nb) {
      double next_threshold = O.Thresholds[b + 1];
      if (xi > next_threshold) {
        above_threshold = next_threshold - O.Thresholds[b];
      }
    }
    y += above_threshold * taper;
  }
  if (!O.refundable && y < 0) {
    y = 0;
  }
  if (*tax <= y) {
    *tax = 0;
  } else {
    *tax -= y;
  }
}



