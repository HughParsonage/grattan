#include "grattan.h"

static bool taper_nonzeroish(double x) {
  return x > 1e-8 || x < -1e-8;
}

SEXP Offset(SEXP x, double y, double a, double m) {
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

double value_OffsetN(int x, const OffsetN O) {
  int nb = O.nb;
  if (x < O.Thresholds[0]) {
    return O.offset_1st;
  }
  double y = O.offset_1st;
  for (int t = 0; t < nb; ++t) {
    if (x < O.Thresholds[t]) {
      break;
    }
    if (t == nb - 1 || x < O.Thresholds[t + 1]) {
      y -= O.Tapers[t] * (x - O.Thresholds[t]);
      break;
    }
    y -= O.Tapers[t] * (O.Thresholds[t + 1] - O.Thresholds[t]);
  }
  if (y < 0 && !O.refundable) {
    return 0;
  }
  return y;
}



SEXP COffset(SEXP x, SEXP y, SEXP a, SEXP m) {
  return Offset(x, asReal(y), asReal(a), asReal(m));
}

static int last_positive_offset(const double * tapers, 
                                const int * thresholds, 
                                const int * offset_1st, 
                                int n_tapers) {
  double o = offset_1st[0];
  int j = thresholds[0];
  int t = 0;
  while (o > 0 && j < 999999) {
    o -= tapers[t];
    ++j;
    if (t + 1 < n_tapers) {
      t += j > thresholds[t + 1];
    }
  }
  return j;
}

OffsetN yr2OffsetN(int yr, int j) {
  System SysYr = yr2System(yr);
  return SysYr.Offsets[j];
}

SEXP Offsets2List(OffsetN O) {
  int np = 0;
  SEXP ans = PROTECT(allocVector(VECSXP, 4)); ++np;
  SEXP Offset1st = PROTECT(ScalarInteger(O.offset_1st)); ++np;
  SET_VECTOR_ELT(ans, 0, Offset1st);
  int nb = O.nb;
  SEXP Thresholds = PROTECT(allocVector(INTSXP, nb)); ++np;
  SEXP Tapers = PROTECT(allocVector(REALSXP, nb)); ++np;
  SEXP Refundable = PROTECT(ScalarLogical(O.refundable)); ++np;
  for (int t = 0; t < nb; ++t) {
    INTEGER(Thresholds)[t] = O.Thresholds[t];
    REAL(Tapers)[t] = O.Tapers[t];
  }
  SET_VECTOR_ELT(ans, 1, Thresholds);
  SET_VECTOR_ELT(ans, 2, Tapers);
  SET_VECTOR_ELT(ans, 3, Refundable);
  
  SEXP nms = PROTECT(allocVector(STRSXP, 4)); ++np;
  SET_STRING_ELT(nms, 0, mkCharCE("offset_1st", CE_UTF8));
  SET_STRING_ELT(nms, 1, mkCharCE("thresholds", CE_UTF8));
  SET_STRING_ELT(nms, 2, mkCharCE("tapers", CE_UTF8));
  SET_STRING_ELT(nms, 3, mkCharCE("refundable", CE_UTF8));
  setAttrib(ans, R_NamesSymbol, nms);
  UNPROTECT(np);
  return ans;
}

SEXP C_yr2Offsets(SEXP Yr) {
  int yr = asInteger(Yr);
  System Sys = yr2System(yr);
  int n_offsetn = Sys.n_offsetn;
  SEXP ans = PROTECT(allocVector(VECSXP, n_offsetn));
  if (n_offsetn == 0) {
    UNPROTECT(1);
    return ans;
  }
  for (int j = 0; j < n_offsetn; ++j) {
    SET_VECTOR_ELT(ans, j, Offsets2List(Sys.Offsets[j]));
  }
  UNPROTECT(1);
  return ans;
}


void SEXP2Offset(OffsetN * O, int nO, SEXP List) {
  if (length(List) != nO) {
    warning("length(List) != length(nO)");
    return;
  }
  for (int j = 0; j < nO; ++j) {
    SEXP el = VECTOR_ELT(List, j);
    if (!isVectorList(el)) {
      error("(SEXP2Offset): Element %d of Offset was not a VectorList.", j);
    }
    SEXP jOffset1st = getListElement(el, "offset_1st");
    int j_offset_1st = asInteger(jOffset1st);
    SEXP jThresholds = getListElement(el, "thresholds");
    if (isNull(jThresholds)) {
      error("Element %d lacked an element named 'thresholds'", j);
    }
    if (!isInteger(jThresholds)) {
      error("Element %d of Offset had an element named 'thresholds' but this was not an integer vector", 
            j);
    }
    SEXP jTapers = getListElement(el, "tapers");
    if (!isReal(jTapers)) {
      error("Element %d of Offset had an element named 'tapers' but this was not an double vector", 
            j);
    }
    
    if (length(jTapers) != length(jThresholds)) {
      error("In element %d of Offset, length(Tapers) = %d yet length(Thresholds) = %d. "
              "tapers and thresholds must have the same length.",
              j, length(jTapers), length(jThresholds));
    }
    int nb = length(jThresholds);
    if (nb > MAX_OFFSETN) {
      error("In element %d of Offset, nb = %d, yet the maximum supported number of "
              "offset thresholds is %d", j, nb, MAX_OFFSETN);
    }
    OffsetN Oj;
    Oj.nb = nb;
    Oj.offset_1st = j_offset_1st;
    for (int t = 0; t < MAX_OFFSETN; ++t) {
      int tt = (t < nb) ? t : nb - 1;
      Oj.Thresholds[t] = INTEGER_ELT(jThresholds, tt);
      Oj.Tapers[t] = REAL_ELT(jTapers, tt);
    }
    Oj.refundable = asLogical(getListElement(el, "refundable"));
    O[j] = Oj;
  }
}



SEXP C_moffset(SEXP x, SEXP Offset1st, SEXP Thresholds, SEXP Tapers) {
  int n_tapers = length(Tapers);
  if (n_tapers != length(Offset1st) || n_tapers != length(Thresholds)) {
    error("n_tapers disagree with length(Thresholds) or length(Offset1st)");
  }
  const double * tapers  = REAL(Tapers);
  const int * thresholds = INTEGER(Thresholds);
  const int * offset_1st = INTEGER(Offset1st);
  int final_offset = last_positive_offset(tapers, thresholds, offset_1st, n_tapers);
  float * fmem = malloc(sizeof(float) * (final_offset));
  if (fmem == NULL) {
    return R_NilValue;
  }
  for (int j = 0, t = 0; j < final_offset; ++j) {
    if (j <= thresholds[0]) {
      fmem[j] = offset_1st[0];
      continue;
    }
    fmem[j] = fmem[j - 1] - tapers[t];
    if ((t + 1) < n_tapers) {
      t += j > thresholds[t + 1];
    }
  }
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  switch(TYPEOF(x)) {
  case INTSXP:
  {
    const int * xp = INTEGER(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      unsigned int xpi = xp[i];
      if (xpi >= final_offset) {
        ansp[i] = 0;
        continue;
      }
      ansp[i] = fmem[xpi];
    }
  }
    break;
  case REALSXP:
  {
    const double * xp = REAL(x);
    for (R_xlen_t i = 0; i < N; ++i) {
      if (ISNAN(xp[i]) || xp[i] < 0 || xp[i] >= final_offset) {
        ansp[i] = 0;
        continue;
      }
      unsigned int xpi = xp[i];
      ansp[i] = fmem[xpi];
    }
  }
    
  }
  
  free(fmem);
  UNPROTECT(1);
  return ans;
}

static int nthOffset(OffsetN O, unsigned int j) {
  // how high the offset is at the j'th threshold
  if (j > 32) {
    return nthOffset(O, j & 31);
  }
  if (j == 0) {
    return O.offset_1st;
  }
  int o = O.offset_1st;
  
  for (unsigned int i = 1; i < j; ++i) {
    int d_threshold_i = O.Thresholds[i] - O.Thresholds[i - 1];
    double taper_i = O.Tapers[i - 1];
    if (taper_nonzeroish(taper_i)) {
      o -= d_threshold_i * taper_i;
    }
  }
  return o;
}

static int nOffset_upper_threshold(const OffsetN O) {
  int nb = O.nb;
  int last_offset = nthOffset(O, nb - 1);
  double last_taper = O.Tapers[nb - 1];
  if (!taper_nonzeroish(last_taper)) {
    return INT_MAX - 1;
  }
  return O.Thresholds[nb - 1] + last_offset / last_taper;
}

SEXP Ctest_nOffset_upper_threshold(SEXP OffsetList, SEXP jj) {
  unsigned int j = asInteger(jj);
  --j;
  if (j >= length(OffsetList)) {
    return R_NilValue;
  }
  
  OffsetN * mOffsets = malloc(sizeof(OffsetN) * length(OffsetList));
  if (mOffsets == NULL) {
    error("mOffsets could not be malloc'd"); // # nocov
  }
  SEXP2Offset(mOffsets, length(OffsetList), OffsetList);
  int o = nOffset_upper_threshold(mOffsets[j]);
  free(mOffsets);
  return ScalarInteger(o);
}

static int mOffsets_max_upper_threshold(const OffsetN * mOffsets, int n_offsets) {
  int o = 0;
  for (int j = 0; j < n_offsets; ++j) {
    int j_upper_threshold = nOffset_upper_threshold(mOffsets[j]);
    o = j_upper_threshold > o ? j_upper_threshold : o;
  }
  return o;
}

//' @noRd
//' @description applies multiple offsets
//' @param apply Whether or not to apply the offset to ansp (i.e. subtract from it),
//' as if it were the tax to be offset
void do_multiOffsets(double * restrict ansp,
                     R_xlen_t N,
                     const OffsetN mOffsets[MAX_N_OFFSETN],
                     int n_offsets,
                     const int * xp, 
                     int nThread,
                     bool apply) {
  // For efficiency, we memoize the elements for each x
  // Originally we used a floating pointer but this was risky
  // (since it might provide results sufficiently inaccurate
  // to violate unit tests) and appeared to offer little performance
  // benefit so we just went to double but kept the name 'fmem'.
  
  // n_fmem is the number of memoized elements to allocate
  // it will be equal to the maximum upper threshold among the offsets
  unsigned int n_fmem = mOffsets_max_upper_threshold(mOffsets, n_offsets);
  
  // currently cannot apply refundable and non-refundable offsets 
  bool any_refundable = false;
  bool all_refundable = true;
  for (int j = 0; j < n_offsets; ++j) {
    any_refundable |= mOffsets[j].refundable;
    all_refundable &= mOffsets[j].refundable;
  }
  
  // In addition, if n_fmem is large, memoization is unlikely to provide 
  // benefits (in fact is likely to be an error -- i.e.
  // the offset never gets to zero
  
  bool dont_memoize = 
    n_fmem > 1048576 || (apply && any_refundable && !all_refundable);
  
  // fmem[i] is the (total) offsets for [income] i
  double * fmem = dont_memoize ? NULL : malloc(sizeof(double) * (n_fmem + 1u));
  
  if (fmem == NULL) {
    
    FORLOOP({
      if (apply) {
        for (int j = 0; j < n_offsets; ++j) {
          ansp[i] -= value_OffsetN(xp[i], mOffsets[j]);
          if (ansp[i] < 0 && !mOffsets[j].refundable) {
            ansp[i] = 0;
          }
        }
      } else {
        double o_i = 0;
        for (int j = 0; j < n_offsets; ++j) {
          o_i += value_OffsetN(xp[i], mOffsets[j]);
        }
        ansp[i] = o_i;
      }
    })
    return;
  }
  
  for (int i = 0; i < n_fmem; ++i) {
    double o_i = 0;
    for (int j = 0; j < n_offsets; ++j) {
      o_i += value_OffsetN(i, mOffsets[j]);
    }
    fmem[i] = o_i;
  }

  fmem[n_fmem] = 0;
  if (apply) {
    if (all_refundable) {
      FORLOOP({
        unsigned int xpi = xp[i];
        if (xpi < n_fmem) {
          ansp[i] -= fmem[xpi];
        }
      })
    } else {
      FORLOOP({
        unsigned int xpi = xp[i];
        if (xpi < n_fmem) {
          ansp[i] -= fmem[xpi];
          if (ansp[i] < 0) {
            ansp[i] = 0;
          }
        }
      })
    }
  } else {
    FORLOOP({
      unsigned int xpi = xp[i];
      ansp[i] = xpi < n_fmem ? fmem[xpi] : 0;
    })
  }
  free(fmem);
}

SEXP C_multiOffset(SEXP x, SEXP OffsetList, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  if (!isInteger(x)) {
    error("(C_multiOffset): `x` was type '%s' but must be type integer.", type2char(TYPEOF(x)));
  }
  AS_NTHREAD;
  int n_offsets = length(OffsetList);
  OffsetN * mOffsets = malloc(sizeof(OffsetN) * n_offsets);
  if (mOffsets == NULL) {
    error("mOffsets could not be malloc'd"); // # nocov
  }
  SEXP2Offset(mOffsets, n_offsets, OffsetList);
  
  
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  do_multiOffsets(ansp, N, mOffsets, n_offsets, xp, nThread, false);
  free(mOffsets);
  UNPROTECT(1);
  return ans;
}



typedef struct {
  int dollar;
  unsigned char frac;
} Currency;

Currency add_currency(Currency a, Currency b) {
  int xa = a.dollar;
  int xb = b.dollar;
  unsigned char fa = a.frac;
  unsigned char fb = b.frac;
  
  unsigned char fab = fa + fb;
  int dab = xa + xb + (fab >= 100);
  fab %= 100;
  Currency o = {.dollar = dab, .frac = fab};
  return o;
}

float Currency2float(Currency C) {
  return C.dollar + 0.01 * ((int)C.frac);
}

SEXP TestCurrency(SEXP x, SEXP Taper) {
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  
  unsigned char taper = 100 * asReal(Taper);
  Currency CTaper = {.dollar = 0, .frac = taper};
  ansp[0] = xp[0];
  Currency C2 = {.dollar = xp[0], .frac = 0};
  for (R_xlen_t i = 1; i < N; ++i) {
    C2 = add_currency(C2, CTaper);
    ansp[i] = Currency2float(C2);
  }
  UNPROTECT(1);
  return ans;
  
}





