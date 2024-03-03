#include "grattan.h"

SEXP C_MAX_N_OFFSETN(SEXP x) {
  return ScalarInteger(MAX_N_OFFSETN);
}

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

double value_OffsetN(int x, const OffsetN O) {
  int nb = O.nb;
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

SEXP nOffsets2List(OffsetN const O[MAX_N_OFFSETN], int noffsets) {
  SEXP ans = PROTECT(allocVector(VECSXP, noffsets));
  for (int i = 0; i < noffsets; ++i) {
    SET_VECTOR_ELT(ans, i, Offsets2List(O[i]));
  }
  UNPROTECT(1);
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
    SEXP x_j = PROTECT(Offsets2List(Sys.Offsets[j]));
    SET_VECTOR_ELT(ans, j, x_j);
    UNPROTECT(1);
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
  // 2024-03-03
  // This was originally VERY CLEVER and used memoization. However, shock horror
  // it was too clever and caused a stack overflow on the Intel compiler for 
  // reasons which I'm not sure about, but which I suspect were to do with an
  // aggressive paralellization.  Tthat was abandoned for this straightforward method.
  
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
}

SEXP C_multiOffset(SEXP x, SEXP OffsetList, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  if (!isInteger(x)) {
    error("(C_multiOffset): `x` was type '%s' but must be type integer.", type2char(TYPEOF(x)));
  }
  int nThread = as_nThread(nthreads);
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




