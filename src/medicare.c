#include "grattan.h"



Medicare yr2Medicare(int yr) {
  System Sys = yr2System(yr);
  return Sys.M;
}

double ml_rate(int yr) {
  System Sys = yr2System(yr);
  return Sys.M.rate;
}

double ml_taper(int yr) {
  System Sys = yr2System(yr);
  return Sys.M.taper;
}

int ml_lower_thresh(int yr, bool family, bool sapto) {
  System Sys = yr2System(yr);
  if (family) {
    return sapto ? Sys.M.lwr_family_sapto : Sys.M.lwr_family;
  }
  return sapto ? Sys.M.lwr_single_sapto : Sys.M.lwr_single;
}

int ml_upper_thresh(int yr, bool family, bool sapto) {
  System Sys = yr2System(yr);
  if (family) {
    return sapto ? Sys.M.upr_family_sapto : Sys.M.upr_family;
  }
  return sapto ? Sys.M.upr_single_sapto : Sys.M.upr_single;
}

SEXP C_ml_rate(SEXP Yr) {
  int yr = asInteger(Yr);
  return ScalarReal(ml_rate(yr));
}

SEXP C_ml_taper(SEXP Yr) {
  int yr = asInteger(Yr);
  return ScalarReal(ml_taper(yr));
}

SEXP C_ml_lower_thresh(SEXP Yr, SEXP Family, SEXP Sapto) {
  int yr = asInteger(Yr);
  bool family = asLogical(Family);
  bool sapto = asLogical(Sapto);
  return ScalarInteger(ml_lower_thresh(yr, family, sapto));
}

SEXP C_ml_upper_thresh(SEXP Yr, SEXP Family, SEXP Sapto) {
  int yr = asInteger(Yr);
  bool family = asLogical(Family);
  bool sapto = asLogical(Sapto);
  return ScalarInteger(ml_upper_thresh(yr, family, sapto));
}

SEXP Cml_child(SEXP Yr) {
  int yr = asInteger(Yr);
  System Sys = yr2System(yr);
  return ScalarInteger(Sys.M.lwr_thr_up_per_child);
}

static void validate_lwr_upr(int * lwr, int * upr, double * r, double * t, const char * str, int fix, int yr) {
  double ratio = t[0]/(t[0] - r[0]);
  int exp_upr = ceil(lwr[0] * ratio);
  int dxp_upr = upr[0] - exp_upr;
  int delta = abs(dxp_upr);
  if (delta > 1) {
    if (fix == 0) {
      error("`%s = %d`, but %d was expected", str, upr[0], exp_upr);
    }
    // don't want to warn on small difference
    if (fix == 1 && delta > 10) {
      warning("`%s = %d`, but %d was expected and so will be set as such", str, upr[0], exp_upr);
    }
    upr[0] = exp_upr;
  }
  
}


void validate_medicare(Medicare * M, int fix, int yr) {
  validate_lwr_upr(&(M->lwr_single), &(M->upr_single), &(M->rate), &(M->taper), "medicare_levy_upper_threshold", fix, yr);
  validate_lwr_upr(&(M->lwr_single_sapto), &(M->upr_single_sapto), &(M->rate), &(M->taper), "medicare_levy_upper_sapto_threshold", fix, yr);
  validate_lwr_upr(&(M->lwr_family), &(M->upr_family), &(M->rate), &(M->taper), "medicare_levy_upper_family_threshold", fix, yr);
  validate_lwr_upr(&(M->lwr_family_sapto), &(M->upr_family_sapto), &(M->rate), &(M->taper), "medicare_levy_upper_family_sapto_threshold", fix, yr);
  
}

void print_Medicare(Medicare M) {
  Rprintf("Medicare:\n");
  Rprintf("\t%d\n", M.lwr_single);
  Rprintf("\t%d\n", M.upr_single);
  Rprintf("\t%d\n", M.lwr_family);
  Rprintf("\t%d\n", M.upr_family);
  Rprintf("\t%d\n", M.has_sapto_thr);
  Rprintf("\t%d\n", M.sapto_age);
  Rprintf("\t%d\n", M.lwr_single_sapto);
  Rprintf("\t%d\n", M.upr_single_sapto);
  Rprintf("\t%d\n", M.lwr_family_sapto);
  Rprintf("\t%d\n", M.upr_family_sapto);
  Rprintf("\t%d\n", M.lwr_thr_up_per_child);
  Rprintf("\t%f\n", M.taper);
  Rprintf("\t%f\n", M.rate);
}

SEXP Medicare2Sexp(Medicare M) {
  int np = 0;
  SEXP ans = PROTECT(allocVector(VECSXP, MEDICARE_LEN)); ++np;
  SET_VECTOR_ELT(ans, 0, ScalarInteger(M.lwr_single));
  SET_VECTOR_ELT(ans, 1, ScalarInteger(M.upr_single));
  SET_VECTOR_ELT(ans, 2, ScalarInteger(M.lwr_family));
  SET_VECTOR_ELT(ans, 3, ScalarInteger(M.upr_family));
  SET_VECTOR_ELT(ans, 4, ScalarLogical(M.has_sapto_thr));
  SET_VECTOR_ELT(ans, 5, ScalarReal(M.sapto_age));
  SET_VECTOR_ELT(ans, 6, ScalarInteger(M.lwr_single_sapto));
  SET_VECTOR_ELT(ans, 7, ScalarInteger(M.upr_single_sapto));
  SET_VECTOR_ELT(ans, 8, ScalarInteger(M.lwr_family_sapto));
  SET_VECTOR_ELT(ans, 9, ScalarInteger(M.upr_family_sapto));
  SET_VECTOR_ELT(ans, 10, ScalarInteger(M.lwr_thr_up_per_child));
  SET_VECTOR_ELT(ans, 11, ScalarReal(M.taper));
  SET_VECTOR_ELT(ans, 12, ScalarReal(M.rate));
  SEXP nms = PROTECT(allocVector(STRSXP, MEDICARE_LEN)); ++np;
  SET_STRING_ELT(nms, 0, mkCharCE("lower_single_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 1, mkCharCE("upper_single_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 2, mkCharCE("lower_family_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 3, mkCharCE("upper_family_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 4, mkCharCE("has_sapto_thr", CE_UTF8));
  SET_STRING_ELT(nms, 5, mkCharCE("sapto_age", CE_UTF8));
  SET_STRING_ELT(nms, 6, mkCharCE("lower_sapto_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 7, mkCharCE("upper_sapto_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 8, mkCharCE("lower_family_sapto_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 9, mkCharCE("upper_family_sapto_threshold", CE_UTF8));
  SET_STRING_ELT(nms, 10, mkCharCE("lower_up_for_each_child", CE_UTF8));
  SET_STRING_ELT(nms, 11, mkCharCE("taper", CE_UTF8));
  SET_STRING_ELT(nms, 12, mkCharCE("rate", CE_UTF8));
  setAttrib(ans, R_NamesSymbol, nms);
  UNPROTECT(np);
  return ans;
}



