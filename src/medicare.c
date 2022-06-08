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
  validate_lwr_upr(&(M->lwr_family_sapto), &(M->upr_family_sapto), &(M->rate), &(M->taper), "medicare_levy_upper_sapto_family_threshold", fix, yr);
  
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




