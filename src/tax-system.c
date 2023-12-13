#include "grattan.h"

System yr2System(int yr) {
  switch(yr) {
  case 1984:
    return System1984;
  case 1985:
    return System1985;
  case 1986:
    return System1986;
  case 1987:
    return System1987;
  case 1988:
    return System1988;
  case 1989:
    return System1989;
  case 1990:
    return System1990;
  case 1991:
    return System1991;
  case 1992:
    return System1992;
  case 1993:
    return System1993;
  case 1994:
    return System1994;
  case 1995:
    return System1995;
  case 1996:
    return System1996;
  case 1997:
    return System1997;
  case 1998:
    return System1998;
  case 1999:
    return System1999;
  case 2000:
    return System2000;
  case 2001:
    return System2001;
  case 2002:
    return System2002;
  case 2003:
    return System2003;
  case 2004:
    return System2004;
  case 2005:
    return System2005;
  case 2006:
    return System2006;
  case 2007:
    return System2007;
  case 2008:
    return System2008;
  case 2009:
    return System2009;
  case 2010:
    return System2010;
  case 2011:
    return System2011;
  case 2012:
    return System2012;
  case 2013:
    return System2013;
  case 2014:
    return System2014;
  case 2015:
    return System2015;
  case 2016:
    return System2016;
  case 2017:
    return System2017;
  case 2018:
    return System2018;
  case 2019:
    return System2019;
  case 2020:
    return System2020;
  case 2021:
    return System2021;
  case 2022:
    return System2022;
  case 2023:
    return System2023;
  case 2024:
    return System2024;
  }
  return System2024; // # nocov
}

unsigned int nb_by_year(int yr) {
  System Sys = yr2System(yr);
  return Sys.nb;
}

int brack_by_yr(int b, int yr) {
  System Sys = yr2System(yr);
  return Sys.BRACKETS[b % MAX_NBRACK];
}

double rates_by_yr(int b, int yr) {
  System Sys = yr2System(yr);
  return Sys.RATES[b % MAX_NBRACK];
}

SEXP Cbracks_by_year(SEXP Yr, SEXP bb) {
  int yr = asInteger(Yr);
  const int * b = INTEGER(bb);
  int n = length(bb);
  if (n > 8) {
    n = 8;
  }
  
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  for (int i = 0; i < n; ++i) {
    INTEGER(ans)[i] = brack_by_yr((b[i] - 1) % MAX_NBRACK, yr);
  }
  UNPROTECT(1);
  return ans;
}

SEXP Crates_by_yr(SEXP Yr, SEXP bb) {
  int yr = asInteger(Yr);
  const int * b = INTEGER(bb);
  int n = length(bb);
  if (n > 8) {
    n = 8;
  }
  SEXP ans = PROTECT(allocVector(REALSXP, n));
  for (int i = 0; i < n; ++i) {
    REAL(ans)[i] = rates_by_yr((b[i] - 1) % MAX_NBRACK, yr);
  }
  UNPROTECT(1);
  return ans;
}



bool safe2int(double x) {
  return !ISNAN(x) && x > -INT_MAX && x < INT_MAX;
}







void setIntElement(int * o, SEXP list, const char * str) {
  SEXP elmt = getListElement(list, str);
  if (isReal(elmt)) {
    double delmt = asReal(elmt);
    if (!safe2int(delmt)) {
      return;
    }
    *o = (int)delmt;
    return;
  }
  if (isInteger(elmt)) {
    *o = asInteger(elmt);
  }
}

void setIntElements(int * o, int n, SEXP list, const char * str) {
  SEXP elmt = getListElement(list, str);
  int M = length(elmt); // elements to assign
  if (n < M) {
    M = n; // in case there are more elements
  }
  if (isReal(elmt)) {
    const double * xp = REAL(elmt);
    for (int j = 0; j < M; ++j) {
      if (safe2int(xp[j])) {
        o[j] = xp[j];
      }
    }
    return;
  }
  
  if (isInteger(elmt)) {
    const int * xp = INTEGER(elmt);
    for (int j = 0; j < M; ++j) {
      if (xp[j] != NA_INTEGER) {
        o[j] = xp[j];
      }
    }
  }
}

void setDblElement(double * o, SEXP list, const char * str) {
  SEXP elmt = getListElement(list, str);
  if (isReal(elmt)) {
    *o = asReal(elmt);
  }
  if (isInteger(elmt)) {
    *o = (double)asInteger(elmt);
  }
}

void setDblElement2(double * o, SEXP list, const char * str, const char * str2) {
  SEXP list2 = getListElement(list, str);
  if (!isVectorList(list)) {
    return;
  }
  SEXP elmt = getListElement(list2, str2);
  if (isReal(elmt)) {
    *o = asReal(elmt);
  }
  if (isInteger(elmt)) {
    *o = (double)asInteger(elmt);
  }
}

void setDblElements(double * o, int n, SEXP list, const char * str) {
  SEXP elmt = getListElement(list, str);
  int M = length(elmt); // elements to assign
  if (n < M) {
    M = n; // in case there are more elements
  }
  if (isReal(elmt)) {
    const double * xp = REAL(elmt);
    for (int j = 0; j < M; ++j) {
      if (!ISNAN(xp[j])) {
        o[j] = xp[j];
      }
    }
  }
  if (isInteger(elmt)) {
    const int * xp = INTEGER(elmt);
    for (int j = 0; j < M; ++j) {
      if (xp[j] != NA_INTEGER) {
        o[j] = xp[j];
      }
    }
  }
}

int getIntElement(SEXP List, const char * str, int ifnotfound) {
  if (!hazName(List, str)) {
    return ifnotfound;
  }
  return asInteger(getListElement(List, str));
}

double getDblElement(SEXP List, const char * str, double ifnotfound) {
  if (!hazName(List, str)) {
    return ifnotfound;
  }
  return asReal(getListElement(List, str));
}

System Sexp2System(SEXP RSystem, int yr) {
  
  if (isNull(RSystem)) {
    return yr2System(yr);
  }
  
  if (!isVectorList(RSystem)) {
    error("(Sexp2System): RSystem was type '%s' but must be type list",
          type2char(TYPEOF(RSystem)));
  }
  
  int n = length(RSystem);
  System Sys = yr2System(yr);
  if (n == 0) {
    return Sys;
  }
  // Sapto S = yr2Medicare(yr);
  if (hazName(RSystem, "yr")) {
    yr = asInteger(getListElement(RSystem, "yr"));
    Sys.yr = yr;
    // Sys.
  }
  Sys.has_sapto = yr >= 2000;
  
  // tax thresholds
  setIntElements(Sys.BRACKETS, MAX_NBRACK, RSystem, "ordinary_tax_thresholds");
  setDblElements(Sys.RATES, MAX_NBRACK, RSystem, "ordinary_tax_rates");
  
  // Set Medicare levy
  setDblElement(&Sys.M.taper, RSystem, "medicare_levy_taper");
  setDblElement(&Sys.M.rate, RSystem, "medicare_levy_rate");
  
  setIntElement(&Sys.M.lwr_single, RSystem, "medicare_levy_lower_threshold");
  setIntElement(&Sys.M.upr_single, RSystem, "medicare_levy_upper_threshold");
  
  setIntElement(&Sys.M.lwr_single_sapto, RSystem, "medicare_levy_lower_sapto_threshold");
  setIntElement(&Sys.M.upr_single_sapto, RSystem, "medicare_levy_upper_sapto_threshold");
  
  setIntElement(&Sys.M.lwr_family, RSystem, "medicare_levy_lower_family_threshold");
  setIntElement(&Sys.M.upr_family, RSystem, "medicare_levy_upper_family_threshold");
  
  setIntElement(&Sys.M.lwr_family_sapto, RSystem, "medicare_levy_lower_family_sapto_threshold");
  setIntElement(&Sys.M.upr_family_sapto, RSystem, "medicare_levy_upper_family_sapto_threshold");
  
  setIntElement(&Sys.M.lwr_thr_up_per_child, RSystem, "medicare_levy_lower_up_for_each_child");
  
  setIntElement(&Sys.M.sapto_age, RSystem, "sapto_pension_age");
  
  if (hazName(RSystem, "Offsets")) {
    SEXP ROffsets = getListElement(RSystem, "Offsets");
    OffsetN COffsets[MAX_N_OFFSETN] = {0};
    Sys.n_offsetn = length(ROffsets);
    SEXP2Offset(COffsets, length(ROffsets), ROffsets);
    for (int j = 0; j < length(ROffsets); ++j) {
      int nb = COffsets[j].nb;
      Sys.Offsets[j].nb = nb;
      Sys.Offsets[j].offset_1st = COffsets[j].offset_1st;
      Sys.Offsets[j].refundable = COffsets[j].refundable;
      for (int k = 0; k < MAX_OFFSETN; ++k) {
        int kk = (k < nb) ? k : nb - 1;
        Sys.Offsets[j].Thresholds[k] = COffsets[j].Thresholds[kk];
        Sys.Offsets[j].Tapers[k] = COffsets[j].Tapers[kk];
      }
    }
  }
  
  // Set Sapto
  setDblElement(&Sys.S.first_tax_rate, RSystem, "sapto_first_tax_rate");
  setIntElement(&Sys.S.lwr_couple, RSystem, "sapto_lower_threshold_married");
  setIntElement(&Sys.S.lwr_single, RSystem, "sapto_lower_threshold");
  setIntElement(&Sys.S.lwr_illness, RSystem, "sapto_lower_illness");
  
  setIntElement(&Sys.S.mxo_single, RSystem, "sapto_max_offset");
  setIntElement(&Sys.S.mxo_couple, RSystem, "sapto_max_offset_married");
  setIntElement(&Sys.S.mxo_illness, RSystem, "sapto_max_offset_illness");
  
  
  setDblElement(&Sys.S.pension_age, RSystem, "sapto_pension_age");
  setDblElement(&Sys.S.second_tax_rate, RSystem, "sapto_second_tax_rate");
  setDblElement(&Sys.S.taper, RSystem, "sapto_taper");
  setIntElement(&Sys.S.tax_free_thresh, RSystem, "sapto_tax_free_thresh");
  
  Sys.S.upr_single = Sys.S.lwr_single + Sys.S.mxo_single / Sys.S.taper;
  Sys.S.upr_couple = Sys.S.lwr_couple + Sys.S.mxo_couple / Sys.S.taper;
  Sys.S.upr_couple *= 2;
  Sys.S.upr_illness = Sys.S.lwr_illness + Sys.S.mxo_illness / Sys.S.taper;
  Sys.S.upr_illness *= 2;
  
  Sys.S.year = yr;
  return Sys;
}

static bool invalid_medicare_params(int ma, int mb, double mt, double mr) {
  int lhs = mt * (mb - ma);
  int rhs = mr * mb;
  return lhs != rhs && lhs != (rhs - 1) && lhs != (rhs + 1);
}

SEXP System2Sexp(const System Sys) {
  int np = 0;
  SEXP ans = PROTECT(allocVector(VECSXP, SYSTEM_LEN)); ++np;
  SET_VECTOR_ELT(ans, 0, ScalarInteger(Sys.yr));
  SEXP Bracks = PROTECT(allocVector(INTSXP, Sys.nb)); ++np;
  SEXP Rates = PROTECT(allocVector(REALSXP, Sys.nb)); ++np;
  for (int b = 0; b < Sys.nb; ++b) {
    INTEGER(Bracks)[b] = Sys.BRACKETS[b];
    REAL(Rates)[b] = Sys.RATES[b];
  }
  SET_VECTOR_ELT(ans, 1, ScalarInteger(Sys.nb));
  SET_VECTOR_ELT(ans, 2, Bracks);
  SET_VECTOR_ELT(ans, 3, Rates);
  SET_VECTOR_ELT(ans, 4, Medicare2Sexp(Sys.M));
  SET_VECTOR_ELT(ans, 5, ScalarLogical(Sys.has_sapto));
  SET_VECTOR_ELT(ans, 6, Sapto2Sexp(Sys.S));
  SET_VECTOR_ELT(ans, 7, ScalarInteger(Sys.n_offsetn));
  SET_VECTOR_ELT(ans, 8, nOffsets2List(Sys.Offsets, Sys.n_offsetn));
  SET_VECTOR_ELT(ans, 9, ScalarLogical(Sys.has_temp_budget_repair_levy));
  
  SEXP nms = PROTECT(allocVector(STRSXP, SYSTEM_LEN)); ++np;
  SET_STRING_ELT(nms, 0, mkCharCE("yr", CE_UTF8));
  SET_STRING_ELT(nms, 1, mkCharCE("nb", CE_UTF8));
  SET_STRING_ELT(nms, 2, mkCharCE("ordinary_tax_thresholds", CE_UTF8));
  SET_STRING_ELT(nms, 3, mkCharCE("ordinary_tax_rates", CE_UTF8));
  SET_STRING_ELT(nms, 4, mkCharCE("Medicare", CE_UTF8));
  SET_STRING_ELT(nms, 5, mkCharCE("has_sapto", CE_UTF8));
  SET_STRING_ELT(nms, 6, mkCharCE("Sapto", CE_UTF8));
  SET_STRING_ELT(nms, 7, mkCharCE("n_offsetn", CE_UTF8));
  SET_STRING_ELT(nms, 8, mkCharCE("Offsets", CE_UTF8));
  SET_STRING_ELT(nms, 9, mkCharCE("has_temp_budget_repair_levy", CE_UTF8));
  setAttrib(ans, R_NamesSymbol, nms);
  UNPROTECT(np);
  return ans;
}

SEXP CvalidateSystem(SEXP RSystem, SEXP Fix) {
  if (isNull(RSystem)) {
    return R_NilValue;
  }
  
  if (!isVectorList(RSystem) || !isInteger(Fix)) {
    error("(CvalidateSystem): RSystem was type '%s' but must be type list",
          type2char(TYPEOF(RSystem)));
  }
  int fix = asInteger(Fix);
  int yr = asInteger(getListElement(RSystem, "yr"));
  SEXP Bracks = getListElement(RSystem, "ordinary_tax_thresholds");
  SEXP Rates = getListElement(RSystem, "ordinary_tax_rates");
  if (length(Bracks) && length(Rates) && length(Bracks) != length(Rates)) {
    error("`length(ordinary_tax_thresholds) = %d` yet `length(ordinary_tax_rates) = %d`. Both lengths must be equal.",
          length(Bracks), length(Rates));
  }
  System Sys = Sexp2System(RSystem, yr);
  
  
  // # Individuals
  //   ma <- medicare_levy_lower_threshold %|||% medicare_tbl_fy[["lower_threshold"]]
  //   mb <- medicare_levy_upper_threshold %|||% medicare_tbl_fy[["upper_threshold"]]
  //   mt <- medicare_levy_taper %|||% medicare_tbl_fy[["taper"]]
  //   mr <- medicare_levy_rate  %|||% medicare_tbl_fy[["rate"]]
  int ma = getIntElement(RSystem, "medicare_levy_lower_threshold", ml_lower_thresh(yr, false, false));
  int mb = getIntElement(RSystem, "medicare_levy_upper_threshold", ml_upper_thresh(yr, false, false));
  double mt = getDblElement(RSystem, "medicare_levy_taper", ml_taper(yr));
  double mr = getDblElement(RSystem, "medicare_levy_rate", ml_rate(yr));
  //   
  // # Individuals - SAPTO
  // # N.B. medicare_tbl_fy[["lower/upper_threshold"]] since the join above correctly identifies which ones
  //   msa <- medicare_levy_lower_sapto_threshold %|||% medicare_tbl_fy[["lower_threshold"]]
  //   msb <- medicare_levy_upper_sapto_threshold %|||% medicare_tbl_fy[["upper_threshold"]]
  int msa = getIntElement(RSystem, "medicare_levy_lower_sapto_threshold", ml_lower_thresh(yr, false, true));
  int msb = getIntElement(RSystem, "medicare_levy_upper_sapto_threshold", ml_upper_thresh(yr, false, true)); 
  //   
  //   ma <- as.integer(ma)
  //     msa <- as.integer(msa)
  //     mb <- as.integer(mb - 1)
  //     msb <- as.integer(msb - 1)
  
  // # Families
  // mfa <- medicare_levy_lower_family_threshold %|||% medicare_tbl_fy[["lower_family_threshold"]]
  // mfb <- medicare_levy_upper_family_threshold %|||% medicare_tbl_fy[["upper_family_threshold"]]
  int mfa = getIntElement(RSystem, "medicare_levy_lower_family_threshold", ml_lower_thresh(yr, true, false));
  int mfb = getIntElement(RSystem, "medicare_levy_upper_family_threshold", ml_upper_thresh(yr, true, false));
  // 
  // # Families - SAPTO
  // mfsa <- medicare_levy_lower_family_sapto_threshold %|||% medicare_tbl_fy[["lower_family_threshold"]]
  // mfsb <- medicare_levy_upper_family_sapto_threshold %|||% medicare_tbl_fy[["upper_family_threshold"]]
  int mfsa = getIntElement(RSystem, "medicare_levy_lower_family_sapto_threshold", ml_lower_thresh(yr, true, true));
  int mfsb = getIntElement(RSystem, "medicare_levy_upper_family_sapto_threshold", ml_upper_thresh(yr, true, true));
  
  errif_nonnegative(ma, "medicare_levy_lower_threshold");
  errif_nonnegative(mb, "medicare_levy_upper_threshold");
  errif_nonnegative(mfa, "medicare_levy_lower_family_threshold");
  errif_nonnegative(mfb, "medicare_levy_upper_family_threshold");
  errif_nonnegative(msa, "medicare_levy_lower_sapto_threshold");
  errif_nonnegative(msb, "medicare_levy_upper_sapto_threshold");
  errif_nonnegative(mfsa, "medicare_levy_lower_family_sapto_threshold");
  errif_nonnegative(mfsb, "medicare_levy_upper_family_sapto_threshold");
  
  if (invalid_medicare_params(ma, mb, mt, mr)) {
    if (!hazName(RSystem, "medicare_levy_upper_threshold")) {
      mb = mt * ma / (mt - mr);
      // TODO: warningcall
      if (fix == 1) {
        warning("`medicare_levy_upper_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_upper_threshold = %d", mb);
      }
    } else if (!hazName(RSystem, "medicare_levy_lower_threshold")) {
      ma = mb * (mt - mr) / mt;
      if (fix == 1) {
        warning("`medicare_levy_lower_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_lower_threshold = %d", ma);
      }
    } else if (!hazName(RSystem, "medicare_levy_taper")) {
      mt = mr * mb / (mb - ma);
      if (fix == 1) {
        warning("`medicare_levy_taper` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_taper = %f", mt);
      }
    } else if (!hazName(RSystem, "medicare_levy_rate")) {
      mr = mt * (mb - ma) / mb;
      if (fix == 1) {
        warning("`medicare_levy_rate` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_rate = %f", mr);
      }
    }
  }
  if (invalid_medicare_params(msa, msb, mt, mr)) {
    if (!hazName(RSystem, "medicare_levy_upper_sapto_threshold")) {
      msb = mt * msa / (mt - mr);
      if (fix == 1) {
        warning("`medicare_levy_upper_sapto_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n" 
                  "Its value has been set to:\n\t"
                  "medicare_levy_upper_sapto_threshold = %d", msb);
      }
    } else if (!hazName(RSystem, "medicare_levy_lower_sapto_threshold")) {
      msa = msb * (mt - mr) / mt;
      if (fix == 1) {
        warning("`medicare_levy_lower_sapto_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_lower_sapto_threshold = %d", msa);
      }
    } else {
      double medicare_levy_taper_stop = mr * msb / (msb - msa);
      error("Medicare levy parameter mismatch could not be safely resolved.\n\n"
            "`medicare_levy_lower_sapto_threshold = %d` and "
            "`medicare_levy_upper_sapto_threshold = %d` were both supplied, "
            "but imply a Medicare taper rate of %f\n\t",
            msa, msb,
            medicare_levy_taper_stop);
    }
  }
  if (invalid_medicare_params(mfa, mfb, mt, mr)) {
    if (!hazName(RSystem, "medicare_levy_upper_family_threshold")) {
      mfb = mt * mfa / (mt - mr);
      if (fix == 1) {
        warning("`medicare_levy_upper_family_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n" 
                  "Its value has been set to:\n\t"
                  "medicare_levy_upper_family_threshold = %d", mfb);
      }
    } else if (!hazName(RSystem, "medicare_levy_lower_family_threshold")) {
      mfa = mfb * (mt - mr) / mt;
      if (fix == 1) {
        warning("`medicare_levy_lower_family_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_lower_family_threshold = %d", mfa);
      }
    } else {
      double medicare_levy_taper_stop = mr * mfb / (mfb - mfa);
      error("Medicare levy parameter mismatch could not be safely resolved.\n\n"
              "`medicare_levy_lower_family_threshold = %d` and "
              "`medicare_levy_upper_family_threshold = %d` were both supplied, "
              "but imply a Medicare taper rate of %f\n\t",
              mfa, mfb,
              medicare_levy_taper_stop);
    }
  }
  if (invalid_medicare_params(mfsa, mfsb, mt, mr)) {
    if (!hazName(RSystem, "medicare_levy_upper_family_sapto_threshold")) {
      mfsb = mt * mfsa / (mt - mr);
      if (fix == 1) {
        warning("`medicare_levy_upper_family_sapto_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n" 
                  "Its value has been set to:\n\t"
                  "medicare_levy_upper_family_sapto_threshold = %d", mfsb);
      }
    } else if (!hazName(RSystem, "medicare_levy_lower_family_sapto_threshold")) {
      mfsa = mfsb * (mt - mr) / mt;
      if (fix == 1) {
        warning("`medicare_levy_lower_family_sapto_threshold` was not specified "
                  "but its default value would be inconsistent with the parameters that were specified.\n"
                  "Its value has been set to:\n\t"
                  "medicare_levy_lower_family_sapto_threshold = %d", mfsa);
      }
    } else {
      double medicare_levy_taper_stop = mr * mfsb / (mfsb - mfa);
      error("Medicare levy parameter mismatch could not be safely resolved.\n\n"
              "`medicare_levy_lower_family_sapto_threshold` and "
              "`medicare_levy_upper_family_sapto_threshold` were both supplied, "
              "but imply a Medicare taper rate of %f\n\t",
              medicare_levy_taper_stop);
    }
  }
  validate_medicare(&Sys.M, fix, yr);
  validate_sapto(&Sys.S, fix);
  
  return System2Sexp(Sys);
}







