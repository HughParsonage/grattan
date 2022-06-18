#include "grattan.h"



// from stats package
SEXP getListElement(SEXP list, const char * str) {
  if (starts_with_medicare(str) && hazName(list, "Medicare")) {
    SEXP MedicareList = getListElement(list, "Medicare");
    
    SEXP out = getListElement(MedicareList, str);
    if (TYPEOF(out) != NILSXP) {
      return out;
    }
    out = getListElement(MedicareList, str + strlen("medicare_levy_"));
    return out;
  }
  if (starts_with_sapto(str) && hazName(list, "Sapto")) {
    SEXP SaptoList = getListElement(list, "Sapto");
    SEXP out = getListElement(SaptoList, str);
    if (TYPEOF(out) != NILSXP) {
      return out;
    }
    out = getListElement(SaptoList, str + strlen("sapto_"));
    if (TYPEOF(out) != NILSXP) {
      return out;
    }
    
    
    return out;
  }
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); i++) {
    if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  return elmt;
}


