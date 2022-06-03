#include "grattan.h"

// from stats package
SEXP getListElement(SEXP list, const char * str) {
  if (starts_with_medicare(str) && hazName(list, "Medicare")) {
    SEXP MedicareList = getListElement(list, "Medicare");
    return getListElement(MedicareList, str);
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
