#include "grattan.h"

bool hazName(SEXP list, const char * str) {
  int n = length(list);
  SEXP names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < n; ++i) {
    if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      return true;
    }
  }
  return false;
}
