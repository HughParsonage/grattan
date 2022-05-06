#include "grattan.h"

void isEquiInt(SEXP x, SEXP y, const char * str) {
  if (!isInteger(x) || !isInteger(y) || xlength(x) != xlength(y)) {
    error("Internal error: '%s' isntEquiInt (type = '%s')", str, type2char(TYPEOF(y)));
  }
}

