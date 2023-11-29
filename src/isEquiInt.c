#include "grattan.h"

void isEquiInt(SEXP x, SEXP y, const char * str) {
  if (!isInteger(x) || !isInteger(y) || xlength(x) != xlength(y)) {
    if (xlength(x) != xlength(y)) {
      error("Internal error: '%s' lengths differ (%lld vs %lld)", str, (long long)xlength(x), (long long)xlength(y));
    }
    if (!isInteger(x)) {
      error("Internal error '%s' was type '%s', not integer.", str, type2char(TYPEOF(x)));
    }
    error("Internal error: '%s' isntEquiInt (type = '%s')", str, type2char(TYPEOF(y)));
  }
}



