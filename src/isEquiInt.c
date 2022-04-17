#include "grattan.h"

void isEquiInt(SEXP x, SEXP y) {
  if (!isInteger(x) || !isInteger(y) || xlength(x) != xlength(y)) {
    error("Internal error: isntEquiInt");
  }
}

