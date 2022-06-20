#include "grattan.h"

unsigned int frac12(double x) {
  double o = x - (int)x;
  int o2 = 12 * o;
  return o2 + 1;
}

Age int2Age(int x) {
  Age A;
  if (x <= 0) {
    A.years = 0;
    A.month = 0;
    return A;
  }
  if (x >= 127) {
    A.years = 126;
    A.month = 12;
    return A;
  }
  A.years = x;
  A.month = 1;
  return A;
}

Age dbl2Age(double x) {
  Age A;
  if (x <= 0) {
    A.years = 0;
    A.month = 0;
    return A;
  }
  if (x >= 127) {
    A.years = 126;
    A.month = 12;
    return A;
  }
  A.years = x;
  A.month = frac12(x);
  return A;
}
