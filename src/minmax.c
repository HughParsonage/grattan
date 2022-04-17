#include "grattan.h"

double dmax(double x, double y) {
  return (x < y) ? y : x;
}

double dmin(double x, double y) {
  return (x < y) ? x : y;
}

int imax(int x, int y) {
  return (x < y) ? y : x;
}

int imin(int x, int y) {
  return (x < y) ? x : y;
}

double dmax0(double x) {
  return (x > 0) ? x : 0;
}

double dmin0(double x) {
  return (x <= 0) ? x : 0;
}


