#include "grattan.h"

void errif_nonnegative(int x, const char * var) {
  if (x < 0) {
    error("`%s < 0` but must be nonnegative.", var);
  }
}
