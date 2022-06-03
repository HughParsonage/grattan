#include "grattan.h"

bool starts_with_medicare(const char * str) {
  return 
  str[0] == 'm' && str[1] == 'e' && str[2] == 'd' &&
    str[3] == 'i' && str[4] == 'c' && str[5] == 'a' &&
    str[6] == 'r' && str[7] == 'e';
}
