#include "grattan.h"

// lito1 ~=> single taper lito
// lito2 ~=> double taper lito

int max_lito1(int yr) {
  if (yr >= 2013) {
    return 445;
  }
  if (yr >= 2011) {
    return 1500;
  }
  if (yr <= 2003) {
    return 150;
  }
  if (yr <= 2006) {
    return 235;
  }
  if (yr == 2007) {
    return 600;
  }
  if (yr == 2008) {
    return 750;
  }
  if (yr == 2009) {
    return 1200;
  }
  return 0; // unknown
}

int threshold_lito1(int yr) {
  if (yr >= 2013) {
    return 37000;
  }
  if (yr <= 2002) {
    return 20700;
  }
  if (yr >= 2008) {
    return 30000;
  }
  if (yr == 2007) {
    return 25000;
  }
  if (yr >= 2004) {
    return 21600;
  }
  if (yr == 2003) {
    return 18575;
  }
  return 0;
}

void apply_lito(double * tax, Person P, int yr) {
  if (yr < 1994) {
    return;
  }
  
  if (yr < 2021) {
    Offset1 O;
    O.taper_1st = yr < 2013 ? -0.04 : -0.015;
    O.offset_1st = max_lito1(yr);
    O.refundable = false;
    O.thresh_1st = threshold_lito1(yr);
    apply_offset1(tax, P, O);
    return;
  }
  
  Offset2 O;
  O.offset_1st = 700;
  O.refundable = false;
  O.taper_1st = -0.05;
  O.taper_2nd = -0.015;
  O.thresh_1st = 37500;
  O.thresh_2nd = 45000;
  apply_offset2(tax, P, O);
}

Offset1 LITO_ante2020(int yr) {
  Offset1 O;
  O.taper_1st = yr < 2013 ? -0.04 : -0.015;
  O.offset_1st = max_lito1(yr);
  O.refundable = false;
  O.thresh_1st = threshold_lito1(yr);
  return O;
}

Offset2 LITO_post2020(int yr) {
  Offset2 O;
  O.offset_1st = 700;
  O.refundable = false;
  O.taper_1st = -0.05;
  O.taper_2nd = -0.015;
  O.thresh_1st = 37500;
  O.thresh_2nd = 66667;
  return O;
}


