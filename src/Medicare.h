#ifndef grattan_medicare_h
#define grattan_medicare_h

struct Medicare {
  double lwr_single;
  double upr_single;
  double lwr_family;
  double upr_family;
  bool has_sapto_thr;
  int sapto_age;
  double lwr_single_sapto;
  double upr_single_sapto;
  double lwr_family_sapto;
  double upr_family_sapto;
  double lwr_thr_up_per_child;
  double taper;
  double rate;
};

Medicare medicare_levies(int yr);

#endif
