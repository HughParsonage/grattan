#ifndef grattan_sapto_h
#define grattan_sapto_h

struct Sapto {
  int year;
  double pension_age;
  double mxo_single;
  double mxo_couple;
  double lwr_single;
  double lwr_couple;
  double upr_single;
  double upr_couple;
  double taper;
  
  double first_tax_rate;
  int tax_free_thresh;
  double lito_max_offset;
};

struct Lodge {
  int xi;
  int yi;
  int agei;
  bool is_married;
  bool is_family;
  int n_child;
};

constexpr double SAPTO_TAPER = 0.125;

extern Sapto SaptoSince2000[31];





#endif

