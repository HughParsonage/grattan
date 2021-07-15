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
  
  // Defined in the regulations (relating to spouse transfers)
  double first_tax_rate;
  double second_tax_rate;
  int tax_free_thresh;
  int tax_2nd_thresh;
  double lito_max_offset;
  double lito_1st_thresh;
  double lito_1st_taper;
};

// s12 of Income Tax Assessment (1936 Act) Regulation 2015
// specfies 6000 and 0.15 in the regulations
// http://classic.austlii.edu.au/au/legis/cth/consol_reg/ita1936ar2015352/s12.html
constexpr int SAPTO_S12_THRESH = 6000;
constexpr double SAPTO_S12_TAPER = 0.15;

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

