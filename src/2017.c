#ifndef grattan_2017_H
#define grattan_2017_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2017 = 21665;
double ML_UPR_THRESHOLD_SINGLE_2017 = 27083;
double ML_LWR_THRESHOLD_FAMILY_2017 = 36541;
double ML_UPR_THRESHOLD_FAMILY_2017 = 45676;
double ML_LWR_THR_UP_PER_CHILD_2017 =  3356;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2017 = 34244;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2017 = 42806;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2017 = 47670;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2017 = 59589;
double ML_TAPER_2017 = 0.1;
double ML_RATE_2017 = 0.02;
double LITO_MAX_OFFSET_2017 = 445;
double LITO_1ST_TAPER_2017 = -0.015;
double LITO_1ST_THRESH_2017 = 37000;
double SAPTO_MAX_SINGLE_2017 = 2230;
double SAPTO_MAX_MARRIED_2017 = 1602;
double SAPTO_MAX_ILL_SEP_2017 = 2040;
double SAPTO_TAPER_2017 = -0.125;
double SAPTO_LWR_SINGLE_2017 = 32279;
double SAPTO_LWR_MARRIED_2017 = 28974;
double SAPTO_LWR_ILL_SEP_2017 = 28974;
double SBTO_DISCOUNT_2017 = 0.08;

System System2017 = {
  .yr = 2017,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 87000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
  .lwr_single = 21665,
  .upr_single = 27083,
  .lwr_family = 36541,
  .upr_family = 45676,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 34244,
  .upr_single_sapto = 42806,
  .lwr_family_sapto = 47670,
  .upr_family_sapto = 59589,
  .lwr_thr_up_per_child = 3356,
  .taper = 0.1,
  .rate = 0.02
  },
  .has_sapto = true,
  .S = {
  .year = 2017,
  .pension_age = 65,
  .mxo_single = 2230,
  .mxo_couple = 1602,
  .lwr_single = 32279,
  .lwr_couple = 28974,
  .upr_single = 50119,
  .upr_couple = 41790,
  .taper = 0.125,
  .first_tax_rate = 0.19,
  .second_tax_rate = 0.325,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 37000,
  .lito_max_offset = 445,
  .lito_1st_thresh = 37000,
  .lito_1st_taper = 0.015
  
  },
  .has_lito = true,
  .has_lmito = false,
  .has_offset1 = false,
  .O1 = {
  .offset_1st = 0,
  .thresh_1st = 0,
  .taper_1st = 0,
  .refundable = false
  },
  .has_offset2 = false,
  .O2 = {
  .offset_1st = 0,
  .thresh_1st = 0,
  .taper_1st = 0,
  .thresh_2nd = 0,
  .taper_2nd = 0,
  .refundable = 0
  },
  .has_offsetn = false,
  .Offsets = {
  .offset_1st = 0,
  .Thresholds = {0, 0, 0, 0, 0, 0, 0, 0},
  .Tapers = {0, 0, 0, 0, 0, 0, 0, 0},
  .nb = 0,
  .refundable = false,
  },
  .has_temp_budget_repair_levy = true
};
#endif