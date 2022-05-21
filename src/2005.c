#ifndef grattan_2005_H
#define grattan_2005_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2005 = 15902;
double ML_UPR_THRESHOLD_SINGLE_2005 = 17192;
double ML_LWR_THRESHOLD_FAMILY_2005 = 26834;
double ML_UPR_THRESHOLD_FAMILY_2005 = 29011;
double ML_LWR_THR_UP_PER_CHILD_2005 =  2464;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2005 = 20500;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2005 = 22163;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2005 = 31729;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2005 = 34303;
double ML_LWR_THRESHOLD_SINGLE_PTO_2005 = 19252;
double ML_LWR_THRESHOLD_FAMILY_PTO_2005 = 31729;
double ML_TAPER_2005 = 0.1;
double ML_RATE_2005 = 0.015;
double LITO_MAX_OFFSET_2005 = 235;
double LITO_1ST_TAPER_2005 = -0.04;
double LITO_1ST_THRESH_2005 = 21600;
double SAPTO_MAX_SINGLE_2005 = 2117;
double SAPTO_MAX_MARRIED_2005 = 3170;
double SAPTO_LWR_SINGLE_2005 = 18453;
double SAPTO_LWR_MARRIED_2005 = 30648;
double SAPTO_TAPER_2005 = -0.125;
System System2005 = {
    .yr = 2005,
  .nb = 5,
  .BRACKETS = 0, 6000, 21600, 58000, 70000, INT_MAX, INT_MAX, INT_MAX,
  .RATES = 0, 0.17, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47,
  .M = {
  .lwr_single = 15902,
  .upr_single = 18709,
  .lwr_family = 26834,
  .upr_family = 31570,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 20500,
  .upr_single_sapto = 24118,
  .lwr_family_sapto = 31729,
  .upr_family_sapto = 37329,
  .lwr_thr_up_per_child = 2464,
  .taper = 0.1,
  .rate = 0.015
  },
  .has_sapto = true,
  .S = {
  .year = 2005,
  .pension_age = 65,
  .mxo_single = 2117,
  .mxo_couple = 3170,
  .lwr_single = 18453,
  .lwr_couple = 30648,
  .upr_single = 35389,
  .upr_couple = 56008,
  .taper = 0.125,
  .first_tax_rate = 0.17,
  .second_tax_rate = 0.3,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 21600,
  .lito_max_offset = 235,
  .lito_1st_thresh = 21600,
  .lito_1st_taper = 0.04
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
  .has_temp_budget_repair_levy = false
};
#endif
