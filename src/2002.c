#ifndef grattan_2002_H
#define grattan_2002_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2002 = 14540;
double ML_UPR_THRESHOLD_SINGLE_2002 = 15718;
double ML_LWR_THRESHOLD_FAMILY_2002 = 24534;
double ML_UPR_THRESHOLD_FAMILY_2002 = 26524;
double ML_LWR_THR_UP_PER_CHILD_2002 =  2253;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2002 = 20000;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2002 = 21623;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2002 = 31729;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2002 = 34303;
double ML_LWR_THRESHOLD_SINGLE_PTO_2002 = 16570;
double ML_LWR_THRESHOLD_FAMILY_PTO_2002 = 31729;
double ML_TAPER_2002 = 0.20;
double ML_RATE_2002 = 0.015;
double LITO_MAX_OFFSET_2002 = 150;
double LITO_1ST_TAPER_2002 = -0.04;
double LITO_1ST_THRESH_2002 = 20700;
double SAPTO_MAX_SINGLE_2002 = 1710;
double SAPTO_MAX_MARRIED_2002 = 2490;
double SAPTO_LWR_SINGLE_2002 = 16059;
double SAPTO_LWR_MARRIED_2002 = 26648;
double SAPTO_TAPER_2002 = -0.125;

System System2002 = {
  .yr = 2002,
  .nb = 5,
  .BRACKETS = {0, 6000, 20000, 50000, 60000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.17, 0.3, 0.42, 0.47, 0.47, 0.47, 0.47},
  .M = {
    .lwr_single = 14540,
    .upr_single = 17106,
    .lwr_family = 24534,
    .upr_family = 28864,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 20000,
    .upr_single_sapto = 23530,
    .lwr_family_sapto = 31729,
    .upr_family_sapto = 37329,
    .lwr_thr_up_per_child = 2253,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2002,
    .pension_age = 65,
    .mxo_single = 1710,
    .mxo_couple = 2490,
    .lwr_single = 16059,
    .lwr_couple = 26648,
    .upr_single = 29739,
    .upr_couple = 46568,
    .taper = 0.125,
    .first_tax_rate = 0.17,
    .second_tax_rate = 0.3,
    .tax_free_thresh = 6000,
    .tax_2nd_thresh = 20000,
    .lito_max_offset = 150,
    .lito_1st_thresh = 20700,
    .lito_1st_taper = 0.04
  },
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 150,
      .Thresholds = {20700},
      .Tapers = {-0.04},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
