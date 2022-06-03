#ifndef grattan_2014_H
#define grattan_2014_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2014 = 20542;
double ML_UPR_THRESHOLD_SINGLE_2014 = 24168;
double ML_LWR_THRESHOLD_FAMILY_2014 = 34367;
double ML_UPR_THRESHOLD_FAMILY_2014 = 40433;
double ML_LWR_THR_UP_PER_CHILD_2014 =  3156;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2014 = 32279;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2014 = 37976;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2014 = 46000;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2014 = 54119;
double ML_TAPER_2014 = 0.1;
double ML_RATE_2014 = 0.015;
double LITO_MAX_OFFSET_2014 = 445;
double LITO_1ST_TAPER_2014 = -0.015;
double LITO_1ST_THRESH_2014 = 37000;
double SAPTO_MAX_SINGLE_2014 = 2230;
double SAPTO_MAX_MARRIED_2014 = 1602;
double SAPTO_MAX_ILL_SEP_2014 = 2040;
double SAPTO_TAPER_2014 = -0.125;
double SAPTO_LWR_SINGLE_2014 = 32279;
double SAPTO_LWR_MARRIED_2014 = 28974;
double SAPTO_LWR_ILL_SEP_2014 = 28974;

System System2014 = {
  .yr = 2014,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
    .lwr_single = 20542,
    .upr_single = 24167,
    .lwr_family = 34367,
    .upr_family = 40431,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 32279,
    .upr_single_sapto = 37976,
    .lwr_family_sapto = 46000,
    .upr_family_sapto = 54118,
    .lwr_thr_up_per_child = 3238,
    .taper = 0.1,
    .rate = 0.015
  },
  .has_sapto = true,
  .S = {
    .year = 2014,
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
  .n_offsetn = 1,
  .Offsets = {
    {
      // LITO
      .offset_1st = 445,
      .Thresholds = {37000},
      .Tapers = {-0.015},
      .nb = 1,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
