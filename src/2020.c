#ifndef grattan_2020_H
#define grattan_2020_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2020 = 22801;
double ML_UPR_THRESHOLD_SINGLE_2020 = 28503;
double ML_LWR_THRESHOLD_FAMILY_2020 = 38474;
double ML_UPR_THRESHOLD_FAMILY_2020 = 48094;
double ML_LWR_THR_UP_PER_CHILD_2020 =  3533;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2020 = 36056;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2020 = 45071;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2020 = 50191;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2020 = 62740;
double ML_TAPER_2020 = 0.1;
double ML_RATE_2020 = 0.02;
double LITO_MAX_OFFSET_2020 = 445;
double LITO_1ST_TAPER_2020 = -0.015;
double LITO_1ST_THRESH_2020 = 37000;
double SAPTO_MAX_SINGLE_2020 = 2230;
double SAPTO_MAX_MARRIED_2020 = 1602;
double SAPTO_MAX_ILL_SEP_2020 = 2040;
double SAPTO_TAPER_2020 = -0.125;
double SAPTO_LWR_SINGLE_2020 = 32279;
double SAPTO_LWR_MARRIED_2020 = 28974;
double SAPTO_LWR_ILL_SEP_2020 = 28974;
double LMITO_1ST_OFFSET_2020 = 255;
double SBTO_DISCOUNT_2020 = 0.08;

System System2020 = {
  .yr = 2020,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 90000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = {
  .lwr_single = 22801,
  .upr_single = 28503,
  .lwr_family = 38474,
  .upr_family = 48094,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 36056,
  .upr_single_sapto = 45071,
  .lwr_family_sapto = 50191,
  .upr_family_sapto = 62740,
  .lwr_thr_up_per_child = 3533,
  .taper = 0.1,
  .rate = 0.02
  },
  .has_sapto = true,
  .S = {
  .year = 2022,
  .pension_age = 65,
  .mxo_single = 2230,
  .mxo_couple = 1602,
  .lwr_single = 33622,
  .lwr_couple = 30316,
  .upr_single = 51462,
  .upr_couple = 43132,
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
  .has_lmito = true,
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
