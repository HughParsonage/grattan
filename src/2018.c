#ifndef grattan_2018_H
#define grattan_2018_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2018 = 21980;
double ML_UPR_THRESHOLD_SINGLE_2018 = 27476;
double ML_LWR_THRESHOLD_FAMILY_2018 = 37089;
double ML_UPR_THRESHOLD_FAMILY_2018 = 46363;
double ML_LWR_THR_UP_PER_CHILD_2018 =  3406;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2018 = 34758;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2018 = 43449;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2018 = 48385;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2018 = 60483;
double ML_TAPER_2018 = 0.1;
double ML_RATE_2018 = 0.02;
double LITO_MAX_OFFSET_2018 = 445;
double LITO_1ST_TAPER_2018 = -0.015;
double LITO_1ST_THRESH_2018 = 37000;
double SAPTO_MAX_SINGLE_2018 = 2230;
double SAPTO_MAX_MARRIED_2018 = 1602;
double SAPTO_MAX_ILL_SEP_2018 = 2040;
double SAPTO_TAPER_2018 = -0.125;
double SAPTO_LWR_SINGLE_2018 = 32279;
double SAPTO_LWR_MARRIED_2018 = 28974;
double SAPTO_LWR_ILL_SEP_2018 = 28974;
double SBTO_DISCOUNT_2018 = 0.08;

System System2018 = {
  .yr = 2018,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 87000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M = 
  {
    .lwr_single = 21980,
    .upr_single = 27476,
    .lwr_family = 37089,
    .upr_family = 46363,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 34758,
    .upr_single_sapto = 43449,
    .lwr_family_sapto = 48385,
    .upr_family_sapto = 60483,
    .lwr_thr_up_per_child = 3406,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = 
  {
    .year = 2018,
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
  .has_temp_budget_repair_levy = false
  };

#endif
