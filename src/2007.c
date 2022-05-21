#ifndef grattan_2007_H
#define grattan_2007_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2007 = 16740;
double ML_UPR_THRESHOLD_SINGLE_2007 = 19695;
double ML_LWR_THRESHOLD_FAMILY_2007 = 28247;
double ML_UPR_THRESHOLD_FAMILY_2007 = 33233;
double ML_LWR_THR_UP_PER_CHILD_2007 =  2594;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2007 = 24867;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2007 = 29256;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2007 = 33500;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2007 = 39413;
double ML_LWR_THRESHOLD_SINGLE_PTO_2007 = 21637;
double ML_LWR_THRESHOLD_FAMILY_PTO_2007 = 33500;
double ML_TAPER_2007 = 0.1;
double ML_RATE_2007 = 0.015;
double LITO_MAX_OFFSET_2007 = 600;
double LITO_1ST_TAPER_2007 = -0.04;
double LITO_1ST_THRESH_2007 = 25000;
double SAPTO_MAX_MARRIED_2007 = 3204;
double SAPTO_LWR_MARRIED_2007 = 43360;
double SAPTO_UPR_MARRIED_2007 = 66992;
double SAPTO_MAX_SINGLE_2007 = 2230;
double SAPTO_LWR_SINGLE_2007 = 24867;
double SAPTO_UPR_SINGLE_2007 = 42707;
double SAPTO_TAPER_2007 = -0.125;
System System2007 = {
    .yr = 2007,
  .nb = 5,
  .BRACKETS = {0, 6000, 25000, 75000, 150000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.3, 0.4, 0.45, 0.45, 0.45, 0.45},
  .M = {
  .lwr_single = 16740,
  .upr_single = 19695,
  .lwr_family = 28247,
  .upr_family = 33232,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 24867,
  .upr_single_sapto = 29256,
  .lwr_family_sapto = 33500,
  .upr_family_sapto = 39412,
  .lwr_thr_up_per_child = 2594,
  .taper = 0.1,
  .rate = 0.015
  },
  .has_sapto = true,
  .S = {
  .year = 2007,
  .pension_age = 65,
  .mxo_single = 2230,
  .mxo_couple = 3204,
  .lwr_single = 24867,
  .lwr_couple = 43360,
  .upr_single = 42707,
  .upr_couple = 68992,
  .taper = 0.125,
  .first_tax_rate = 0.15,
  .second_tax_rate = 0.3,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 25000,
  .lito_max_offset = 600,
  .lito_1st_thresh = 25000,
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
