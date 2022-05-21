#ifndef grattan_2010_H
#define grattan_2010_H
#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2010 = 18488;
double ML_UPR_THRESHOLD_SINGLE_2010 = 21752;
double ML_LWR_THRESHOLD_FAMILY_2010 = 31196;
double ML_UPR_THRESHOLD_FAMILY_2010 = 36702;
double ML_LWR_THR_UP_PER_CHILD_2010 =  2865;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2010 = 29867;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2010 = 35139;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2010 = 43500;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2010 = 51178;
double ML_LWR_THRESHOLD_SINGLE_PTO_2010 = 27697;
double ML_LWR_THRESHOLD_FAMILY_PTO_2010 = 27697;
double ML_TAPER_2010 = 0.1;
double ML_RATE_2010 = 0.015;
double LITO_MAX_OFFSET_2010 = 1350;
double LITO_1ST_TAPER_2010 = -0.04;
double LITO_1ST_THRESH_2010 = 30000;
double SAPTO_MAX_SINGLE_2010 = 2230;
double SAPTO_LWR_SINGLE_2010 = 29867;
double SAPTO_UPR_SINGLE_2010 = 47707;
double SAPTO_MAX_MARRIED_2010 = 2230;
double SAPTO_LWR_MARRIED_2010 = 29867;
double SAPTO_UPR_MARRIED_2010 = 47707;
double SAPTO_TAPER_2010 = -0.125;

System System2010 = {
  .yr = 2010,
  .nb = 5,
  .BRACKETS = {0, 6000, 37000, 80000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.15, 0.30, 0.38, 0.45, 0.45, 0.45, 0.45},
  .M = {
  .lwr_single = 18488,
  .upr_single = 21752,
  .lwr_family = 31196,
  .upr_family = 36702,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 29867,
  .upr_single_sapto = 35139,
  .lwr_family_sapto = 43500,
  .upr_family_sapto = 51178,
  .lwr_thr_up_per_child = 2865,
  .taper = 0.1,
  .rate = 0.015
  },
  .has_sapto = true,
  .S = {
  .year = 2010,
  .pension_age = 65,
  .mxo_single = 2230,
  .mxo_couple = 1602,
  .lwr_single = 30685,
  .lwr_couple = 28974,
  .upr_single = 48525,
  .upr_couple = 41790,
  .taper = 0.125,
  .first_tax_rate = 0.15,
  .second_tax_rate = 0.30,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 37000,
  .lito_max_offset = 1500,
  .lito_1st_thresh = 30000,
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
