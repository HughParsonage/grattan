#ifndef grattan_2021_H
#define grattan_2021_H

#include "grattan.h"

double ML_LWR_THRESHOLD_SINGLE_2021 = 22801;
double ML_UPR_THRESHOLD_SINGLE_2021 = 28503;
double ML_LWR_THRESHOLD_FAMILY_2021 = 38474; // Constant from here on
double ML_UPR_THRESHOLD_FAMILY_2021 = 48094;
double ML_LWR_THR_UP_PER_CHILD_2021 =  3533;
double ML_LWR_THRESHOLD_SINGLE_SAPTO_2021 = 36056;
double ML_UPR_THRESHOLD_SINGLE_SAPTO_2021 = 45071;
double ML_LWR_THRESHOLD_FAMILY_SAPTO_2021 = 50191;
double ML_UPR_THRESHOLD_FAMILY_SAPTO_2021 = 62740;
double ML_TAPER_2021 = 0.1;
double ML_RATE_2021 = 0.02;
double LITO_MAX_OFFSET_2021 = 700;
double LITO_1ST_TAPER_2021 = -0.050;
double LITO_2ND_TAPER_2021 = -0.015;
int LITO_1ST_THRESH_2021 = 37500;
int LITO_2ND_THRESH_2021 = 45000;
double SAPTO_MAX_SINGLE_2021 = 2230;
double SAPTO_MAX_MARRIED_2021 = 1602;
double SAPTO_MAX_ILL_SEP_2021 = 2040;
double SAPTO_TAPER_2021 = 0.125;
double SAPTO_LWR_SINGLE_2021 = 32279;
double SAPTO_LWR_MARRIED_2021 = 28974;
double SAPTO_LWR_ILL_SEP_2021 = 28974;
double LMITO_1ST_OFFSET_2021 = 255;
double SBTO_DISCOUNT_2021 = 0.08;


System System2021 = {
  .yr = 2021,
  .nb = 5,
  .BRACKETS = {0, 18200, 37000, 90000, 180000, INT_MAX, INT_MAX, INT_MAX},
  .RATES = {0, 0.19, 0.325, 0.37, 0.45, 0.45, 0.45, 0.45},
  .M =
  {
    .lwr_single = 23226,
    .upr_single = 29033,
    .lwr_family = 39167,
    .upr_family = 48959,
    .has_sapto_thr = true,
    .sapto_age = 65,
    .lwr_single_sapto = 36056,
    .upr_single_sapto = 45071,
    .lwr_family_sapto = 51094,
    .upr_family_sapto = 63868,
    .lwr_thr_up_per_child = 3533,
    .taper = 0.1,
    .rate = 0.02
  },
  .has_sapto = true,
  .S = {
    .year = 2021,
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
  .n_offsetn = 2,
  .Offsets = {
    {
      // LITO
      .offset_1st = 700,
      .Thresholds = {37500, 45000},
      .Tapers = {0.05, 0.015},
      .nb = 2,
      .refundable = false
    },
    {
      // LMITO
      .offset_1st = 255,
      .Thresholds = {37000, 48000, 90000},
      .Tapers = {-0.075, 0, 0.03},
      .nb = 3,
      .refundable = false
    }
  },
  .has_temp_budget_repair_levy = false
};
#endif
