double ML_LWR_THRESHOLD_SINGLE_1988 =  8981;
double ML_UPR_THRESHOLD_SINGLE_1988 =  9579;
double ML_LWR_THRESHOLD_FAMILY_1988 = 15091;
double ML_UPR_THRESHOLD_FAMILY_1988 = 16097;
double ML_LWR_THR_UP_PER_CHILD_1988 =  2100;
double ML_TAPER_1988 = 0.25;
double ML_RATE_1988 = 0.0125;
System System1988 = {
    .yr = 1988,
  .nb = 5,
  .BRACKETS = { 0, 5100, 12600, 19500, 35000, INT_MAX, INT_MAX, INT_MAX },
  .RATES = { 0, 0.24, 0.29, 0.4, 0.49, 0.49, 0.49, 0.49 },
  .M = {
  .lwr_single = 8981,
  .upr_single = 10566,
  .lwr_family = 15091,
  .upr_family = 17755,
  .has_sapto_thr = true,
  .sapto_age = 65,
  .lwr_single_sapto = 1988,
  .upr_single_sapto = 2339,
  .lwr_family_sapto = 16843008,
  .upr_family_sapto = 19815304,
  .lwr_thr_up_per_child = 2100,
  .taper = 0.25,
  .rate = 0.0125
  },
  .has_sapto = false,
  .S = {
  .year = 1988,
  .pension_age = 65,
  .mxo_single = 0,
  .mxo_couple = 0,
  .lwr_single = 0,
  .lwr_couple = 0,
  .upr_single = 0,
  .upr_couple = 0,
  .taper = 0.125,
  .first_tax_rate = 0.24,
  .second_tax_rate = 0.29,
  .tax_free_thresh = 6000,
  .tax_2nd_thresh = 12600,
  .lito_max_offset = NA,
  .lito_1st_thresh = NA,
  .lito_1st_taper = NA
  },
  .has_lito = false,
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
