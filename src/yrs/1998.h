#ifndef grattan_1998_H
#define grattan_1998_H

constexpr double ORD_TAX_BRACK_1998[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1998[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1998 = 13390;
constexpr double ML_UPR_THRESHOLD_SINGLE_1998 = 14474;
constexpr double ML_LWR_THRESHOLD_FAMILY_1998 = 22595;
constexpr double ML_UPR_THRESHOLD_FAMILY_1998 = 24695;
constexpr double ML_LWR_THR_UP_PER_CHILD_1998 =  2100;
constexpr double ML_TAPER_1998 = 0.20;
constexpr double ML_RATE_1998 = 0.015;
constexpr double LITO_MAX_OFFSET_1998 = 150;
constexpr double LITO_1ST_TAPER_1998 = -0.04;
constexpr double LITO_1ST_THRESH_1998 = 20700;
double do_1_medicare_levy_1998(double xd, double yd, bool is_family, int n_dependants );
#endif
