#ifndef grattan_1999_H
#define grattan_1999_H

constexpr double ORD_TAX_BRACK_1999[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1999[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1999 = 13390;
constexpr double ML_UPR_THRESHOLD_SINGLE_1999 = 14474;
constexpr double ML_LWR_THRESHOLD_FAMILY_1999 = 22595;
constexpr double ML_UPR_THRESHOLD_FAMILY_1999 = 24425;
constexpr double ML_LWR_THR_UP_PER_CHILD_1999 =  2100;
constexpr double ML_TAPER_1999 = 0.20;
constexpr double ML_RATE_1999 = 0.015;
constexpr double LITO_MAX_OFFSET_1999 = 150;
constexpr double LITO_1ST_TAPER_1999 = -0.04;
constexpr double LITO_1ST_THRESH_1999 = 20700;
double do_1_medicare_levy_1999(double xd, double yd, bool is_family, int n_dependants );
#endif
