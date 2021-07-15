#ifndef grattan_1997_H
#define grattan_1997_H

constexpr double ORD_TAX_BRACK_1997[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1997[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1997 = 13128;
constexpr double ML_UPR_THRESHOLD_SINGLE_1997 = 14346;
constexpr double ML_LWR_THRESHOLD_FAMILY_1997 = 22153;
constexpr double ML_UPR_THRESHOLD_FAMILY_1997 = 24209;
constexpr double ML_LWR_THR_UP_PER_CHILD_1997 =  2100;
constexpr double ML_TAPER_1997 = 0.20;
constexpr double ML_RATE_1997 = 0.017;
constexpr double LITO_MAX_OFFSET_1997 = 150;
constexpr double LITO_1ST_TAPER_1997 = -0.04;
constexpr double LITO_1ST_THRESH_1997 = 20700;
double do_1_medicare_levy_1997(double xd, double yd, bool is_family, int n_dependants );
#endif
