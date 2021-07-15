#ifndef grattan_1996_H
#define grattan_1996_H

constexpr double ORD_TAX_BRACK_1996[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1996[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1996 = 12871;
constexpr double ML_UPR_THRESHOLD_SINGLE_1996 = 13913;
constexpr double ML_LWR_THRESHOLD_FAMILY_1996 = 21719;
constexpr double ML_UPR_THRESHOLD_FAMILY_1996 = 23478;
constexpr double ML_LWR_THR_UP_PER_CHILD_1996 =  2100;
constexpr double ML_TAPER_1996 = 0.20;
constexpr double ML_RATE_1996 = 0.015;
constexpr double LITO_MAX_OFFSET_1996 = 150;
constexpr double LITO_1ST_TAPER_1996 = -0.04;
constexpr double LITO_1ST_THRESH_1996 = 20700;
double do_1_medicare_levy_1996(double xd, double yd, bool is_family, int n_dependants );
#endif
