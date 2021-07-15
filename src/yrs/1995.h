#ifndef grattan_1995_H
#define grattan_1995_H

constexpr double ORD_TAX_BRACK_1995[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1995[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1995 = 12689;
constexpr double ML_UPR_THRESHOLD_SINGLE_1995 = 13643;
constexpr double ML_LWR_THRESHOLD_FAMILY_1995 = 21367;
constexpr double ML_UPR_THRESHOLD_FAMILY_1995 = 22974;
constexpr double ML_LWR_THR_UP_PER_CHILD_1995 =  2100;
constexpr double ML_TAPER_1995 = 0.20;
constexpr double ML_RATE_1995 = 0.014;
constexpr double LITO_MAX_OFFSET_1995 = 150;
constexpr double LITO_1ST_TAPER_1995 = -0.04;
constexpr double LITO_1ST_THRESH_1995 = 20700;
double do_1_medicare_levy_1995(double xd, double yd, bool is_family, int n_dependants );
#endif
