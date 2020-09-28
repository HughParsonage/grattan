#ifndef grattan_1992_H
#define grattan_1992_H

constexpr double ORD_TAX_BRACK_1992[5] = {0, 5400, 20700, 36e3, 50e3};
constexpr double ORD_TAX_RATES_1992[5] = {0, 0.2, 0.38, 0.46, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1992 = 11746;
constexpr double ML_UPR_THRESHOLD_SINGLE_1992 = 12529;
constexpr double ML_LWR_THRESHOLD_FAMILY_1992 = 19675;
constexpr double ML_UPR_THRESHOLD_FAMILY_1992 = 20986;
constexpr double ML_LWR_THR_UP_PER_CHILD_1992 =  2100;
constexpr double ML_TAPER_1992 = 0.25;
constexpr double ML_RATE_1992 = 0.0125;
double do_1_medicare_levy_1992(double xd, double yd, bool is_family, int n_dependants );
#endif
