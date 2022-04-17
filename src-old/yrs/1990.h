#ifndef grattan_1990_H
#define grattan_1990_H

constexpr double ORD_TAX_BRACK_1990[6] = {0, 5100, 17650, 20600, 35e3, 50e3};
constexpr double ORD_TAX_RATES_1990[6] = {0, 0.21, 0.29, 0.39, 0.47, 0.48};
constexpr double ML_LWR_THRESHOLD_SINGLE_1990 = 10331;
constexpr double ML_UPR_THRESHOLD_SINGLE_1990 = 11019;
constexpr double ML_LWR_THRESHOLD_FAMILY_1990 = 17401;
constexpr double ML_UPR_THRESHOLD_FAMILY_1990 = 18560;
constexpr double ML_LWR_THR_UP_PER_CHILD_1990 =  2100;
constexpr double ML_TAPER_1990 = 0.25;
constexpr double ML_RATE_1990 = 0.0125;
double do_1_medicare_levy_1990(double xd, double yd, bool is_family, int n_dependants );
#endif
