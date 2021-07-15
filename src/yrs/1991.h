#ifndef grattan_1991_H
#define grattan_1991_H

constexpr double ORD_TAX_BRACK_1991[8] = {0, 5250, 17650, 20600, 20700, 35e3, 36e3, 50e3};
constexpr double ORD_TAX_RATES_1991[8] = {0, 0.205, 0.245, 0.295, 0.385, 0.425, 0.465, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1991 = 11746;
constexpr double ML_UPR_THRESHOLD_SINGLE_1991 = 12529;
constexpr double ML_LWR_THRESHOLD_FAMILY_1991 = 19046;
constexpr double ML_UPR_THRESHOLD_FAMILY_1991 = 20314;
constexpr double ML_LWR_THR_UP_PER_CHILD_1991 =  2100;
constexpr double ML_TAPER_1991 = 0.25;
constexpr double ML_RATE_1991 = 0.0125;
double do_1_medicare_levy_1991(double xd, double yd, bool is_family, int n_dependants );
#endif
