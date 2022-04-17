#ifndef grattan_1993_H
#define grattan_1993_H

constexpr double ORD_TAX_BRACK_1993[5] = {0, 5400, 20700, 36e3, 50e3};
constexpr double ORD_TAX_RATES_1993[5] = {0, 0.2, 0.38, 0.46, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_1993 = 11888;
constexpr double ML_UPR_THRESHOLD_SINGLE_1993 = 12680;
constexpr double ML_LWR_THRESHOLD_FAMILY_1993 = 20071;
constexpr double ML_UPR_THRESHOLD_FAMILY_1993 = 21408;
constexpr double ML_LWR_THR_UP_PER_CHILD_1993 =  2100;
constexpr double ML_TAPER_1993 = 0.25;
constexpr double ML_RATE_1993 = 0.0125;
double do_1_medicare_levy_1993(double xd, double yd, bool is_family, int n_dependants );
#endif
