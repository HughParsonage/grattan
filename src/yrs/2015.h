#ifndef grattan_2015_H
#define grattan_2015_H

constexpr double ORD_TAX_BRACK_2015[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2015[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2015 = 20896;
constexpr double ML_UPR_THRESHOLD_SINGLE_2015 = 26121;
constexpr double ML_LWR_THRESHOLD_FAMILY_2015 = 35261;
constexpr double ML_UPR_THRESHOLD_FAMILY_2015 = 44078;
constexpr double ML_LWR_THR_UP_PER_CHILD_2015 =  3238;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2015 = 33044;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2015 = 41306;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2015 = 46000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2015 = 57501;
constexpr double ML_TAPER_2015 = 0.1;
constexpr double ML_RATE_2015 = 0.02;
double do_1_medicare_levy_2015(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2015 = 445;
constexpr double LITO_1ST_TAPER_2015 = -0.015;
constexpr double LITO_1ST_THRESH_2015 = 37000;
constexpr double SAPTO_MAX_SINGLE_2015 = 2230;
constexpr double SAPTO_MAX_MARRIED_2015 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2015 = 2040;
constexpr double SAPTO_TAPER_2015 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2015 = 32279;
constexpr double SAPTO_LWR_MARRIED_2015 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2015 = 28974;
#endif
