#ifndef grattan_2014_H
#define grattan_2014_H

constexpr double ORD_TAX_BRACK_2014[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2014[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2014 = 20542;
constexpr double ML_UPR_THRESHOLD_SINGLE_2014 = 24168;
constexpr double ML_LWR_THRESHOLD_FAMILY_2014 = 34367;
constexpr double ML_UPR_THRESHOLD_FAMILY_2014 = 40433;
constexpr double ML_LWR_THR_UP_PER_CHILD_2014 =  3156;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2014 = 32279;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2014 = 37976;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2014 = 46000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2014 = 54119;
constexpr double ML_TAPER_2014 = 0.1;
constexpr double ML_RATE_2014 = 0.015;
double do_1_medicare_levy_2014(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2014 = 445;
constexpr double LITO_1ST_TAPER_2014 = -0.015;
constexpr double LITO_1ST_THRESH_2014 = 37000;
constexpr double SAPTO_MAX_SINGLE_2014 = 2230;
constexpr double SAPTO_MAX_MARRIED_2014 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2014 = 2040;
constexpr double SAPTO_TAPER_2014 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2014 = 32279;
constexpr double SAPTO_LWR_MARRIED_2014 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2014 = 28974;
#endif
