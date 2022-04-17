#ifndef grattan_2017_H
#define grattan_2017_H

constexpr double ORD_TAX_BRACK_2017[5] = {0, 18200, 37e3, 87e3, 180e3};
constexpr double ORD_TAX_RATES_2017[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2017 = 21665;
constexpr double ML_UPR_THRESHOLD_SINGLE_2017 = 27083;
constexpr double ML_LWR_THRESHOLD_FAMILY_2017 = 36541;
constexpr double ML_UPR_THRESHOLD_FAMILY_2017 = 45676;
constexpr double ML_LWR_THR_UP_PER_CHILD_2017 =  3356;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2017 = 34244;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2017 = 42806;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2017 = 47670;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2017 = 59589;
constexpr double ML_TAPER_2017 = 0.1;
constexpr double ML_RATE_2017 = 0.02;
double do_1_medicare_levy_2017(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2017 = 445;
constexpr double LITO_1ST_TAPER_2017 = -0.015;
constexpr double LITO_1ST_THRESH_2017 = 37000;
constexpr double SAPTO_MAX_SINGLE_2017 = 2230;
constexpr double SAPTO_MAX_MARRIED_2017 = 1602;
constexpr double SAPTO_MAX_ILL_SEP_2017 = 2040;
constexpr double SAPTO_TAPER_2017 = -0.125;
constexpr double SAPTO_LWR_SINGLE_2017 = 32279;
constexpr double SAPTO_LWR_MARRIED_2017 = 28974;
constexpr double SAPTO_LWR_ILL_SEP_2017 = 28974;
constexpr double SBTO_DISCOUNT_2017 = 0.08;
#endif
