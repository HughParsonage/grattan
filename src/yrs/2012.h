#ifndef grattan_2012_H
#define grattan_2012_H

constexpr double ORD_TAX_BRACK_2012[5] = {0, 6000, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2012[5] = {0, 0.15, 0.3, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2012 = 19404;
constexpr double ML_UPR_THRESHOLD_SINGLE_2012 = 22829;
constexpr double ML_LWR_THRESHOLD_FAMILY_2012 = 32743;
constexpr double ML_UPR_THRESHOLD_FAMILY_2012 = 38522;
constexpr double ML_LWR_THR_UP_PER_CHILD_2012 =  3007;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2012 = 30685;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2012 = 36101;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2012 = 44500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2012 = 52354;
constexpr double ML_TAPER_2012 = 0.1;
constexpr double ML_RATE_2012 = 0.015;
double do_1_medicare_levy_2012(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2012 = 1500;
constexpr double LITO_1ST_TAPER_2012 = -0.04;
constexpr double LITO_1ST_THRESH_2012 = 30000;
constexpr double SAPTO_MAX_SINGLE_2012 = 2230;
constexpr double SAPTO_LWR_SINGLE_2012 = 30685;
constexpr double SAPTO_UPR_SINGLE_2012 = 48525;
constexpr double SAPTO_MAX_MARRIED_2012 = 2230;
constexpr double SAPTO_LWR_MARRIED_2012 = 30685;
constexpr double SAPTO_UPR_MARRIED_2012 = 48525;
constexpr double SAPTO_TAPER_2012 = -0.125;
#endif
