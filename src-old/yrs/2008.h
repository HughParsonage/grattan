#ifndef grattan_2008_H
#define grattan_2008_H

constexpr double ORD_TAX_BRACK_2008[5] = {0, 6000, 30e3, 75e3, 150e3};
constexpr double ORD_TAX_RATES_2008[5] = {0, 0.15, 0.3, 0.4, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2008 = 17309;
constexpr double ML_UPR_THRESHOLD_SINGLE_2008 = 20365;
constexpr double ML_LWR_THRESHOLD_FAMILY_2008 = 29207;
constexpr double ML_UPR_THRESHOLD_FAMILY_2008 = 34362;
constexpr double ML_LWR_THR_UP_PER_CHILD_2008 =  2682;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2008 = 25867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2008 = 30433;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2008 = 33500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2008 = 39413;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2008 = 22922;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2008 = 33500;
constexpr double ML_TAPER_2008 = 0.1;
constexpr double ML_RATE_2008 = 0.015;
double do_1_medicare_levy_2008(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2008 = 750;
constexpr double LITO_1ST_TAPER_2008 = -0.04;
constexpr double LITO_1ST_THRESH_2008 = 30000;
constexpr double SAPTO_MAX_MARRIED_2008 = 3204;
constexpr double SAPTO_LWR_MARRIED_2008 = 41360;
constexpr double SAPTO_UPR_MARRIED_2008 = 66992;
constexpr double SAPTO_MAX_SINGLE_2008 = 2230;
constexpr double SAPTO_LWR_SINGLE_2008 = 24867;
constexpr double SAPTO_UPR_SINGLE_2008 = 42707;
constexpr double SAPTO_TAPER_2008 = -0.125;
#endif
