#ifndef grattan_2002_H
#define grattan_2002_H

constexpr double ORD_TAX_BRACK_2002[5] = {0, 6000, 20e3, 50e3, 60e3};
constexpr double ORD_TAX_RATES_2002[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ML_LWR_THRESHOLD_SINGLE_2002 = 14540;
constexpr double ML_UPR_THRESHOLD_SINGLE_2002 = 15718;
constexpr double ML_LWR_THRESHOLD_FAMILY_2002 = 24534;
constexpr double ML_UPR_THRESHOLD_FAMILY_2002 = 26524;
constexpr double ML_LWR_THR_UP_PER_CHILD_2002 =  2253;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2002 = 20000;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2002 = 21623;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2002 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2002 = 34303;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2002 = 16570;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2002 = 31729;
constexpr double ML_TAPER_2002 = 0.20;
constexpr double ML_RATE_2002 = 0.015;
double do_1_medicare_levy_2002(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2002 = 150;
constexpr double LITO_1ST_TAPER_2002 = -0.04;
constexpr double LITO_1ST_THRESH_2002 = 20700;
constexpr double SAPTO_MAX_SINGLE_2002 = 1710;
constexpr double SAPTO_MAX_MARRIED_2002 = 2490;
constexpr double SAPTO_LWR_SINGLE_2002 = 16059;
constexpr double SAPTO_LWR_MARRIED_2002 = 26648;
constexpr double SAPTO_TAPER_2002 = -0.125;

#endif
