#ifndef grattan_2007_H
#define grattan_2007_H

constexpr double ORD_TAX_BRACK_2007[5] = {0, 6000, 25e3, 75e3, 150e3};
constexpr double ORD_TAX_RATES_2007[5] = {0, 0.15, 0.3, 0.4, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2007 = 16740;
constexpr double ML_UPR_THRESHOLD_SINGLE_2007 = 19695;
constexpr double ML_LWR_THRESHOLD_FAMILY_2007 = 28247;
constexpr double ML_UPR_THRESHOLD_FAMILY_2007 = 33233;
constexpr double ML_LWR_THR_UP_PER_CHILD_2007 =  2594;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2007 = 24867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2007 = 29256;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2007 = 33500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2007 = 39413;
constexpr double ML_LWR_THRESHOLD_SINGLE_PTO_2007 = 21637;
constexpr double ML_LWR_THRESHOLD_FAMILY_PTO_2007 = 33500;
constexpr double ML_TAPER_2007 = 0.1;
constexpr double ML_RATE_2007 = 0.015;
double do_1_medicare_levy_2007(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2007 = 600;
constexpr double LITO_1ST_TAPER_2007 = -0.04;
constexpr double LITO_1ST_THRESH_2007 = 25000;
constexpr double SAPTO_MAX_MARRIED_2007 = 3204;
constexpr double SAPTO_LWR_MARRIED_2007 = 43360;
constexpr double SAPTO_UPR_MARRIED_2007 = 66992;
constexpr double SAPTO_MAX_SINGLE_2007 = 2230;
constexpr double SAPTO_LWR_SINGLE_2007 = 24867;
constexpr double SAPTO_UPR_SINGLE_2007 = 42707;
constexpr double SAPTO_TAPER_2007 = -0.125;
#endif
