#ifndef grattan_2011_H
#define grattan_2011_H

constexpr int FLOOD_LEVY_1ST_THRESH_2011 = 50e3;
constexpr int FLOOD_LEVY_2ND_THRESH_2011 = 100e3;
constexpr double FLOOD_LEVY_TAPER_2011 = 0.005;


constexpr double ORD_TAX_BRACK_2011[5] = {0, 6000, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2011[5] = {0, 0.15, 0.3, 0.37, 0.45};
constexpr double ML_LWR_THRESHOLD_SINGLE_2011 = 18839;
constexpr double ML_UPR_THRESHOLD_SINGLE_2011 = 22165;
constexpr double ML_LWR_THRESHOLD_FAMILY_2011 = 31789;
constexpr double ML_UPR_THRESHOLD_FAMILY_2011 = 37400;
constexpr double ML_LWR_THR_UP_PER_CHILD_2011 =  2919;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2011 = 30685;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2011 = 36101;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2011 = 44500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2011 = 52354;
constexpr double ML_TAPER_2011 = 0.1;
constexpr double ML_RATE_2011 = 0.015;
double do_1_medicare_levy_2011(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
constexpr double LITO_MAX_OFFSET_2011 = 1500;
constexpr double LITO_1ST_TAPER_2011 = -0.04;
constexpr double LITO_1ST_THRESH_2011 = 30000;
constexpr double SAPTO_MAX_SINGLE_2011 = 2230;
constexpr double SAPTO_LWR_SINGLE_2011 = 30685;
constexpr double SAPTO_UPR_SINGLE_2011 = 48525;
constexpr double SAPTO_MAX_MARRIED_2011 = 2230;
constexpr double SAPTO_LWR_MARRIED_2011 = 30685;
constexpr double SAPTO_UPR_MARRIED_2011 = 48525;
constexpr double SAPTO_TAPER_2011 = -0.125;
#endif
