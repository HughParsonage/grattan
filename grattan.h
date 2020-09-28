#ifndef grattan_H
#define grattan_H

#include <Rcpp.h>
using namespace Rcpp;

double sapto_rcpp_singleton(double rebate_income,double max_offset,double lower_threshold,double taper_rate,bool sapto_eligible,double Spouse_income,bool is_married);


constexpr double ORD_TAX_BRACK_1990[6] = {0, 5100, 17650, 20600, 35e3, 50e3};
constexpr double ORD_TAX_RATES_1990[6] = {0, 0.21, 0.29, 0.39, 0.47, 0.48};
constexpr double ORD_TAX_BRACK_1991[8] = {0, 5250, 17650, 20600, 20700, 35e3, 36e3, 50e3};
constexpr double ORD_TAX_RATES_1991[8] = {0, 0.205, 0.245, 0.295, 0.385, 0.425, 0.465, 0.47};
constexpr double ORD_TAX_BRACK_1992[5] = {0, 5400, 20700, 36e3, 50e3};
constexpr double ORD_TAX_RATES_1992[5] = {0, 0.2, 0.38, 0.46, 0.47};
constexpr double ORD_TAX_BRACK_1993[5] = {0, 5400, 20700, 36e3, 50e3};
constexpr double ORD_TAX_RATES_1993[5] = {0, 0.2, 0.38, 0.46, 0.47};
constexpr double ORD_TAX_BRACK_1994[6] = {0, 5400, 20700, 36e3, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1994[6] = {0, 0.2, 0.355, 0.385, 0.44125, 0.47};
constexpr double ORD_TAX_BRACK_1995[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1995[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ORD_TAX_BRACK_1996[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1996[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ORD_TAX_BRACK_1997[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1997[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ORD_TAX_BRACK_1998[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1998[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ORD_TAX_BRACK_1999[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_1999[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ORD_TAX_BRACK_2000[5] = {0, 5400, 20700, 38e3, 50e3};
constexpr double ORD_TAX_RATES_2000[5] = {0, 0.2, 0.34, 0.43, 0.47};
constexpr double ORD_TAX_BRACK_2001[5] = {0, 6000, 20e3, 50e3, 60e3};
constexpr double ORD_TAX_RATES_2001[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ORD_TAX_BRACK_2002[5] = {0, 6000, 20e3, 50e3, 60e3};
constexpr double ORD_TAX_RATES_2002[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ORD_TAX_BRACK_2003[5] = {0, 6000, 20e3, 50e3, 60e3};
constexpr double ORD_TAX_RATES_2003[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ORD_TAX_BRACK_2004[5] = {0, 6000, 21600, 52e3, 62500};
constexpr double ORD_TAX_RATES_2004[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ORD_TAX_BRACK_2005[5] = {0, 6000, 21600, 58e3, 70e3};
constexpr double ORD_TAX_RATES_2005[5] = {0, 0.17, 0.3, 0.42, 0.47};
constexpr double ORD_TAX_BRACK_2006[5] = {0, 6000, 21600, 63e3, 95e3};
constexpr double ORD_TAX_RATES_2006[5] = {0, 0.15, 0.3, 0.42, 0.47};
constexpr double ORD_TAX_BRACK_2007[5] = {0, 6000, 25e3, 75e3, 150e3};
constexpr double ORD_TAX_RATES_2007[5] = {0, 0.15, 0.3, 0.4, 0.45};
constexpr double ORD_TAX_BRACK_2008[5] = {0, 6000, 30e3, 75e3, 150e3};
constexpr double ORD_TAX_RATES_2008[5] = {0, 0.15, 0.3, 0.4, 0.45};
constexpr double ORD_TAX_BRACK_2009[5] = {0, 6000, 34e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2009[5] = {0, 0.15, 0.3, 0.4, 0.45};
constexpr double ORD_TAX_BRACK_2010[5] = {0, 6000, 35e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2010[5] = {0, 0.15, 0.3, 0.38, 0.45};
constexpr double ORD_TAX_BRACK_2011[5] = {0, 6000, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2011[5] = {0, 0.15, 0.3, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2012[5] = {0, 6000, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2012[5] = {0, 0.15, 0.3, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2013[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2013[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2014[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2014[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2015[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2015[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2016[5] = {0, 18200, 37e3, 80e3, 180e3};
constexpr double ORD_TAX_RATES_2016[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2017[5] = {0, 18200, 37e3, 87e3, 180e3};
constexpr double ORD_TAX_RATES_2017[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2018[5] = {0, 18200, 37e3, 87e3, 180e3};
constexpr double ORD_TAX_RATES_2018[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2019[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2019[5] = {0, 0.19, 0.325, 0.37, 0.45};
constexpr double ORD_TAX_BRACK_2020[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2020[5] = {0, 0.19, 0.325, 0.37, 0.45};
// above won't change, but future years may so we break them into paragraphs

constexpr double ORD_TAX_BRACK_2021[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2021[5] = {0, 0.19, 0.325, 0.37, 0.45};

// Current year is 2021
constexpr int CURRENT_YEAR = 2021;
constexpr double ORD_TAX_BRACK_CURRENT_YEAR[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_CURRENT_YEAR[5] = {0, 0.19, 0.325, 0.37, 0.45};

constexpr double ORD_TAX_BRACK_2022[5] = {0, 18200, 37e3, 90e3, 180e3};
constexpr double ORD_TAX_RATES_2022[5] = {0, 0.19, 0.325, 0.37, 0.45};

constexpr double ORD_TAX_BRACK_2023[5] = {0, 18200, 45e3, 120e3, 180e3};
constexpr double ORD_TAX_RATES_2023[5] = {0, 0.190, 0.325, 0.370, 0.450};

constexpr double ORD_TAX_BRACK_2024[5] = {0, 18200, 45e3, 120e3, 180e3};
constexpr double ORD_TAX_RATES_2024[5] = {0, 0.190, 0.325, 0.370, 0.450};

constexpr double ORD_TAX_BRACK_2025[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2025[4] = {0, 0.190, 0.300, 0.450};

constexpr double ORD_TAX_BRACK_2026[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2026[4] = {0, 0.190, 0.300, 0.450};

constexpr double ORD_TAX_BRACK_2027[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2027[4] = {0, 0.190, 0.300, 0.450};

constexpr double ORD_TAX_BRACK_2028[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2028[4] = {0, 0.190, 0.300, 0.450};

constexpr double ORD_TAX_BRACK_2029[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2029[4] = {0, 0.190, 0.300, 0.450};

constexpr double ORD_TAX_BRACK_2030[4] = {0, 18200, 45e3, 200e3};
constexpr double ORD_TAX_RATES_2030[4] = {0, 0.190, 0.300, 0.450};



// SAPTO parameters
constexpr int SAPTO_MAX_SINGLE_2013_____ = 2230;
constexpr int SAPTO_THRESH_SINGLE_2013_2022 = 32279;
constexpr int SAPTO_THRESH_SINGLE_2023_____ = 33622;
constexpr int SAPTO_MAX_MARRIED_2013_____ = 1604 * 2; // married couple
constexpr int SAPTO_THRESH_MARRIED_2013_2022 = 28974 * 2;
constexpr int SAPTO_THRESH_MARRIED_2023_____ = 30316 * 2;
constexpr double SAPTO_TAPER = -0.125;



// MOD vars
constexpr int YEAR4[21] = { 910, 1011, 1112, 1213, 1314, 1415, 1516, 1617, 1718, 1819, 
                       1920, 2021, 2122, 2223, 2324, 2425, 2526, 2627, 2728, 2829, 2930};

constexpr int SAPTO_MAX_SING[21]       = {0, 0, 0, 0, 0,  2230,  2230,  2230,  2230,  2230,  2230,  2230,  2230,  2230,  2230,  2230,  2230 , 2230,  2230,  2230, 2230};
constexpr int SAPTO_THRESH_SING[21]    = {0, 0, 0, 0, 0, 32279, 32279, 32279, 32279, 32279, 32279, 32279, 32279, 33622, 33622, 33622, 33622, 33622, 33622, 33622, 33622};
constexpr int SAPTO_MAX_MARR[21]       = {0, 0, 0, 0, 0,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602};
constexpr int SAPTO_THRESH_MARR[21]    = {0, 0, 0, 0, 0, 28974, 28974, 28974, 28974, 28974, 28974, 28974, 28974, 30316, 30316, 30316, 30316, 30316, 30316, 30316, 30316};
constexpr int SAPTO_MAX_ILL_SEP[21]    = {0, 0, 0, 0, 0,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040,  2040};
constexpr int SAPTO_THRESH_ILL_SEP[21] = {0, 0, 0, 0, 0, 31279, 31279, 31279, 31279, 31279, 31279, 31279, 31279, 32622, 32622, 32622, 32622, 32622, 32622, 32622, 32622};

constexpr int ML_THRESH_SING[21]        = {0, 0, 0, 0, 0, 20896, 21335, 21655, 21980, 22398, 22398, 22398, 22398, 22398, 22398, 22398, 22398, 22398, 22398, 22398, 22398};
constexpr int ML_THRESH_MARR[21]        = {0, 0, 0, 0, 0, 35261, 36001, 36541, 37089, 37794, 37794, 37794, 37794, 37794, 37794, 37794, 37794, 37794, 37794, 37794, 37794};
constexpr int ML_THRESH_EXTRA_CHILD[21] = {0, 0, 0, 0, 0,  3238,  3306,  3356,  3406,  3471,  3471,  3471,  3471,  3471,  3471,  3471,  3471,  3471,  3471,  3471,  3471};
constexpr int ML_THRESH_SING_SAPTO[21]  = {0, 0, 0, 0, 0, 33044, 33738, 34244, 34758, 35418, 35418, 35418, 35418, 35418, 35418, 35418, 35418, 35418, 35418, 35418, 35418};
constexpr int ML_THRESH_MARR_SAPTO[21]  = {0, 0, 0, 0, 0, 46000, 46966, 47670, 48385, 49304, 49304, 49304, 49304, 49304, 49304, 49304, 49304, 49304, 49304, 49304, 49304};
constexpr double ML_RATE[21]            = {0, 0, 0, 0, 0, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02};
constexpr double ML_SHADE_IN_RATE[21]   = {0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1};

constexpr int TAX_STEP1PRO_RATE[21] = {0, 0, 0, 0, 0, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736, 4736};

constexpr double LITO_MAX_OFFSET_2001_2012[12] = {  150,   150,   150,   235,   235,   235,   600,   750,  1200,  1350,  1500,  1500};
constexpr double LITO_1ST_THRESH_2001_2012[12] = {20700, 20700, 18575, 21600, 21600, 21600, 25000, 30000, 30000, 30000, 30000, 30000};
constexpr double LITO_TAPER_RATE_2001_2012 = 0.04;

constexpr double LITO_MAX_OFFSET_2013_2022 = 445;
constexpr double LITO_MAX_OFFSET_2023_____ = 700;
constexpr double LITO_1ST_THRESH_2013_2022 = 37000;
constexpr double LITO_1ST_THRESH_2023_____ = 37500;
constexpr double LITO_2ND_THRESH_2023_____ = 45000;
constexpr double LITO_TAPER_RATE_2013_2022 = -0.015;
constexpr double LITO_1ST_TAPER_2023_____ = -0.050;
constexpr double LITO_2ND_TAPER_2023_____ = -0.015;


constexpr double LMITO_1ST_OFFSET = 255;
constexpr double LMITO_2ND_LEVEL = 1080;
constexpr double LMITO_THRESHOLDS[4] = {37e3, 48e3, 90e3, 126e3};
constexpr double LMITO_TAPER_RATES[4] = {0.075, 0, -0.03, 0};

constexpr int TEMP_BUDGET_REPAIR_LEVY_THRESH = 180000;
constexpr double TEMP_BUDGET_REPAIR_LEVY_RATE = 0.02;





// 2010 to 2030
constexpr int SBITO_THRESH[21] = {0, 0, 0, 0, 0, 0, 2000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000, 5000000};

constexpr int MLS_THRESH_SING1[21] = {0, 0, 0, 0, 0, 90000, 90000, 90000, 90000, 90000, 90000, 90000, 93000, 96000, 99000, 102000, 106000, 110000, 114000, 118000, 122000};
constexpr int MLS_THRESH_SING2[21] = {0, 0, 0, 0, 0, 105000, 105000, 105000, 105000, 105000, 105000, 105000, 108000, 111000, 115000, 119000, 123000, 127000, 132000, 137000, 142000};
constexpr int MLS_THRESH_SING3[21] = {0, 0, 0, 0, 0, 140000, 140000, 140000, 140000, 140000, 140000, 140000, 145000, 150000, 155000, 161000, 167000, 173000, 179000, 186000, 193000};
constexpr int MLS_THRESH_MARR1[21] = {0, 0, 0, 0, 0, 180000, 180000, 180000, 180000, 180000, 180000, 180000, 186000, 192000, 198000, 204000, 212000, 220000, 228000, 236000, 244000};
constexpr int MLS_THRESH_MARR2[21] = {0, 0, 0, 0, 0, 210000, 210000, 210000, 210000, 210000, 210000, 210000, 216000, 222000, 230000, 238000, 246000, 254000, 264000, 274000, 284000};
constexpr int MLS_THRESH_MARR3[21] = {0, 0, 0, 0, 0, 280000, 280000, 280000, 280000, 280000, 280000, 280000, 290000, 300000, 310000, 322000, 334000, 346000, 358000, 372000, 386000};
constexpr int MLS_THRESH_EXTRA_CHILD[21] = {0, 0, 0, 0, 0, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500, 1500};
constexpr double MLS_RATE1[21] = {0, 0, 0, 0, 0, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01};
constexpr double MLS_RATE2[21] = {0, 0, 0, 0, 0, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125};
constexpr double MLS_RATE3[21] = {0, 0, 0, 0, 0, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015};

constexpr double TBRLRATE[21] = {0, 0, 0, 0, 0, 0.02, 0.02, 0.02, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

constexpr int SUPER_ECAP[21] = {25000, 25000, 25000, 25000, 25000, 30000, 30000, 30000, 25000, 25000, 25000, 25000, 27500, 27500, 30000, 30000, 32500, 32500, 35000, 35000, 37500};
constexpr int SUPER_ECAP_LAG1[21] = {0, 25000, 25000, 25000, 25000, 25000, 30000, 30000, 30000, 25000, 25000, 25000, 25000, 27500, 27500, 30000, 30000, 32500, 32500, 35000, 35000};
constexpr int SUPER_ECAP_LAG2[21] = {0, 0, 25000, 25000, 25000, 25000, 25000, 30000, 30000, 30000, 25000, 25000, 25000, 25000, 27500, 27500, 30000, 30000, 32500, 32500, 35000};
constexpr int SUPER_ECAP_LAG3[21] = {0, 0, 0, 25000, 25000, 25000, 25000, 25000, 30000, 30000, 30000, 25000, 25000, 25000, 25000, 27500, 27500, 30000, 30000, 32500, 32500};
constexpr int SUPER_ECAP_LAG4[21] = {0, 0, 0, 0, 25000, 25000, 25000, 25000, 25000, 30000, 30000, 30000, 25000, 25000, 25000, 25000, 27500, 27500, 30000, 30000, 32500};
constexpr int SUPER_ECAP_LAG5[21] = {0, 0, 0, 0, 0, 25000, 25000, 25000, 25000, 25000, 30000, 30000, 30000, 25000, 25000, 25000, 25000, 27500, 27500, 30000, 30000};
constexpr int SUPER_CATCH_UP_CAP[21] = {0, 0, 0, 0, 10000, 5000, 5000, 5000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CATCH_UP_CAP_LAG1[21] = {0, 0, 0, 0, 0, 10000, 5000, 5000, 5000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CATCH_UP_CAP_LAG2[21] = {0, 0, 0, 0, 0, 0, 10000, 5000, 5000, 5000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CATCH_UP_CAP_LAG3[21] = {0, 0, 0, 0, 0, 0, 0, 10000, 5000, 5000, 5000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CATCH_UP_CAP_LAG4[21] = {0, 0, 0, 0, 0, 0, 0, 0, 10000, 5000, 5000, 5000, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CATCH_UP_CAP_LAG5[21] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 10000, 5000, 5000, 5000, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CAP_AGE[21] = {50, 50, 50, 50, 60, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CAP_AGE_LAG1[21] = {0, 50, 50, 50, 50, 60, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CAP_AGE_LAG2[21] = {0, 0, 50, 50, 50, 50, 60, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CAP_AGE_LAG3[21] = {0, 0, 0, 50, 50, 50, 50, 60, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CAP_AGE_LAG4[21] = {0, 0, 0, 0, 50, 50, 50, 50, 60, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_CAP_AGE_LAG5[21] = {0, 0, 0, 0, 0, 50, 50, 50, 50, 60, 50, 50, 50, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr int SUPER_UNDED_CONT_CAP[21] = {0, 0, 0, 0, 0, 180000, 180000, 180000, 100000, 100000, 100000, 100000, 110000, 110000, 120000, 120000, 130000, 130000, 140000, 140000, 150000};
constexpr double SUPER_TCONT[21] = {0, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15};
constexpr int SUPER_ECTRATE[21] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
constexpr double SUPER_GRATE[21] = {0, 0, 0, 0, 0, 0.095, 0.095, 0.095, 0.095, 0.095, 0.095, 0.095, 0.1, 0.105, 0.11, 0.115, 0.12, 0.12, 0.12, 0.12, 0.12};

constexpr int LISCMAX[21] = {0, 0, 0, 0, 0, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500, 500};
constexpr int LISCMIN[21] = {0, 0, 0, 0, 0, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10};
constexpr int LISCTHRESH[21] = {0, 0, 0, 0, 0, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000};

constexpr int SUPER_SURCHARGE_CAP[21] = {0, 0, 0, 0, 0, 300000, 300000, 300000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000, 250000};
constexpr double SUPER_SURCHARGE_RATE[21] = {0, 0, 0, 0, 0, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.15};
constexpr int SUPER_LOW_RATE_AMT[21] = {0, 0, 0, 0, 0, 185000, 195000, 195000, 200000, 205000, 210000, 215000, 220000, 230000, 240000, 250000, 260000, 270000, 280000, 290000, 300000};
constexpr int SSCTOTHRESH[21] = {0, 0, 0, 0, 0, 10800, 10800, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000, 37000};
constexpr int SSCTOMAX_CONTS[21] = {0, 0, 0, 0, 0, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000, 3000};
constexpr double SSCTOOFFSET_RATE[21] = {0, 0, 0, 0, 0, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18, 0.18};
constexpr int SUPER_CARRY_FWD_CAP[21] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 500000, 500000, 500000, 500000, 500000, 500000, 500000, 500000, 500000, 500000, 500000, 500000};

constexpr int MAX_SG_BASE_SALARY[21] = {0, 0, 0, 0, 0, 49430, 50810, 51620, 52760, 54030, 55000, 56540, 58410, 60570, 62750, 65070, 67670, 67700, 70410, 72910, 75830};

constexpr double SUPER_DED_INC_PCT[21] = {0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
constexpr int SUPER_DED_BAL_LIMIT[21] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

constexpr double PEN_AGE[21] = {0, 0, 0, 0, 0, 65, 65, 65, 65.5, 65.5, 66, 66, 66.5, 66.5, 67, 67, 67, 67, 67, 67, 67};

constexpr double FBTRATE[21] = {0, 0, 0, 0, 0, 0.47, 0.49, 0.49, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47, 0.47};

constexpr int AGE_PRESERVATION[21] = {0, 0, 0, 0, 0, 55, 56, 56, 57, 57, 58, 58, 59, 59, 60, 60, 60, 60, 60, 60, 60};




#endif
