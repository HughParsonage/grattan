#ifndef grattanMedicareLevy_H
#define grattanMedicareLevy_H

#include "grattan.h"
// Global constants for medicare levy functions
// It's seductive to make these all functions, and 
// that may prove to be the better options. However,
// there are two reasons that made global constants the
// preferred choice. First, they really are global 
// constants -- they'll never change. Second, if the 
// logic of the medicare functons changes (as it has)
// the functions will probably need to be rewritten, 
// and then you have awkward switch statements alongside
// your switch(yr) statements.

constexpr double ML_RATE_1984_____[64] =
  // 1984
  {0.0125,
   // 1985
   0.0125, 0.0125, 0.0125, 0.0125, 0.0125,
   // 1990
   0.0125, 0.0125, 0.0125, 0.0125, 0.0140,
   // 1995
   0.014, 0.015, 0.015, 0.017, 0.015, 
   // 2000
   0.015, 0.015, 0.015, 0.015, 0.015, 
   // 2005
   0.015, 0.015, 0.015, 0.015, 0.015, 
   // 2010
   0.015, 0.015, 0.015, 0.015, 0.015,
   // 2015
   0.02, 0.02, 0.02, 0.02, 0.02, 
   // 2020
   0.02, 0.02, 0.02, 0.02, 0.02, 
   // 2025
   0.02, 0.02, 0.02, 0.02, 0.02, 0.02};

constexpr double ML_TAPER_1984_____[64] =
  // 1984
  {0.25,
   // 1985
   0.25, 0.25, 0.25, 0.25, 0.25,
   // 1990
   0.25, 0.25, 0.25, 0.25, 0.20,
   // 1995
   0.20, 0.20, 0.20, 0.20, 0.20, 
   // 2000
   0.20, 0.20, 0.20, 0.20, 0.20, 
   // 2005
   0.10, 0.10, 0.10, 0.10, 0.10, 
   // 2010
   0.10, 0.10, 0.10, 0.10, 0.10, 
   // 2015
   0.10, 0.10, 0.10, 0.10, 0.10, 
   // 2020
   0.10, 0.10, 0.10, 0.10, 0.10, 
   // 2025
   0.10, 0.10, 0.10, 0.10, 0.10, 0.1};

constexpr double ML_LWR_THRESHOLD_SINGLE_1984 =  6699;
constexpr double ML_UPR_THRESHOLD_SINGLE_1984 =  7051;
constexpr double ML_LWR_THRESHOLD_SINGLE_1985 =  7111;
constexpr double ML_UPR_THRESHOLD_SINGLE_1985 =  7485;
constexpr double ML_LWR_THRESHOLD_SINGLE_1986 =  7527;
constexpr double ML_UPR_THRESHOLD_SINGLE_1986 =  7923;
constexpr double ML_LWR_THRESHOLD_SINGLE_1987 =  8031;
constexpr double ML_UPR_THRESHOLD_SINGLE_1987 =  8518;
constexpr double ML_LWR_THRESHOLD_SINGLE_1988 =  8981;
constexpr double ML_UPR_THRESHOLD_SINGLE_1988 =  9579;
constexpr double ML_LWR_THRESHOLD_SINGLE_1989 =  9561;
constexpr double ML_UPR_THRESHOLD_SINGLE_1989 = 10198;
constexpr double ML_LWR_THRESHOLD_SINGLE_1990 = 10331;
constexpr double ML_UPR_THRESHOLD_SINGLE_1990 = 11019;
constexpr double ML_LWR_THRESHOLD_SINGLE_1991 = 11746;
constexpr double ML_UPR_THRESHOLD_SINGLE_1991 = 12529;
constexpr double ML_LWR_THRESHOLD_SINGLE_1992 = 11746;
constexpr double ML_UPR_THRESHOLD_SINGLE_1992 = 12529;
constexpr double ML_LWR_THRESHOLD_SINGLE_1993 = 11888;
constexpr double ML_UPR_THRESHOLD_SINGLE_1993 = 12680;
constexpr double ML_LWR_THRESHOLD_SINGLE_1994 = 12689;
constexpr double ML_UPR_THRESHOLD_SINGLE_1994 = 13643;
constexpr double ML_LWR_THRESHOLD_SINGLE_1995 = 12689;
constexpr double ML_UPR_THRESHOLD_SINGLE_1995 = 13643;
constexpr double ML_LWR_THRESHOLD_SINGLE_1996 = 12871;
constexpr double ML_UPR_THRESHOLD_SINGLE_1996 = 13913;
constexpr double ML_LWR_THRESHOLD_SINGLE_1997 = 13128;
constexpr double ML_UPR_THRESHOLD_SINGLE_1997 = 14346;
constexpr double ML_LWR_THRESHOLD_SINGLE_1998 = 13390;
constexpr double ML_UPR_THRESHOLD_SINGLE_1998 = 14474;
constexpr double ML_LWR_THRESHOLD_SINGLE_1999 = 13390;
constexpr double ML_UPR_THRESHOLD_SINGLE_1999 = 14474;
constexpr double ML_LWR_THRESHOLD_SINGLE_2000 = 13551;
constexpr double ML_UPR_THRESHOLD_SINGLE_2000 = 14648;
constexpr double ML_LWR_THRESHOLD_SINGLE_2001 = 13808;
constexpr double ML_UPR_THRESHOLD_SINGLE_2001 = 14927;
constexpr double ML_LWR_THRESHOLD_SINGLE_2002 = 14540;
constexpr double ML_UPR_THRESHOLD_SINGLE_2002 = 15718;
constexpr double ML_LWR_THRESHOLD_SINGLE_2003 = 15062;
constexpr double ML_UPR_THRESHOLD_SINGLE_2003 = 16284;
constexpr double ML_LWR_THRESHOLD_SINGLE_2004 = 15529;
constexpr double ML_UPR_THRESHOLD_SINGLE_2004 = 16789;
constexpr double ML_LWR_THRESHOLD_SINGLE_2005 = 15902;
constexpr double ML_UPR_THRESHOLD_SINGLE_2005 = 17192;
constexpr double ML_LWR_THRESHOLD_SINGLE_2006 = 16284;
constexpr double ML_UPR_THRESHOLD_SINGLE_2006 = 17605;
constexpr double ML_LWR_THRESHOLD_SINGLE_2007 = 16740;
constexpr double ML_UPR_THRESHOLD_SINGLE_2007 = 19695;
constexpr double ML_LWR_THRESHOLD_SINGLE_2008 = 17309;
constexpr double ML_UPR_THRESHOLD_SINGLE_2008 = 20365;
constexpr double ML_LWR_THRESHOLD_SINGLE_2009 = 17794;
constexpr double ML_UPR_THRESHOLD_SINGLE_2009 = 20935;
constexpr double ML_LWR_THRESHOLD_SINGLE_2010 = 18488;
constexpr double ML_UPR_THRESHOLD_SINGLE_2010 = 21752;
constexpr double ML_LWR_THRESHOLD_SINGLE_2011 = 18839;
constexpr double ML_UPR_THRESHOLD_SINGLE_2011 = 22165;
constexpr double ML_LWR_THRESHOLD_SINGLE_2012 = 19404;
constexpr double ML_UPR_THRESHOLD_SINGLE_2012 = 22829;
constexpr double ML_LWR_THRESHOLD_SINGLE_2013 = 20542;
constexpr double ML_UPR_THRESHOLD_SINGLE_2013 = 24168;
constexpr double ML_LWR_THRESHOLD_SINGLE_2014 = 20542;
constexpr double ML_UPR_THRESHOLD_SINGLE_2014 = 24168;
constexpr double ML_LWR_THRESHOLD_SINGLE_2015 = 20896;
constexpr double ML_UPR_THRESHOLD_SINGLE_2015 = 26121;
constexpr double ML_LWR_THRESHOLD_SINGLE_2016 = 21335;
constexpr double ML_UPR_THRESHOLD_SINGLE_2016 = 26670;
constexpr double ML_LWR_THRESHOLD_SINGLE_2017 = 21665;
constexpr double ML_UPR_THRESHOLD_SINGLE_2017 = 27083;
constexpr double ML_LWR_THRESHOLD_SINGLE_2018 = 21980;
constexpr double ML_UPR_THRESHOLD_SINGLE_2018 = 27476;
constexpr double ML_LWR_THRESHOLD_SINGLE_2019 = 22398;
constexpr double ML_UPR_THRESHOLD_SINGLE_2019 = 27999;
constexpr double ML_LWR_THRESHOLD_SINGLE_2020 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2020 = 28503;

constexpr double ML_LWR_THRESHOLD_SINGLE_2021 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2021 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2022 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2022 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2023 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2023 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2024 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2024 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2025 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2025 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2026 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2026 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2027 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2027 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2028 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2028 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2029 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2029 = 28503;
constexpr double ML_LWR_THRESHOLD_SINGLE_2030 = 22801;
constexpr double ML_UPR_THRESHOLD_SINGLE_2030 = 28503;


constexpr double ML_LWR_THRESHOLD_FAMILY_1990 = 17401;
constexpr double ML_UPR_THRESHOLD_FAMILY_1990 = 18560;
constexpr double ML_LWR_THR_UP_PER_CHILD_1990 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1991 = 19046;
constexpr double ML_UPR_THRESHOLD_FAMILY_1991 = 20314;
constexpr double ML_LWR_THR_UP_PER_CHILD_1991 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1992 = 19675;
constexpr double ML_UPR_THRESHOLD_FAMILY_1992 = 20986;
constexpr double ML_LWR_THR_UP_PER_CHILD_1992 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1993 = 20071;
constexpr double ML_UPR_THRESHOLD_FAMILY_1993 = 21408;
constexpr double ML_LWR_THR_UP_PER_CHILD_1993 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1994 = 21367;
constexpr double ML_UPR_THRESHOLD_FAMILY_1994 = 22974;
constexpr double ML_LWR_THR_UP_PER_CHILD_1994 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1995 = 21367;
constexpr double ML_UPR_THRESHOLD_FAMILY_1995 = 22974;
constexpr double ML_LWR_THR_UP_PER_CHILD_1995 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1996 = 21719;
constexpr double ML_UPR_THRESHOLD_FAMILY_1996 = 23478;
constexpr double ML_LWR_THR_UP_PER_CHILD_1996 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1997 = 22153;
constexpr double ML_UPR_THRESHOLD_FAMILY_1997 = 24209;
constexpr double ML_LWR_THR_UP_PER_CHILD_1997 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1998 = 22595;
constexpr double ML_UPR_THRESHOLD_FAMILY_1998 = 24695;
constexpr double ML_LWR_THR_UP_PER_CHILD_1998 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_1999 = 22595;
constexpr double ML_UPR_THRESHOLD_FAMILY_1999 = 24425;
constexpr double ML_LWR_THR_UP_PER_CHILD_1999 =  2100;
constexpr double ML_LWR_THRESHOLD_FAMILY_2000 = 22866;
constexpr double ML_UPR_THRESHOLD_FAMILY_2000 = 24718;
constexpr double ML_LWR_THR_UP_PER_CHILD_2000 =  2100;


constexpr double ML_LWR_THRESHOLD_FAMILY_2001 = 23300;
constexpr double ML_UPR_THRESHOLD_FAMILY_2001 = 25190;
constexpr double ML_LWR_THR_UP_PER_CHILD_2001 =  2140;
constexpr double ML_LWR_THRESHOLD_FAMILY_2002 = 24534;
constexpr double ML_UPR_THRESHOLD_FAMILY_2002 = 26524;
constexpr double ML_LWR_THR_UP_PER_CHILD_2002 =  2253;
constexpr double ML_LWR_THRESHOLD_FAMILY_2003 = 25417;
constexpr double ML_UPR_THRESHOLD_FAMILY_2003 = 27479;
constexpr double ML_LWR_THR_UP_PER_CHILD_2003 =  2334;
constexpr double ML_LWR_THRESHOLD_FAMILY_2004 = 26205;
constexpr double ML_UPR_THRESHOLD_FAMILY_2004 = 28331;
constexpr double ML_LWR_THR_UP_PER_CHILD_2004 =  2406;
constexpr double ML_LWR_THRESHOLD_FAMILY_2005 = 26834;
constexpr double ML_UPR_THRESHOLD_FAMILY_2005 = 29011;
constexpr double ML_LWR_THR_UP_PER_CHILD_2005 =  2464;

constexpr double ML_LWR_THRESHOLD_FAMILY_2006 = 27478;
constexpr double ML_UPR_THRESHOLD_FAMILY_2006 = 29707;
constexpr double ML_LWR_THR_UP_PER_CHILD_2006 =  2523;
constexpr double ML_LWR_THRESHOLD_FAMILY_2007 = 28247;
constexpr double ML_UPR_THRESHOLD_FAMILY_2007 = 33233;
constexpr double ML_LWR_THR_UP_PER_CHILD_2007 =  2594;
constexpr double ML_LWR_THRESHOLD_FAMILY_2008 = 29207;
constexpr double ML_UPR_THRESHOLD_FAMILY_2008 = 34362;
constexpr double ML_LWR_THR_UP_PER_CHILD_2008 =  2682;
constexpr double ML_LWR_THRESHOLD_FAMILY_2009 = 30025;
constexpr double ML_UPR_THRESHOLD_FAMILY_2009 = 35325;
constexpr double ML_LWR_THR_UP_PER_CHILD_2009 =  2757;
constexpr double ML_LWR_THRESHOLD_FAMILY_2010 = 31196;
constexpr double ML_UPR_THRESHOLD_FAMILY_2010 = 36702;
constexpr double ML_LWR_THR_UP_PER_CHILD_2010 =  2865;

constexpr double ML_LWR_THRESHOLD_FAMILY_2011 = 31789;
constexpr double ML_UPR_THRESHOLD_FAMILY_2011 = 37400;
constexpr double ML_LWR_THR_UP_PER_CHILD_2011 =  2919;
constexpr double ML_LWR_THRESHOLD_FAMILY_2012 = 32743;
constexpr double ML_UPR_THRESHOLD_FAMILY_2012 = 38522;
constexpr double ML_LWR_THR_UP_PER_CHILD_2012 =  3007;
constexpr double ML_LWR_THRESHOLD_FAMILY_2013 = 33693;
constexpr double ML_UPR_THRESHOLD_FAMILY_2013 = 39640;
constexpr double ML_LWR_THR_UP_PER_CHILD_2013 =  3094;
constexpr double ML_LWR_THRESHOLD_FAMILY_2014 = 34367;
constexpr double ML_UPR_THRESHOLD_FAMILY_2014 = 40433;
constexpr double ML_LWR_THR_UP_PER_CHILD_2014 =  3156;
constexpr double ML_LWR_THRESHOLD_FAMILY_2015 = 35261;
constexpr double ML_UPR_THRESHOLD_FAMILY_2015 = 44078;
constexpr double ML_LWR_THR_UP_PER_CHILD_2015 =  3238;

constexpr double ML_LWR_THRESHOLD_FAMILY_2016 = 35261;
constexpr double ML_UPR_THRESHOLD_FAMILY_2016 = 44078;
constexpr double ML_LWR_THR_UP_PER_CHILD_2016 =  3306;
constexpr double ML_LWR_THRESHOLD_FAMILY_2017 = 36541;
constexpr double ML_UPR_THRESHOLD_FAMILY_2017 = 45678;
constexpr double ML_LWR_THR_UP_PER_CHILD_2017 =  3356;
constexpr double ML_LWR_THRESHOLD_FAMILY_2018 = 37089;
constexpr double ML_UPR_THRESHOLD_FAMILY_2018 = 46363;
constexpr double ML_LWR_THR_UP_PER_CHILD_2018 =  3406;
constexpr double ML_LWR_THRESHOLD_FAMILY_2019 = 37794;
constexpr double ML_UPR_THRESHOLD_FAMILY_2019 = 47244;
constexpr double ML_LWR_THR_UP_PER_CHILD_2019 =  3471;
constexpr double ML_LWR_THRESHOLD_FAMILY_2020 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2020 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2020 =  3533;

constexpr double ML_LWR_THRESHOLD_FAMILY_2021 = 38474; // Constant from here on
constexpr double ML_UPR_THRESHOLD_FAMILY_2021 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2021 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2022 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2022 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2022 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2023 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2023 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2023 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2024 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2024 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2024 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2025 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2025 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2025 =  3533;

constexpr double ML_LWR_THRESHOLD_FAMILY_2026 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2026 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2026 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2027 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2027 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2027 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2028 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2028 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2028 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2029 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2029 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2029 =  3533;
constexpr double ML_LWR_THRESHOLD_FAMILY_2030 = 38474;
constexpr double ML_UPR_THRESHOLD_FAMILY_2030 = 48094;
constexpr double ML_LWR_THR_UP_PER_CHILD_2030 =  3533;


constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2001 = 20000;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2001 = 21623;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2002 = 20000;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2002 = 21623;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2003 = 20000;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2003 = 21623;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2004 = 20500;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2004 = 22163;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2005 = 20500;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2005 = 22163;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2006 = 21968;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2006 = 23750;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2007 = 24867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2007 = 29256;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2008 = 25867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2008 = 30433;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2009 = 28867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2009 = 33962;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2010 = 29867;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2010 = 35139;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2011 = 30685;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2011 = 36101;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2012 = 30685;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2012 = 36101;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2013 = 32279;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2013 = 37976;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2014 = 32279;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2014 = 37976;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2015 = 33044;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2015 = 41306;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2016 = 33738;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2016 = 42174;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2017 = 34244;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2017 = 42806;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2018 = 34758;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2018 = 43449;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2019 = 35418;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2019 = 44274;

constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2001 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2001 = 34303;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2002 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2002 = 34303;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2003 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2003 = 34303;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2004 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2004 = 34303;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2005 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2005 = 34303;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2006 = 31729;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2006 = 34303;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2007 = 33500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2007 = 39413;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2008 = 33500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2008 = 39413;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2009 = 42000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2009 = 49413;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2010 = 43500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2010 = 51178;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2011 = 44500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2011 = 52354;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2012 = 44500;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2012 = 52354;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2013 = 46000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2013 = 54119;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2014 = 46000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2014 = 54119;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2015 = 46000;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2015 = 57501;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2016 = 46966;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2016 = 58709;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2017 = 47670;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2017 = 59589;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2018 = 48385;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2018 = 60483;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2019 = 49304;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2019 = 61631;

constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2020 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2020 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2020 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2020 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2021 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2021 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2021 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2021 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2022 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2022 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2022 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2022 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2023 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2023 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2023 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2023 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2024 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2024 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2024 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2024 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2025 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2025 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2025 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2025 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2026 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2026 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2026 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2026 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2027 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2027 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2027 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2027 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2028 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2028 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2028 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2028 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2029 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2029 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2029 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2029 = 62740;
constexpr double ML_LWR_THRESHOLD_SINGLE_SAPTO_2030 = 36056;
constexpr double ML_UPR_THRESHOLD_SINGLE_SAPTO_2030 = 45071;
constexpr double ML_LWR_THRESHOLD_FAMILY_SAPTO_2030 = 50191;
constexpr double ML_UPR_THRESHOLD_FAMILY_SAPTO_2030 = 62740;




constexpr double ML_UPR_THRESHOLD_FAMILY_1990_____[64] = {
  ML_UPR_THRESHOLD_FAMILY_1990,
  ML_UPR_THRESHOLD_FAMILY_1991,
  ML_UPR_THRESHOLD_FAMILY_1992,
  ML_UPR_THRESHOLD_FAMILY_1993,
  ML_UPR_THRESHOLD_FAMILY_1994,
  ML_UPR_THRESHOLD_FAMILY_1995,
  ML_UPR_THRESHOLD_FAMILY_1996,
  ML_UPR_THRESHOLD_FAMILY_1997,
  ML_UPR_THRESHOLD_FAMILY_1998,
  ML_UPR_THRESHOLD_FAMILY_1999,
  ML_UPR_THRESHOLD_FAMILY_2000,
  ML_UPR_THRESHOLD_FAMILY_2001,
  ML_UPR_THRESHOLD_FAMILY_2002,
  ML_UPR_THRESHOLD_FAMILY_2003,
  ML_UPR_THRESHOLD_FAMILY_2004,
  ML_UPR_THRESHOLD_FAMILY_2005,
  ML_UPR_THRESHOLD_FAMILY_2006,
  ML_UPR_THRESHOLD_FAMILY_2007,
  ML_UPR_THRESHOLD_FAMILY_2008,
  ML_UPR_THRESHOLD_FAMILY_2009,
  ML_UPR_THRESHOLD_FAMILY_2010,
  ML_UPR_THRESHOLD_FAMILY_2011,
  ML_UPR_THRESHOLD_FAMILY_2012,
  ML_UPR_THRESHOLD_FAMILY_2013,
  ML_UPR_THRESHOLD_FAMILY_2014,
  ML_UPR_THRESHOLD_FAMILY_2015,
  ML_UPR_THRESHOLD_FAMILY_2016,
  ML_UPR_THRESHOLD_FAMILY_2017,
  ML_UPR_THRESHOLD_FAMILY_2018,
  ML_UPR_THRESHOLD_FAMILY_2019,
  ML_UPR_THRESHOLD_FAMILY_2020};






double do_1_medicare_levy_1990(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1991(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1992(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1993(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1994(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1995(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1996(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1997(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1998(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_1999(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_2000(double xd, double yd, bool is_family, int n_dependants );
double do_1_medicare_levy_2001(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2002(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2003(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2004(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2005(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2006(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2007(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2008(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2009(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2010(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2011(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2012(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2013(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2014(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2015(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2016(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2017(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2018(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2019(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2020(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2021(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2022(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2023(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2024(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2025(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2026(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2027(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2028(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2029(double xd, double yd, bool is_family, bool pensioner, int n_dependants );
double do_1_medicare_levy_2030(double xd, double yd, bool is_family, bool pensioner, int n_dependants );

#endif
