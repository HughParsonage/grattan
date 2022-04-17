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







#endif
