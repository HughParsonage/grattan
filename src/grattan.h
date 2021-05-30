#ifndef grattan_H
#define grattan_H

#include <Rcpp.h>
using namespace Rcpp;

#include "yrs/1984.h"
#include "yrs/1985.h"
#include "yrs/1986.h"
#include "yrs/1987.h"
#include "yrs/1988.h"
#include "yrs/1989.h"
#include "yrs/1990.h"
#include "yrs/1991.h"
#include "yrs/1992.h"
#include "yrs/1993.h"
#include "yrs/1994.h"
#include "yrs/1995.h"
#include "yrs/1996.h"
#include "yrs/1997.h"
#include "yrs/1998.h"
#include "yrs/1999.h"
#include "yrs/2000.h"
#include "yrs/2001.h"
#include "yrs/2002.h"
#include "yrs/2003.h"
#include "yrs/2004.h"
#include "yrs/2005.h"
#include "yrs/2006.h"
#include "yrs/2007.h"
#include "yrs/2008.h"
#include "yrs/2009.h"
#include "yrs/2010.h"
#include "yrs/2011.h"
#include "yrs/2012.h"
#include "yrs/2013.h"
#include "yrs/2014.h"
#include "yrs/2015.h"
#include "yrs/2016.h"
#include "yrs/2017.h"
#include "yrs/2018.h"
#include "yrs/2019.h"
#include "yrs/2020.h"
#include "yrs/2021.h"
#include "yrs/2022.h"
#include "yrs/2023.h"
#include "yrs/2024.h"
#include "yrs/2025.h"
#include "yrs/2026.h"
#include "yrs/2027.h"
#include "yrs/2028.h"
#include "yrs/2029.h"
#include "yrs/2030.h"

const int TEMP_BUDGET_REPAIR_LEVY_THRESH = 180000;
const double TEMP_BUDGET_REPAIR_LEVY_RATE = 0.02;
double SBTO_DISCOUNT(int yr);
double sapto_rcpp_singleton(double rebate_income, double max_offset, double lower_threshold, double taper_rate, bool sapto_eligible, double Spouse_income, bool is_married);


#endif
