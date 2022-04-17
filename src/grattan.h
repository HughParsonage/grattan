#ifndef grattan_H
#define grattan_H

#if _OPENMP
#include <omp.h>
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>
#include <ctype.h>

#define MAX_NBRACK 8

extern int ORDINARY_TAX_BRACKETS_1984[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1985[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1986[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1987[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1988[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1989[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1990[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1991[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1992[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1993[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1994[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1995[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1996[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1997[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1998[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_1999[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2000[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2001[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2002[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2003[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2004[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2005[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2006[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2007[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2008[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2009[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2010[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2011[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2012[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2013[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2014[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2015[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2016[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2017[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2018[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2019[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2020[MAX_NBRACK];
extern int ORDINARY_TAX_BRACKETS_2021[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1984[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1985[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1986[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1987[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1988[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1989[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1990[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1991[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1992[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1993[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1994[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1995[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1996[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1997[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1998[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_1999[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2000[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2001[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2002[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2003[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2004[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2005[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2006[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2007[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2008[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2009[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2010[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2011[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2012[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2013[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2014[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2015[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2016[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2017[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2018[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2019[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2020[MAX_NBRACK];
extern double ORDINARY_TAX_RATES_2021[MAX_NBRACK];



struct Medicare {
  double lwr_single;
  double upr_single;
  double lwr_family;
  double upr_family;
  bool has_sapto_thr;
  int sapto_age;
  double lwr_single_sapto;
  double upr_single_sapto;
  double lwr_family_sapto;
  double upr_family_sapto;
  double lwr_thr_up_per_child;
  double taper;
  double rate;
};

struct Person {
  int xi;
  int yi;
  int agei;
  bool is_married;
  int n_child;
  bool is_family;
};

struct Sapto {
  int year;
  double pension_age;
  double mxo_single;
  double mxo_couple;
  double lwr_single;
  double lwr_couple;
  double upr_single;
  double upr_couple;
  double taper;
  
  // Defined in the regulations (relating to spouse transfers)
  double first_tax_rate;
  double second_tax_rate;
  int tax_free_thresh;
  int tax_2nd_thresh;
  double lito_max_offset;
  double lito_1st_thresh;
  double lito_1st_taper;
};

struct System {
  int nb; // number of tax breaks
  int BRACKETS[MAX_NBRACK];
  double RATES[MAX_NBRACK];
  Medicare M;
  Sapto S;
};

// isEquiInt
void isEquiInt(SEXP x, SEXP y);

// medicare.c
Medicare yr2Medicare(int yr);


#endif
