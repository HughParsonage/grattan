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

#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP(content)                                                \
_Pragma("omp parallel for num_threads(nThread)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                    \
    (content);                                                          \
  }
#else
#define FORLOOP(content)                                       \
for (R_xlen_t i = 0; i < N; ++i) {                             \
  (content);                                                      \
}
#endif



#define MAX_NBRACK 8
#define MAX_OFFSETN 31
#define MIN_YEAR 1984
#define MAX_YEAR 2030
#define NA_INT -2147483648
#define TEMP_BUDGET_REPAIR_LEVY_THRESH 180000
#define TEMP_BUDGET_REPAIR_LEVY_RATE 0.02
#ifndef NaN
  #define NaN NAN
#endif

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
extern int ORDINARY_TAX_BRACKETS_2022[MAX_NBRACK];
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
extern double ORDINARY_TAX_RATES_2022[MAX_NBRACK];

extern int SAPTO_S12_THRESH;
extern double SAPTO_S12_TAPER;
extern double SAPTO_TAPER;

typedef struct {
  int lwr_single;
  int upr_single;
  int lwr_family;
  int upr_family;
  bool has_sapto_thr;
  int sapto_age;
  int lwr_single_sapto;
  int upr_single_sapto;
  int lwr_family_sapto;
  int upr_family_sapto;
  int lwr_thr_up_per_child;
  double taper;
  double rate;
} Medicare;

typedef struct {
  int xi;
  int yi;
  int ri; // rebate income
  unsigned int agei : 7;
  int is_married : 1;
  unsigned int n_child : 4;
  int is_family : 1;
} Person;

typedef struct {
  int year;
  double pension_age;
  int mxo_single;
  int mxo_couple;
  int lwr_single;
  int lwr_couple;
  int upr_single;
  int upr_couple;
  double taper;
  
  // Defined in the regulations (relating to spouse transfers)
  double first_tax_rate;
  double second_tax_rate;
  int tax_free_thresh;
  int tax_2nd_thresh;
  double lito_max_offset;
  double lito_1st_thresh;
  double lito_1st_taper;
} Sapto;

typedef struct {
  int offset_1st;
  int thresh_1st;
  double taper_1st;
  bool refundable;
} Offset1;

typedef struct {
  int offset_1st;
  int thresh_1st;
  double taper_1st;
  int thresh_2nd;
  double taper_2nd;
  bool refundable;
} Offset2;

typedef struct {
  int offset_1st;
  int Thresholds[MAX_OFFSETN];
  double Tapers[MAX_OFFSETN];
  R_xlen_t nb;
  bool refundable;
} OffsetN;

typedef struct {
  int yr;
  int nb; // number of tax breaks
  int BRACKETS[MAX_NBRACK];
  double RATES[MAX_NBRACK];
  Medicare M;
  bool has_sapto;
  Sapto S;
  bool has_lito;
  bool has_lmito;
  bool has_offset1;
  Offset1 O1;
  bool has_offset2;
  Offset2 O2;
  bool has_offsetn;
  OffsetN Offsets;
  bool has_temp_budget_repair_levy;
} System;

#include "1984.h"
extern System System1984;
#include "1985.h"
#include "1986.h"
#include "1987.h"
#include "1988.h"
#include "1989.h"
#include "1990.h"
#include "1991.h"
#include "1992.h"
#include "1993.h"
#include "1994.h"
#include "1995.h"
#include "1996.h"
#include "1997.h"
#include "1998.h"
#include "1999.h"
#include "2000.h"
#include "2001.h"
#include "2002.h"
#include "2003.h"
#include "2004.h"
#include "2005.h"
#include "2006.h"
#include "2007.h"
#include "2008.h"
#include "2009.h"
#include "2010.h"
#include "2011.h"

extern System System2011;
#include "2012.h"
#include "2013.h"
#include "2014.h"
extern System System2014;
#include "2015.h"
extern System System2015;
#include "2016.h"
extern System System2016;
#include "2017.h"
extern System System2017;
#include "2018.h"
extern System System2018;
#include "2019.h"
extern System System2019;
#include "2020.h"
extern System System2020;
#include "2021.h"
extern System System2021;
#include "2022.h"
extern System System2022;
#include "2023.h"
#include "2024.h"
#include "2025.h"
#include "2026.h"
#include "2027.h"
#include "2028.h"
#include "2029.h"
#include "2030.h"


// isEquiInt
void isEquiInt(SEXP x, SEXP y, const char * str);

// lito.c
void apply_lito(double * tax, int x, int yr);

// medicare.c
Medicare yr2Medicare(int yr);
void print_Medicare(Medicare M);
void validate_medicare(Medicare * M, int fix, int yr);

// minmax.c
double dmax(double x, double y);
double dmin(double x, double y);
int imax(int x, int y);
int imin(int x, int y);
double dmax0(double x);
double dmin0(double x);

// Offset.c
void apply_offset1(double * tax, int x, Offset1 O);
void apply_offset2(double * tax, int x, Offset2 O);

// omp_diagnose.c
int as_nThread(SEXP x);

// sapto.c
Sapto yr2Sapto(int yr);
void apply_sapto(double * taxi, Person P, Sapto S);

// tax-system.c
System yr2System(int yr);
System Sexp2System(SEXP RSystem, int yr);

#endif
