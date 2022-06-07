#ifndef grattan_H
#define grattan_H

#if _OPENMP
#include <omp.h>
#define AS_NTHREAD int nThread = as_nThread(nthreads);
#else
#define AS_NTHREAD
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
#define MAX_OFFSETN 15
#define MAX_N_OFFSETN 15
#define MIN_YEAR 1984
#define MAX_YEAR 2030
#define NA_INT -2147483648
#define TEMP_BUDGET_REPAIR_LEVY_THRESH 180000
#define TEMP_BUDGET_REPAIR_LEVY_RATE 0.02
#ifndef NaN
  #define NaN NAN
#endif

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
  int nb;
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
  int n_offsetn;
  OffsetN Offsets[MAX_N_OFFSETN];
  bool has_temp_budget_repair_levy;
} System;

#include "1984.h"
extern System System1984;
#include "1985.h"
extern System System1985;
#include "1986.h"
extern System System1986;
#include "1987.h"
extern System System1987;
#include "1988.h"
extern System System1988;
#include "1989.h"
extern System System1989;
#include "1990.h"
extern System System1990;
#include "1991.h"
extern System System1991;
#include "1992.h"
extern System System1992;
#include "1993.h"
extern System System1993;
#include "1994.h"
extern System System1994;
#include "1995.h"
extern System System1995;
#include "1996.h"
extern System System1996;
#include "1997.h"
extern System System1997;
#include "1998.h"
extern System System1998;
#include "1999.h"
extern System System1999;
#include "2000.h"
extern System System2000;
#include "2001.h"
extern System System2001;
#include "2002.h"
extern System System2002;
#include "2003.h"
extern System System2003;
#include "2004.h"
extern System System2004;
#include "2005.h"
extern System System2005;
#include "2006.h"
extern System System2006;
#include "2007.h"
extern System System2007;
#include "2008.h"
extern System System2008;
#include "2009.h"
extern System System2009;
#include "2010.h"
extern System System2010;
#include "2011.h"
extern System System2011;
#include "2012.h"
extern System System2012;
#include "2013.h"
extern System System2013;
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

// errif
void errif_nonnegative(int x, const char * var);

// getListElement
SEXP getListElement(SEXP list, const char * str);

// isEquiInt
void isEquiInt(SEXP x, SEXP y, const char * str);

// hazName
bool hazName(SEXP list, const char * str);

// lito.c
void apply_lito(double * tax, int x, int yr);

// medicare.c
Medicare yr2Medicare(int yr);
void print_Medicare(Medicare M);
void validate_medicare(Medicare * M, int fix, int yr);
double ml_rate(int yr);
double ml_taper(int yr);
int ml_lower_thresh(int yr, bool family, bool sapto);
int ml_upper_thresh(int yr, bool family, bool sapto);

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
void SEXP2Offset(OffsetN * O, int nO, SEXP List);
void do_multiOffsets(double * ansp,
                     R_xlen_t N,
                     const OffsetN mOffsets[MAX_N_OFFSETN],
                     int n_offsets,
                     const int * xp, 
                     int nThread,
                     bool apply);

// omp_diagnose.c
int as_nThread(SEXP x);

// sapto.c
Sapto yr2Sapto(int yr);
void apply_sapto(double * taxi, Person P, Sapto S);

// starts_with_medicare
bool starts_with_medicare(const char * str);

// tax-system.c
System yr2System(int yr);
System Sexp2System(SEXP RSystem, int yr);

#endif
