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
#define FORLOOP(content) do {                                  \
_Pragma("omp parallel for num_threads(nThread)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                    \
    content;                                                          \
  }                                                            \
} while (0);                                                   \

#else
#define FORLOOP(content) do {                                       \
for (R_xlen_t i = 0; i < N; ++i) {                             \
  content;                                                      \
}                                                              \
} while (0);
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

// 0-127 with months
typedef struct {
  unsigned int years : 7;
  unsigned int month : 4;
} Age;

typedef struct {
  double taper;
  double rate;
  int lwr_single;
  int upr_single;
  int lwr_family;
  int upr_family;
  int sapto_age;
  int lwr_single_sapto;
  int upr_single_sapto;
  int lwr_family_sapto;
  int upr_family_sapto;
  int lwr_thr_up_per_child;
  bool has_sapto_thr : 1;
} Medicare;

#define MEDICARE_LEN 13

typedef struct {
  int xi;
  int yi;
  int ri; // rebate income
  unsigned int agei : 7;
  unsigned int n_child : 4;
  int on_sapto_cd : 3;
  int is_married : 1;
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

#define SAPTO_LEN 16

SEXP Sapto2Sexp(Sapto S);

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
  bool has_sapto : 1;
  Sapto S;
  int n_offsetn;
  OffsetN Offsets[MAX_N_OFFSETN];
  bool has_temp_budget_repair_levy;
} System;

#define SYSTEM_LEN 10

extern System System1984;
extern System System1985;
extern System System1986;
extern System System1987;
extern System System1988;
extern System System1989;
extern System System1990;
extern System System1991;
extern System System1992;
extern System System1993;
extern System System1994;
extern System System1995;
extern System System1996;
extern System System1997;
extern System System1998;
extern System System1999;
extern System System2000;
extern System System2001;
extern System System2002;
extern System System2003;
extern System System2004;
extern System System2005;
extern System System2006;
extern System System2007;
extern System System2008;
extern System System2009;
extern System System2010;
extern System System2011;
extern System System2012;
extern System System2013;
extern System System2014;
extern System System2015;
extern System System2016;
extern System System2017;
extern System System2018;
extern System System2019;
extern System System2020;
extern System System2021;
extern System System2022;

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
SEXP Medicare2Sexp(Medicare M) ;

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
double value_OffsetN(int x, const OffsetN O);
SEXP nOffsets2List(OffsetN const O[MAX_N_OFFSETN], int noffsets);
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
void validate_sapto(Sapto * S, int fix);

// starts_with_medicare
bool starts_with_medicare(const char * str);

// starts_with_sapto
bool starts_with_sapto(const char * str);

// tax-system.c
System yr2System(int yr);
System Sexp2System(SEXP RSystem, int yr);

#endif
