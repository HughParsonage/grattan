#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_asraw_OnSaptoCd(SEXP, SEXP);
extern SEXP C_lito(SEXP, SEXP, SEXP);
extern SEXP C_MAX_N_OFFSETN(SEXP);
extern SEXP C_ml_lower_thresh(SEXP, SEXP, SEXP);
extern SEXP C_ml_rate(SEXP);
extern SEXP C_ml_taper(SEXP);
extern SEXP C_ml_upper_thresh(SEXP, SEXP, SEXP);
extern SEXP C_multiOffset(SEXP, SEXP, SEXP);
extern SEXP C_sf2osc(SEXP, SEXP);
extern SEXP C_yr2Offsets(SEXP);
extern SEXP Cbracks_by_year(SEXP, SEXP);
extern SEXP Cdecode_age_range(SEXP, SEXP);
extern SEXP Cdo_medicare_levy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cdo_rn(SEXP, SEXP, SEXP);
extern SEXP Cincome_tax(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cincome2022(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cml_child(SEXP);
extern SEXP COffset(SEXP, SEXP, SEXP, SEXP);
extern SEXP Crates_by_yr(SEXP, SEXP);
extern SEXP Crebate_income(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Csapto(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Ctest_nOffset_upper_threshold(SEXP, SEXP);
extern SEXP CvalidateSystem(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_asraw_OnSaptoCd",             (DL_FUNC) &C_asraw_OnSaptoCd,              2},
    {"C_lito",                        (DL_FUNC) &C_lito,                         3},
    {"C_MAX_N_OFFSETN",               (DL_FUNC) &C_MAX_N_OFFSETN,                1},
    {"C_ml_lower_thresh",             (DL_FUNC) &C_ml_lower_thresh,              3},
    {"C_ml_rate",                     (DL_FUNC) &C_ml_rate,                      1},
    {"C_ml_taper",                    (DL_FUNC) &C_ml_taper,                     1},
    {"C_ml_upper_thresh",             (DL_FUNC) &C_ml_upper_thresh,              3},
    {"C_multiOffset",                 (DL_FUNC) &C_multiOffset,                  3},
    {"C_sf2osc",                      (DL_FUNC) &C_sf2osc,                       2},
    {"C_yr2Offsets",                  (DL_FUNC) &C_yr2Offsets,                   1},
    {"Cbracks_by_year",               (DL_FUNC) &Cbracks_by_year,                2},
    {"Cdecode_age_range",             (DL_FUNC) &Cdecode_age_range,              2},
    {"Cdo_medicare_levy",             (DL_FUNC) &Cdo_medicare_levy,              6},
    {"Cdo_rn",                        (DL_FUNC) &Cdo_rn,                         3},
    {"Cincome_tax",                   (DL_FUNC) &Cincome_tax,                   10},
    {"Cincome2022",                   (DL_FUNC) &Cincome2022,                    7},
    {"Cml_child",                     (DL_FUNC) &Cml_child,                      1},
    {"COffset",                       (DL_FUNC) &COffset,                        4},
    {"Crates_by_yr",                  (DL_FUNC) &Crates_by_yr,                   2},
    {"Crebate_income",                (DL_FUNC) &Crebate_income,                 9},
    {"Csapto",                        (DL_FUNC) &Csapto,                         7},
    {"Ctest_nOffset_upper_threshold", (DL_FUNC) &Ctest_nOffset_upper_threshold,  2},
    {"CvalidateSystem",               (DL_FUNC) &CvalidateSystem,                2},
    {NULL, NULL, 0}
};

void R_init_grattan(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
