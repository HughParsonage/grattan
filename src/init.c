#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP Cdecode_age_range(SEXP, SEXP);
extern SEXP Cdo_medicare_levy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cdo_rn(SEXP, SEXP, SEXP);
extern SEXP Cincome_tax(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cincome2022(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP COffset(SEXP, SEXP, SEXP, SEXP);
extern SEXP Crebate_income(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"Cdecode_age_range", (DL_FUNC) &Cdecode_age_range, 2},
    {"Cdo_medicare_levy", (DL_FUNC) &Cdo_medicare_levy, 6},
    {"Cdo_rn",            (DL_FUNC) &Cdo_rn,            3},
    {"Cincome_tax",       (DL_FUNC) &Cincome_tax,       9},
    {"Cincome2022",       (DL_FUNC) &Cincome2022,       7},
    {"COffset",           (DL_FUNC) &COffset,           4},
    {"Crebate_income",    (DL_FUNC) &Crebate_income,    9},
    {NULL, NULL, 0}
};

void R_init_grattanDev(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
