#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP Cincome_tax(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cincome2022(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Crebate_income(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"Cincome_tax",    (DL_FUNC) &Cincome_tax,    10},
    {"Cincome2022",    (DL_FUNC) &Cincome2022,     6},
    {"Crebate_income", (DL_FUNC) &Crebate_income,  8},
    {NULL, NULL, 0}
};

void R_init_grattanDev(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
