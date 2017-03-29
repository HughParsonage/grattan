#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
  Check these declarations against the C/Fortran source code.
*/
  
  /* .Call calls */
  extern SEXP grattan_pmax3(SEXP, SEXP, SEXP);
extern SEXP grattan_pmaxC(SEXP, SEXP);
extern SEXP grattan_pmaxV(SEXP, SEXP);
extern SEXP grattan_pminC(SEXP, SEXP);
extern SEXP grattan_pminV(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"grattan_pmax3", (DL_FUNC) &grattan_pmax3, 3},
  {"grattan_pmaxC", (DL_FUNC) &grattan_pmaxC, 2},
  {"grattan_pmaxV", (DL_FUNC) &grattan_pmaxV, 2},
  {"grattan_pminC", (DL_FUNC) &grattan_pminC, 2},
  {"grattan_pminV", (DL_FUNC) &grattan_pminV, 2},
  {NULL, NULL, 0}
};

void R_init_grattan(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}