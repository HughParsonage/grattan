#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
  Check these declarations against the C/Fortran source code.
*/
  
/* .Call calls */
extern SEXP _grattan_pmax3(SEXP, SEXP, SEXP);
extern SEXP _grattan_IncomeTax(SEXP, SEXP, SEXP);
extern SEXP _grattan_pmaxC(SEXP, SEXP);
extern SEXP _grattan_pmaxV(SEXP, SEXP);
extern SEXP _grattan_pminC(SEXP, SEXP);
extern SEXP _grattan_pminV(SEXP, SEXP);
extern SEXP _grattan_sapto_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_sapto_rcpp_yr(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_sapto_rcpp_singleton(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201314Sapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201314NoSapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201213Sapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201213NoSapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201415Sapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201415NoSapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201617Sapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201617NoSapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201718Sapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_MedicareLevy201718NoSapto(SEXP, SEXP, SEXP, SEXP);
extern SEXP _grattan_Offset(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_grattan_pmax3", (DL_FUNC) &_grattan_pmax3, 3},
  {"_grattan_IncomeTax", (DL_FUNC) &_grattan_IncomeTax, 3},
  {"_grattan_pmaxC", (DL_FUNC) &_grattan_pmaxC, 2},
  {"_grattan_pmaxV", (DL_FUNC) &_grattan_pmaxV, 2},
  {"_grattan_pminC", (DL_FUNC) &_grattan_pminC, 2},
  {"_grattan_pminV", (DL_FUNC) &_grattan_pminV, 2},
  {"_grattan_sapto_rcpp", (DL_FUNC) &_grattan_sapto_rcpp, 7},
  {"_grattan_sapto_rcpp_singleton", (DL_FUNC) &_grattan_sapto_rcpp_singleton, 7},
  {"_grattan_MedicareLevy", (DL_FUNC) &_grattan_MedicareLevy, 11},
  {"_grattan_MedicareLevy201213Sapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201213NoSapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201314Sapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201314NoSapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201415Sapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201415NoSapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201516Sapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201516NoSapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201617Sapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201617NoSapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201718Sapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_MedicareLevy201718NoSapto", (DL_FUNC) &_grattan_MedicareLevy201314NoSapto, 4},
  {"_grattan_sapto_rcpp_yr", (DL_FUNC) &_grattan_sapto_rcpp_yr, 4},
  {"_grattan_Offset", (DL_FUNC) &_grattan_Offset, 4},
  {NULL, NULL, 0}
};

void R_init_grattan(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}