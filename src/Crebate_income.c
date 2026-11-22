#include "grattan.h"

typedef struct {
  const double * real;
  const int * integer;
  R_xlen_t length;
} Amount;

// Materialize any ALTREP vectors on the R thread before entering OpenMP.
static Amount amount_view(SEXP x) {
  Amount out = {0};
  out.length = xlength(x);
  if (out.length) {
    if (isReal(x)) out.real = REAL(x);
    else if (isInteger(x)) out.integer = INTEGER(x);
    else if (isLogical(x)) out.integer = LOGICAL(x);
  }
  return out;
}

// Missing addbacks are zero. Worker threads only read plain pointers.
static double amount_at(Amount x, R_xlen_t i) {
  if (!x.length) return NA_REAL;
  R_xlen_t j = x.length == 1 ? 0 : i;
  double out = NA_REAL;
  if (x.real) {
    out = x.real[j];
  } else if (x.integer) {
    int value = x.integer[j];
    if (value != NA_INTEGER) out = value;
  }
  return out;
}

static double amount0(Amount x, R_xlen_t i) {
  double value = amount_at(x, i);
  return R_FINITE(value) ? value : 0;
}

static void check_amount(SEXP x, R_xlen_t n, const char * name) {
  if (xlength(x) && xlength(x) != 1 && xlength(x) != n) {
    error("`%s` must have length 1 or the number of incomes.", name);
  }
  if (xlength(x) && !isReal(x) && !isInteger(x) && !isLogical(x)) {
    error("`%s` must be numeric.", name);
  }
}

static double fringe_adjustment(int year) {
  // ITAA 1936 s6: complement of the applicable FBT rate. These are the
  // 2014/2016/2017 ATO rebate-income worksheet multipliers, not PIT rates.
  if (year <= 2014) return 0.535;
  if (year == 2016 || year == 2017) return 0.51;
  return 0.53;
}

SEXP Crebate_income(SEXP Income, SEXP ReportableEmployerSuper,
                    SEXP DeductiblePersonalSuper,
                    SEXP InvestmentLoss, SEXP NetRent, SEXP FringeBenefits,
                    SEXP ExemptFringe, SEXP OtherFringe, SEXP FHSS,
                    SEXP Year, SEXP nthreads) {
  R_xlen_t N = xlength(Income);
  if (!isInteger(Income) || !isInteger(Year) ||
      (xlength(Year) != 1 && xlength(Year) != N)) {
    error("Internal error(Crebate_income): income and year must be compatible integer vectors.");
  }
  check_amount(ReportableEmployerSuper, N, "reportable employer super");
  check_amount(DeductiblePersonalSuper, N, "deductible personal super");
  check_amount(InvestmentLoss, N, "investment loss");
  check_amount(NetRent, N, "net rent");
  check_amount(FringeBenefits, N, "reportable fringe benefits");
  check_amount(ExemptFringe, N, "exempt employer benefits");
  check_amount(OtherFringe, N, "other employer benefits");
  check_amount(FHSS, N, "assessable FHSS release");
  const Amount employer = amount_view(ReportableEmployerSuper);
  const Amount personal = amount_view(DeductiblePersonalSuper);
  const Amount investment = amount_view(InvestmentLoss);
  const Amount rent = amount_view(NetRent);
  const Amount fringe = amount_view(FringeBenefits);
  const Amount exempt = amount_view(ExemptFringe);
  const Amount other = amount_view(OtherFringe);
  const Amount fhss = amount_view(FHSS);
  int nThread = as_nThread(nthreads);
  const int * xp = INTEGER(Income);
  const int * years = INTEGER(Year);
  bool constant_year = xlength(Year) == 1;
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * out = INTEGER(ans);
  FORLOOP({
    int year = years[constant_year ? 0 : i];
    double rebate = xp[i] == NA_INTEGER ? 0 : imax(xp[i], 0);
    if (year <= 2009) {
      out[i] = (int)rebate;
      continue;
    }
    if (year >= 2019) rebate = fmax(0, rebate - amount0(fhss, i));
    rebate += amount0(employer, i);
    rebate += amount0(personal, i);
    rebate += amount0(investment, i);
    rebate += fmax(0, -amount0(rent, i));
    double total = amount0(fringe, i);
    double e = amount_at(exempt, i);
    double o = amount_at(other, i);
    if (R_FINITE(e) || R_FINITE(o)) {
      if (!R_FINITE(e)) e = fmax(0, total - o);
      if (!R_FINITE(o)) o = fmax(0, total - e);
      if (year <= 2017) {
        rebate += floor(fringe_adjustment(year) * (e + o));
      } else {
        rebate += floor(fringe_adjustment(year) * e) + o;
      }
    } else {
      rebate += floor(fringe_adjustment(year) * total);
    }
    out[i] = rebate >= INT_MAX ? INT_MAX : (rebate <= 0 ? 0 : (int)rebate);
  })
  UNPROTECT(1);
  return ans;
}
