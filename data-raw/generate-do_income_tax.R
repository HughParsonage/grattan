
g <- glue::glue


cg <- function(...) base::cat(g(...), "\n", file = "tmp_income_tax.cpp", sep = "", append = TRUE)
cl <- function(...) base::cat(..., "\n", file = "tmp_income_tax.cpp", sep = "", append = TRUE)
for (.yr in c(NA, 1990:2030)) {
  is_na_YEAR <- is.na(.yr)
  yr <- coalesce(.yr, 2020L)
  YEAR <- yr
  if (is.na(.yr)) {
    YEAR <- "CURRENT_YEAR"
    next # won't bother with it for now
  }
  
  cl("case ", coalesce(as.character(.yr), "NA_ALIAS"), ": {")
  cl("{") # lint
  cl("for (R_xlen_t i = 0; i < N; ++i) {")
  cg("int xi = ic_taxable_income_loss[i];")
  cg("double xd = (double)xi;")
  if (is_na_YEAR || yr >= 2013) {
    cg("double yd = (double)spc_rebate_income[i];")
    cl("double is_married = yd > 0 || partner_status[i];")
    cl("int agei = c_age_30_june[i];")
  }
  
  cg("double taxi = 0;")
  cg("// ordinary tax")
  cg("taxi += income_taxi_nb(xd, ORD_TAX_BRACK_{YEAR}, ORD_TAX_RATES_{YEAR});")
  
  cg("// Offsets")
  if (is_na_YEAR) {
    cg("// Order of offsets: ITAA 1997 Division 63")
    cg("// 1. SAPTO (individuals then trustees)")
    cg("// 2. BENTO")
    cg("// 3. LITO")
    cg("// 4. LMITO")
  }
  
  if (yr >= 2013) {
    cg("// SAPTO")
    cl("bool sapto_eligible = agei >= 65;")
    cl("if (sapto_eligible) {")
    cl("apply_sapto(yr >= 2023, taxi, xd, yd, is_married);")
    cl("}")
  }
  
  if (yr > 2000) {
    cl("apply_lito(yr, taxi, xd);")
  }
  if (yr >= 2019 && yr <= 2022) {
    cl("apply_lmito(taxi, xd);")
  }
  
  
  cg("\n")
  cg("// Medicare levy")
  
  
  cg("// Budget levies")
  if (yr == 2011) {
    cl("// flood levy")
    cl("taxi += 0.005 * (max0(xd - 50e3) * max0(xd - 100e3));")
  }
  if (yr >= 2015 && yr <= 2017) {
    cl("// temporary budget repair levy")
    cl("taxi += 0.02 * max0(xd - 180e3);")
  }
  
  cl("out[i] = taxi;")
  cl("}") # lint
  cl("}") # for (R_xlen_t ...
  cl("}") # switch
  cl("break;")
}
