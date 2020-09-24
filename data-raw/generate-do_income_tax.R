
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
  cg("double yd = (double)spc_rebate_income[i];")
  cl("bool is_married = yd > 0 || partner_status[i];")
  cl("bool is_family = is_married || n_dependants > 0;")
  if (is_na_YEAR || yr >= 2000) {
    # SAPTO (and predecessors)
    cl("int agei = c_age_30_june[i];")
    cl("bool pensioner_eligible = agei >= 65;")
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
    
    cl("if (pensioner_eligible) {")
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
  if (yr >= 2000) {
    cg("double ml = do_1_medicare_levy_{YEAR}(xd, yd, is_family, pensioner_eligible);")
  } else {
    cg("double ml = do_1_medicare_levy_{YEAR}(xd, yd, is_family);")
  }
  
  
  cg("// Budget levies")
  if (yr == 2011) {
    cl("// flood levy")
    cl("taxi += 0.005 * (max0(xd - 50e3) * max0(xd - 100e3));")
  }
  if (yr >= 2015 && yr <= 2017) {
    cl("// temporary budget repair levy")
    cl("taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xd - TEMP_BUDGET_REPAIR_LEVY_THRESH);")
  }
  
  cl("out[i] = taxi + mk;")
  cl("}") # lint
  cl("}") # for (R_xlen_t ...
  cl("}") # switch
  cl("break;")
}

cg <- function(...) base::cat(g(...), "\n", file = "tmp_med_levy.cpp", sep = "", append = TRUE)
cl <- function(...) base::cat(..., "\n", file = "tmp_med_levy.cpp", sep = "", append = TRUE)
for (.yr in c(1990:2030)) {
  if (.yr %% 4 == 0)
    print("")
  YEAR <- .yr
  if (.yr < 2001) {
    cg("double do_1_medicare_levy_{YEAR}(double xd, double yd, bool is_family, int n_dependants) {{")
  } else {
    cg("double do_1_medicare_levy_{YEAR}(double xd, double yd, bool is_family, bool pensioner, int n_dependants) {{")
  }
  #   double do_1_medicare_2018(double xd, int n_dependants, bool is_family, bool pensioner) {
  # cg("double out = 0;")
  #   ML_LWR_THRESHOLD_SINGLE_1984
  if (.yr < 2001) {
    cg("double threshold =")
    cg("is_family ? ML_LWR_THRESHOLD_FAMILY_{YEAR} : ML_LWR_THRESHOLD_SINGLE_{YEAR};")
    
    cg("double upper_threshold = ")
    cg("is_family ? ML_UPR_THRESHOLD_FAMILY_{YEAR} : ML_UPR_THRESHOLD_SINGLE_{YEAR};")
  } else {
    cg("double threshold =")
    cg("is_family ?")
    cg("(pensioner ? ML_LWR_THRESHOLD_FAMILY_SAPTO_{YEAR} : ML_LWR_THRESHOLD_FAMILY_{YEAR}) :")
    cg("(pensioner ? ML_LWR_THRESHOLD_SINGLE_SAPTO_{YEAR} : ML_LWR_THRESHOLD_SINGLE_{YEAR});")
    
    cg("double upper_threshold = ")
    cg("is_family ? ")
    cg("(pensioner ? ML_UPR_THRESHOLD_FAMILY_SAPTO_{YEAR} : ML_UPR_THRESHOLD_FAMILY_{YEAR}) :")
    cg("(pensioner ? ML_UPR_THRESHOLD_SINGLE_SAPTO_{YEAR} : ML_UPR_THRESHOLD_SINGLE_{YEAR});")
  }
  
  cl("if (xd < threshold) {")
  cl("return 0;")
  cl("}")
  cl("double upr_over_lwr = (double)upper_threshold / (double)threshold;")
  
  cl("if (n_dependants) {")
  cg("threshold += ML_LWR_UP_PER_CHILD_{YEAR} * n_dependants;")
  cl("upper_threshold *= upr_over_lwr;")
  cl("upper_threshold = floor(upper_threshold);")
  cl("}")
  cl("if (xd > upper_threshold) {")
  cg("return ML_RATE_1984_____[{YEAR} - 1984] * xd;")
  cl("}")
  cg("return ML_TAPER_1984_____[{YEAR} - 1984] * (xd - threshold);")
  cg("}}")
  cl("\n")
}

cg <- function(...) base::cat(g(...), "\n", file = "src/grattanMedicareLevy.h", sep = "", append = TRUE)
cl <- function(...) base::cat(..., "\n", file = "src/grattanMedicareLevy.h", sep = "", append = TRUE)
for (.yr in c(1990:2030)) {
  YEAR <- .yr
  if (.yr < 2001) {
    cg("double do_1_medicare_levy_{YEAR}(double xd, double yd, bool is_family, int n_dependants = 0);")
  } else {
    cg("double do_1_medicare_levy_{YEAR}(double xd, double yd, bool is_family, bool pensioner, int n_dependants = 0);")
  }
}



cl("ML_UPR_THRESHOLD_FAMILY_1984_____[64] = {")
for (y in 1990:2030) {
  cg("ML_UPR_THRESHOLD_FAMILY_{y},")
}
cl("}")

