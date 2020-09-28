
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
  
  
  
  if (yr >= 2000) {
    cl("apply_sapto(yr, taxi, xd, yd, is_married, agei);")
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
  
  cl("out[i] = taxi + ml;")
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

library(hutils)
for (yr in 1984:2030) {
  provide.file(file.yr <- paste0("src/", yr, ".h"))
  grattan.h <- readLines("grattan.h")
  medicare.h <- readLines("grattanMedicareLevy.h")
  
  
  grep.yr <- grep(paste0("_", yr, ".*;"), grattan.h)
  grep.my <- grep(paste0("_", yr, ".*;"), medicare.h)
  if (!length(grep.yr) && !length(grep.my)) {
    next
  }
  # writeLines(grattan.h[-grep.yr], "src/grattan.h")
  # writeLines(medicare.h[-grep.my], "src/grattanMedicareLevy.h")
  write(unique(c(grattan.h[grep.yr], medicare.h[grep.my])), file =  file.yr, append = TRUE, sep = "")
}




LITO_TBL <- copy(grattan:::lito_tbl)[complete.cases(max_lito)]
LITO_TBL[, yr := fy::fy2yr(fy_year)]
for (i in 1:nrow(LITO_TBL)) {
  out <- LITO_TBL[i, paste0("constexpr double LITO_MAX_OFFSET_", yr, " = ", max_lito, ";\n",
                            "constexpr double LITO_1ST_THRESH_", yr, " = ", lito_taper, ";\n",
                            "constexpr double LITO_TAPER_RATE_", yr, " = ", min_bracket, ";\n")]
  cat(out, file = paste0("src/", LITO_TBL$yr[i], ".h"), append = TRUE)
}

cg <- function(...) base::cat(g(...), "\n", file = "src/grattanMedicareLevy.h", sep = "", append = TRUE)
cl <- function(...) base::cat(..., "\n", file = "src/grattanMedicareLevy.h", sep = "", append = TRUE)
for (yr in 2013:2030) {
  cat(g("constexpr double SAPTO_MAX_SINGLE_{yr} = 2230;"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
  cat(g("constexpr double SAPTO_MAX_MARRIED_{yr} = 1602;"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
  cat(g("constexpr double SAPTO_MAX_ILL_SEP_{yr} = 2040;"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
  cat(g("constexpr double SAPTO_TAPER_{yr} = -0.125;"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
  if (yr <= 2022) {
    cat(g("constexpr double SAPTO_LWR_SINGLE_{yr} = 32279;"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
    cat(g("constexpr double SAPTO_LWR_MARRIED_{yr} = 28974;"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
    cat(g("constexpr double SAPTO_LWR_ILL_SEP_{yr} = 28974;"), "\n",  file = g("src/{yr}.h"), append = TRUE, sep = "")
  } else {
    cat(g("constexpr double SAPTO_LWR_SINGLE_{yr} = 33622;\n"),"\n",  file = g("src/{yr}.h"), append = TRUE, sep = "")
    cat(g("constexpr double SAPTO_LWR_MARRIED_{yr} = 30316;\n"),"\n",  file = g("src/{yr}.h"), append = TRUE, sep = "")
    cat(g("constexpr double SAPTO_LWR_ILL_SEP_{yr} = 32622;\n"), "\n", file = g("src/{yr}.h"), append = TRUE, sep = "")
  }
}

cg <- function(..., file) base::cat(g(...), "\n", sep = "", append = TRUE, file = file)
cl <- function(..., file) base::cat(..., "\n", sep = "", append = TRUE, file = file)

for (yr in 2023:2030) {
  cg("constexpr double LITO_MAX_OFFSET_{yr} = ",   700L, ";", file = g("src/{yr}.h"))
  cg("constexpr double LITO_1ST_THRESH_{yr} = ", 37500L, ";", file = g("src/{yr}.h"))
  cg("constexpr double LITO_2ND_THRESH_{yr} = ", 45000L, ";", file = g("src/{yr}.h"))
}

for (yr in 2019:2022) {
  cg("constexpr double LMITO_1ST_OFFSET_{yr} = 255;", file = g("src/{yr}.h"))
  cg("constexpr double LMITO_THRESHOLDS_{yr}[4] = {{37e3, 48e3, 90e3, 126e3}};", file = g("src/{yr}.h"))
  cg("constexpr double LMITO_TAPER_RATES_{yr}[4] = {{0.075, 0, -0.03, 0}};", file = g("src/{yr}.h"))
}


for (yr in 1984:2030) {
  writeLines(grep("20.._((20..)|____)", unique(readLines(paste0("src/", yr, ".h"))), value = TRUE, invert = TRUE), 
             con = paste0("src/", yr, ".h"))
}


cat("switch (yr) {\n")
for (yr in 2001:2030) {
  y <- yr
  cat("case ", yr, ": {\n", sep = '')
  cat("{\n")
  cat("double y = ", "LITO_MAX_OFFSET_", y, ";\n", sep = "")
  cat("double r = LITO_TAPER_RATE_", y, ";\n", sep = "")
  cat("double b1 = LITO_1ST_THRESH_", y, ";\n", sep = "")
  if (yr < 2023) {
    cat(paste0("return (x < b) ? y : max0(y + r * (x - b));\n"), sep = "")
  } else {
    cat("double b2 = LITO_2ND_THRESH_", y, ";\n", sep = "")
    cat("double r1 = LITO_1ST_THRESH_", y, ";\n", sep = "")
    cat("double r2 = LITO_2ND_RATE_", y, ";\n", sep = "")
    cat("if (x > b1) {\n")
    cat("return y + r1 * (x - b1);\n")
    cat("} else {\n")
    cat("return y + r1 * (b2 - b1) + r2 * (x - b2);\n")
    cat("}\n")
  }
  cat("}\n")
  cat("}\n")
  cat("break;\n")
}

g <- glue::glue
for (YEAR in 1990:2030) {
  cat("case ", YEAR, ": \n", sep = '')
  cat(g("sapto = do_1_sapto_(xd, yd, sapto_cd, SAPTO_LWR_{YEAR}, SAPTO_UPR_{YEAR}, SAPTO_MAX_{YEAR}, SAPTO_TAPER_{YEAR}, ", 
      "ORD_TAX_BRACK_{YEAR}[1], ORD_TAX_RATES_{YEAR}[1], ", sep = "")
}

for (YEAR in 1984:2030) {
  Year.h <- readLines(file.h <- file.path("src", "yrs", paste0(YEAR, ".h")))
  if (!any_grepl(Year.h, "ML_TAPER")) {
    w <- which_last(grepl("ML_", Year.h))
    Year.h <- c(Year.h[1:w], 
                paste0("ML_TAPER_", YEAR, " = ", 
                       fcase(YEAR <= 1993, "0.25;",
                             YEAR <= 2004, "0.20;",
                             default = "0.1;")),
                Year.h[-seq_len(w)])
  }
  if (!any_grepl(Year.h, "ML_RATE")) {
    w <- which_last(grepl("ML_", Year.h))
    Year.h <- c(Year.h[1:w], 
                paste0("ML_RATE_", YEAR, " = ", 
                       fcase(YEAR <= 1993, "0.0125;",
                             YEAR <= 1995, "0.014;",
                             YEAR == 1997, "0.017;",
                             YEAR <= 2014, "0.015;",
                             default = "0.02;")),
                Year.h[-seq_len(w)])
  }
  writeLines(Year.h, file.h)
}

for (YEAR in 1984:2030) {
  Year.h <- readLines(file.h <- file.path("src", "yrs", paste0(YEAR, ".h")))
  Year.h <- hutils::if_else(startsWith(Year.h, "ML_"), paste0("constexpr double ", Year.h), Year.h)
  writeLines(Year.h, file.h)
}

for (YEAR in 1984:2012) {
  Year.h <- readLines(file.h <- file.path("src", "yrs", paste0(YEAR, ".h")))
  if (any_grepl(Year.h, "SAPTO_") && !any_grepl(Year.h, "SAPTO_TAPER_[12][0-9]{3}")) {
    w <- which_last(grepl("SAPTO_", Year.h))
    if (!w) {
      stop("YEAR = ", YEAR)
    }
      Year.h <- c(Year.h[seq_len(w)], 
                  paste0("constexpr double SAPTO_TAPER_", YEAR, " = -0.125;"),
                  Year.h[-seq_len(w)])
    
  writeLines(Year.h, file.h)
  }
}



cg <- function(...) base::cat(g(...), "\n", file = "src/tmp_income_tax.cpp", sep = "", append = TRUE)
cl <- function(...) base::cat(..., "\n", file = "src/tmp_income_tax.cpp", sep = "", append = TRUE)
for (YEAR in 1984:2030) {
  if (YEAR == 1984) {
    cl("switch(yr) {")
  }
  cl("case ", YEAR, ": {")
  if (YEAR >= 2000L) {
    cl("// Create a Sapto struct for this year")
    cg("Sapto Sapto{YEAR};")
    cg("Sapto{YEAR}.year = {YEAR};")
    cg("Sapto{YEAR}.pension_age = 65;")
    cg("Sapto{YEAR}.mxo_single = SAPTO_MAX_SINGLE_{YEAR};")
    cg("Sapto{YEAR}.mxo_couple = SAPTO_MAX_MARRIED_{YEAR};")
    cg("Sapto{YEAR}.lwr_single = SAPTO_LWR_SINGLE_{YEAR};")
    cg("Sapto{YEAR}.lwr_couple = SAPTO_LWR_MARRIED_{YEAR};")
    cg("Sapto{YEAR}.upr_single = SAPTO_LWR_SINGLE_{YEAR} + SAPTO_MAX_SINGLE_{YEAR} / SAPTO_TAPER_{YEAR};")
    cg("Sapto{YEAR}.upr_couple = SAPTO_LWR_MARRIED_{YEAR} + SAPTO_MAX_MARRIED_{YEAR} / SAPTO_TAPER_{YEAR};")
    cg("Sapto{YEAR}.taper = SAPTO_TAPER_{YEAR};")
    cg("Sapto{YEAR}.first_tax_rate = ORD_TAX_RATES_{YEAR}[1];")
    cg("Sapto{YEAR}.tax_free_thresh = ORD_TAX_BRACK_{YEAR}[1];")
    cg("Sapto{YEAR}.lito_max_offset = LITO_MAX_OFFSET_{YEAR};")
    cl("")
  }
  
  
  cg("Medicare M{YEAR};")
  cg("M{YEAR}.lwr_single = ML_LWR_THRESHOLD_SINGLE_{YEAR};")
  cg("M{YEAR}.upr_single = ML_UPR_THRESHOLD_SINGLE_{YEAR};")
  cg("M{YEAR}.lwr_family = ML_LWR_THRESHOLD_FAMILY_{YEAR};")
  cg("M{YEAR}.upr_family = ML_UPR_THRESHOLD_FAMILY_{YEAR};")
  if (YEAR >= 2000L) {
    cg("M{YEAR}.lwr_single_sapto = ML_LWR_THRESHOLD_SINGLE_SAPTO_{YEAR};")
    cg("M{YEAR}.upr_single_sapto = ML_UPR_THRESHOLD_SINGLE_SAPTO_{YEAR};")
    cg("M{YEAR}.lwr_family_sapto = ML_LWR_THRESHOLD_FAMILY_SAPTO_{YEAR};")
    cg("M{YEAR}.upr_family_sapto = ML_UPR_THRESHOLD_FAMILY_SAPTO_{YEAR};")
  }
  cg("M{YEAR}.lwr_thr_up_per_child = ML_LWR_THR_UP_PER_CHILD_{YEAR};")
  cg("M{YEAR}.taper = ML_TAPER_{YEAR};")
  cg("M{YEAR}.rate = ML_RATE_{YEAR};")
  cg("M{YEAR}.has_sapto_thr = {as.double(YEAR > 2000)};")
  cg("M{YEAR}.sapto_age = 65;")
  
  cl("{")
  cl("for (R_xlen_t i = 0; i < N; ++i) {")
  cl("Person P;")
  cl("int xi = ic_taxable_income_loss[i];")
  cl("double taxi = 0;")
  cl("")
  cl("P.xi = xi;")
  cl("P.yi = spc_rebate_income[i];")
  cl("P.agei = c_age_30_june[i];")
  cl("P.is_married = partner_status[i];")
  cl("P.n_child = n_dependants[i];")
  cl("P.is_family = P.is_married || P.n_child;")
  cl("")
  cg("taxi += income_taxi_nb(xi, ORD_TAX_BRACK_{YEAR}, ORD_TAX_RATES_{YEAR});")
  # SAPTO
  if (YEAR >= 2000L) {
  cg("apply_sapto(taxi, Person, Sapto{YEAR});")
  }
  
  
  # LITO
  if (YEAR >= 1994) {
    if (YEAR <= 2023) {
      cg("apply_lito(taxi, xi, LITO_MAX_OFFSET_{YEAR}, LITO_1ST_THRESH_{YEAR}, LITO_1ST_TAPER_{YEAR});")
    } else {
      cg("apply_lito(taxi, xi, LITO_MAX_OFFSET_{YEAR}, LITO_1ST_THRESH_{YEAR}, LITO_1ST_TAPER_{YEAR}, LITO_2ND_THRESH_{YEAR}, LITO_2ND_TAPER_{YEAR});")
    }
  }
  
  if (YEAR >= 2019 && YEAR <= 2022) {
    cl("apply_lmito(taxi, xd);")
  }
  cg("")
  cg("// Medicare levy")
  cg("double ml = do_1_ML(P, M{YEAR});")
  cl("taxi += ml;")
  cl("")
  if (YEAR == 2011 || YEAR == 2015 || YEAR == 2016 || YEAR == 2017) {
    cl("// Budget levies")
  }
  if (YEAR == 2011) {
    cl("// flood levy")
    cl("taxi += 0.005 * (max0(xd - 50000) * max0(xd - 100000));")
  }
  if (YEAR >= 2015 && YEAR <= 2017) {
    cl("// temporary budget repair levy")
    cl("taxi += TEMP_BUDGET_REPAIR_LEVY_RATE * max0(xd - TEMP_BUDGET_REPAIR_LEVY_THRESH);")
  }
  cl("")
  cl("// finally")
  cl("out[i] = taxi;")
  cl("}")
  cl("")
  cl("} // inner case ", YEAR)
  cl("} // outer case ", YEAR)
  cl("break;")
  if (YEAR == 2030) {
    cl("}")
  }
  cl("")
}










