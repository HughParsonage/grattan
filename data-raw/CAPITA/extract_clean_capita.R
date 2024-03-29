library(data.table)
library(magrittr)
library(tidyxl)
library(unpivotr)
library(hutils)
library(grattan)

clean_CAPITA_sheet <- function(input, debug = NULL) {
  sheetDT <- as.data.table(input)
  
  # tidy_xlsx() does not read in the columns but 
  # retains the original of the value for 'col'. So
  # if column 10 was empty in the excel file, the 
  # col values will be 8 9 11 12
  
  empty_columns <- setdiff(seq_len(max(sheetDT$col)),
                           unique(sheetDT$col))
  
  # Goddammit Mark
  if (identical(sheetDT[address == "E2"][["character"]], "Bott, Mark:")) {
    empty_columns <- sheetDT[address == "E2"][["col"]][1]
  }
  
  # exclude empty columns (e.g. CPI_A has a comment to the right of the table 
  # which messes up the logic of numericness)
  
  if (length(empty_columns) > 0) {
    sheetDT <- sheetDT[col < min(empty_columns)]
  }
  
  type_by_row <-
    sheetDT %>%
    .[data_type != "blank"] %>%
    .[, .N, by = c("data_type", "row")]
  
  numeric_row <- 
    type_by_row %>%
    .[, .(numeric = all(data_type %in% c("numeric", "date"))), keyby = row]
  
  headers <- 
    as.data.table(sheetDT) %>%
    .[row < min(numeric_row[(numeric)][["row"]]), .(row, col, character)] %>%
    .[order(col)]
  
  tryCatch({
    raw_headers <- 
      headers[row == max(row), list(col, raw_name = character)]
  }, 
  warning = function(e) {
    cat(debug)
    e
  })
  
  numeric_cols <- 
    sheetDT[row >= which.max(numeric_row[["numeric"]]),
            list(row, col, numeric)] %>%
    melt.data.table(id.vars = c("col", "row")) %>%
    .[complete.cases(.)] %>%
    .[raw_headers, on = "col"]
  
  # Are columns 1:2 just dates (i.e. the keys of the table)?
  which_dates_cols <- unique(sheetDT[!is.na(date)][["col"]])
  
  assign("the_sheet_DT", sheetDT, envir = .GlobalEnv)
  if (identical(which_dates_cols, c(1L, 2L))) {
    start_date_by_row <-
      sheetDT %>%
      .[col == 1] %>%
      .[row >= which.max(numeric_row[["numeric"]]),
              list(row, start_date = date)] %>%
      .[complete.cases(.)]
    
    end_date_by_row <-
      sheetDT %>%
      .[col == 2] %>%
      .[row >= which.max(numeric_row[["numeric"]]),
        list(row, end_date = date)] %>%
      .[complete.cases(.)]
    
    out <- 
      numeric_cols %>%
      .[col > 2] %>%
      .[start_date_by_row, on = "row"] %>%
      .[end_date_by_row, on = "row"]
    
    
    
  } else {
    # i.e. one date to key the data
    if (identical(which_dates_cols, 1L)) {
      date_by_row <- 
        sheetDT %>%
        .[col == 1] %>%
        .[row >= which.max(numeric_row[["numeric"]]),
          list(row, start_date = date)]  %>%
        .[, end_date := shift(start_date, type = "lead")] %>%
        .[complete.cases(.)]
      
      out <- 
        numeric_cols %>%
        .[col > 1] %>%
        .[date_by_row, on = "row"]
    } else {
      
      if (!is.null(debug)) {
        cat(debug, "")
        out <- data.table(col = NA_integer_)
      }
    }
  }
  
  out
}

if (!file.exists("data-raw/CAPITA/capita.tsv") || 
    !file.exists("data-raw/CAPITA/capita-headers.tsv")) {
  tidy_xlsx <- function (path, sheets = NA) {
    # .Deprecated(msg = paste("'tidy_xlsx()' is deprecated.", "Use 'xlsx_cells()' or 'xlsx_formats()' instead.", 
    #                         sep = "\n"))
    path <- tidyxl:::check_file(path)
    all_sheets <- tidyxl:::utils_xlsx_sheet_files(path)
    sheets <- tidyxl:::check_sheets(sheets, path)
    formats <- tidyxl:::xlsx_formats_(path)
    cells <- tidyxl:::xlsx_cells_(path, sheets$sheet_path, sheets$name, 
                                  sheets$comments_path)
    cells$sheet <- factor(cells$sheet, levels = sheets$name)
    cells_list <- split(cells, cells$sheet)
    cells_list <- lapply(cells_list, function(x) x[, -1])
    list(data = cells_list, formats = formats)
  }
  
  CAPITA <- 
    switch(basename(getwd()),
           "grattan" = tidy_xlsx("./data-raw/CAPITA/CPS-v17-09-12.xlsx"),
           "data-raw" = tidy_xlsx("./CAPITA/CPS-v17-09-12.xlsx"),
           "CAPITA" = tidy_xlsx("CPS-v17-09-12.xlsx"))
  
  CAPITA_data <- CAPITA[["data"]]
  
  capita_tables <- list(length(CAPITA_data))
  K <- seq_along(CAPITA_data)[names(CAPITA_data) != "Contents"]
  
  
  capita <- 
    lapply(K, function(k) {
      sheet_nom <- names(CAPITA_data)[k]
      if (sheet_nom != "Contents") {
        capita_tables[[k]] <- 
          clean_CAPITA_sheet(CAPITA_data[[k]], debug = k) %>%
          .[, "sheet_name" := sheet_nom] %>%
          .[, "k" := k]
      }
    }) %>%
    rbindlist
  
  get_headers <- function(input) {
    sheetDT <- as.data.table(input)
    
    sheetDT[row <= 7, .(row, col, character)] %>%
      .[, is_raw_header := if_else(row == 7L, "raw", "above")] %>%
      setorder(col, -row) %>%
      .[, character_filled_right := zoo::na.locf(character, na.rm = FALSE), by = row] %>%
      .[, character_filled_left := zoo::na.locf(character, na.rm = FALSE, fromLast = TRUE), by = row] %>%
      .[, character_filled := coalesce(character_filled_right, character_filled_left)] %>%
      .[, row_as_char := paste0("R", row)] %>%
      dcast.data.table(col ~ row_as_char, value.var = "character_filled") %>%
      setcolorder(rev(names(.))) %>%
      setnames("R7", "raw_name")
  }
  
  capita_headers <- 
    lapply(K, function(k) {
      sheet_nom <- names(CAPITA_data)[k]
      if (sheet_nom != "Contents") {
        capita_tables[[k]] <- 
          get_headers(CAPITA_data[[k]]) %>%
          .[, "sheet_name" := sheet_nom] %>%
          .[, "k" := k]
      }
    }) %>%
    rbindlist(use.names = TRUE, fill = TRUE) 
  
  fwrite(capita_headers, "data-raw/CAPITA/capita-headers.tsv", sep = "\t")
  
  # stopifnot(basename(getwd()) == "grattan")
  # fwrite(capita, "data-raw/CAPITA/capita.tsv", sep = "\t")
} else {
  capita <- fread("data-raw/CAPITA/capita.tsv", sep = "\t", na.strings = c("NA", ""))
  capita_headers <- fread("data-raw/CAPITA/capita-headers.tsv", na.strings = c("NA", ""))
}

cols2COL <- function(x) {
  w0 <- which(x <= 26)
  w1 <- which(x >= 27)
  x0 <- x[w0]
  x1 <- x[w1]
  out <- character(length(x))
  out[w0] <- LETTERS[x0]
  out[w1] <- paste0(LETTERS[x1 %/% 26], LETTERS[x1 %% 26])
  out
}

capita[, COL := cols2COL(col)]





assert_drop60plus_ok <- function(DT) {
  #' @return Asserts that the allowances for
  #' Singles with dependants and singles (60 + and long-term recipient)
  #' are identical
  #' 
  #' 
  stopifnot("col" %in% names(DT))
  
  Keeps <- DT[col %between% c(6, 7)]
  Drops <- DT[col %between% c(9, 10)]
  
  if (!identical(Keeps[["value"]],
                 Drops[["value"]])) {
    stop("`With Dependents` not identical to `60+ and long term recipient`")
  }
  
  DT[!between(col, 9, 11)]
}

unemployment_annual_rates <-
  capita[sheet_name == "Unemployment_A"] %>%
  .[!grepl("^not_used", raw_name)] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %T>%
  {stopifnot(all(.$raw_name == .$i.raw_name))} %>%
  .[R1 == "Period" | R2 %pin% c("Period", "NewStart Allowance,")] %>%
  assert_drop60plus_ok %>%
  .[, .(fy_year = yr2fy(year(end_date)),
        hasPartner = R3 %ein% "Married",
        hasDependant = R4 %enotin% "No Dependents",
        Component = R6,
        value = round(value, 2))] %>%
  unique %>%
  dcast.data.table(... ~ Component, value.var = "value")


# Give up
unemployment_table_means_tests_annual <-
  capita[sheet_name == "Unemployment_A"] %>%
  .[!grepl("^not_used", raw_name)] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %T>%
  {stopifnot(all(.$raw_name == .$i.raw_name))} %>%
  .[R1 == "Period" | R1 == "Means Tests"] %>%
  .[, fy_year := yr2fy(year(end_date))] %>%
  drop_constant_cols %>%
  .[]

unemployment_assets_tests <-
  capita[sheet_name == "Unemployment_A"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[R2 == "Asset Cutout"] %>%
  drop_constant_cols %>%
  .[,
    list(fy_year = date2fy(end_date),
         hasPartner = R3 == "Married",
         HomeOwner = R4 == "Home owner",
         asset_cutout = as.integer(floor(value)))] %>%
  setkey(fy_year, hasPartner, HomeOwner) %>%
  .[]

unemployment_income_thresholds <- 
  capita[sheet_name == "Unemployment_A"] %>%
  .[!grepl("^not_used", raw_name)] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[R2 == "Income tests"] %>%
  .[, i.raw_name := NULL] %>%
  .[raw_name %chin% c("UnempThr1F", "UnempThr2F")] %>%
  drop_constant_cols %>%
  .[, .(fy_year = date2fy(end_date),
        TaperNumber = as.integer(gsub("[^0-9]", "", raw_name)),
        IncomeThreshold = as.integer(floor(value)))]

unemployment_income_tapers <- 
  capita[sheet_name == "Unemployment_A"] %>%
  .[!grepl("^not_used", raw_name)] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[R2 == "Income tests"] %>%
  .[, i.raw_name := NULL] %>%
  .[raw_name %chin% c("UnempTpr1", "UnempTpr2","UnempSingDepsTpr1", "UnempSingDepsTpr2")] %>%
  drop_constant_cols %>%
  .[, .(fy_year = date2fy(end_date),
        R5,
        TaperNumber = as.integer(R6 == "Upper") + 1L,
        raw_name,
        value)] %>%
  .[, hasPartner := grepl("Couples", R5)] %>%
  .[, hasDependant := R5 == "Singles with Dependents"] %>%
  {
    dot <- .
    Couples_with_dependants <- dot[(hasPartner)]
    Couples_with_dependants[, R5 := "Couples with Dependants"]
    rbind(dot, Couples_with_dependants[, hasDependant := TRUE])
  } %>%
  {
    dot <- .
    Singles_without_dependants <- dot[R5 %ein% "Couples and Singles Without Deps"]
    Singles_without_dependants[, R5 := "Singles without Dependants"]
    Singles_without_dependants[, hasPartner := FALSE]
    rbind(dot, Singles_without_dependants[, hasDependant := FALSE])
  } %>%
  .[order(fy_year, value, hasPartner, hasDependant, TaperNumber),
    .(fy_year, hasPartner, hasDependant, TaperNumber, taper = value)]

unemployment_income_tests <-
  unemployment_income_thresholds[unemployment_income_tapers,
                                 on = c("fy_year", "TaperNumber")] %>%
  dcast.data.table(fy_year + hasPartner + hasDependant ~ TaperNumber,
                   value.var = c("IncomeThreshold", "taper"),
                   sep = "_")

unemployment_rates_by_date <-
  capita[sheet_name == "Unemployment"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[, (grep("raw_name", names(.), value = TRUE)) := NULL] %>%
  drop_constant_cols %>%
  .[R1 == "Period" | R2 %pin% c("Period", "NewStart Allowance,")] %>%
  assert_drop60plus_ok %>%
  .[col %notin% c(5, 8, 11, 14)] %>%
  .[, .(Date = as.Date(end_date),
        hasPartner = R3 %ein% "Married",
        hasDependant = R4 %enotin% "No Dependents",
        Component = R6,
        value = round(value, 2))] %>%
  unique %>%
  dcast.data.table(Date + hasPartner + hasDependant ~ Component,
                   value.var = "value") %>%
  .[, ES := coalesce(ES, 0.0)] %>%
  setkey(hasPartner, hasDependant, Date)

unemployment_assets_tests_by_date <-
  capita[sheet_name == "Unemployment"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[R1 == "Means Tests"] %>%
    .[R2 == "Asset Cutout"] %>%
    .[,
      .(Date = as.Date(end_date),
        hasPartner = R3 %ein% "Married",
        HomeOwner = R4 %ein% "Home owner",
        asset_cutout = as.integer(floor(value)))
      ] %>%
    setkey(hasPartner, HomeOwner, Date)

unemployment_income_thresholds_by_date <-
  capita[sheet_name == "Unemployment"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[R1 == "Means Tests"] %>%
  .[R3 == "Income Thresholds"] %>%
  .[,
    .(Date = as.Date(end_date),
      TaperNumber = as.integer(R5 %ein% "Upper") + 1L,
      IncomeThreshold = round(value, 2))
    ] 

unemployment_income_tapers_by_date <-
  capita[sheet_name == "Unemployment"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[R1 == "Means Tests"] %>%
  .[R3 == "Taper Rates"] %>%
  .[, .(Date = as.Date(end_date),
        R5,
        TaperNumber = as.integer(R6 == "Upper") + 1L,
        taper = round(value, 2))] %>%
  .[, hasPartner := grepl("Couples", R5)] %>%
  .[, hasDependant := R5 == "Singles with Dependents"] %>%
  {
    dot <- .
    Couples_with_dependants <- dot[(hasPartner)]
    Couples_with_dependants[, R5 := "Couples with Dependants"]
    rbind(dot, Couples_with_dependants[, hasDependant := TRUE])
  } %>%
  {
    dot <- .
    Singles_without_dependants <- dot[R5 %ein% "Couples and Singles Without Deps"]
    Singles_without_dependants[, R5 := "Singles without Dependants"]
    Singles_without_dependants[, hasPartner := FALSE]
    rbind(dot, Singles_without_dependants[, hasDependant := FALSE])
  } %>%
  .[, R5 := NULL]

unemployment_income_tests_by_date <-
  unemployment_income_thresholds_by_date[unemployment_income_tapers_by_date,
                                 on = c("Date", "TaperNumber")] %>%
  dcast.data.table(Date + hasPartner + hasDependant ~ TaperNumber,
                   value.var = c("IncomeThreshold", "taper"),
                   sep = "_") %>%
  setkeyv(c("hasPartner",
            "hasDependant",
            "Date"))





rent_assistance_table <-
  capita[sheet_name == "RentA_A"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  drop_empty_cols %>%
  drop_constant_cols %>%
  .[R6 == "No deps", nDependants := 0L] %>%
  .[R6 == "1-2 deps", nDependants := 1L] %>%
  .[R6 == "> 2 deps", nDependants := 3L] %>%
  .[, hasPartner := R5 %ein% "Married"] %>%
  .[,
    .(fy_year = date2fy(end_date),
      R2,
      hasPartner,
      nDependants,
      value)]

# assert constant
rent_assistance_table[R2 == "Proportion of rent paid by RA"] %>%
  .[, .(fy_year, value)] %$%
  stopifnot(all(value == 0.75))

rent_assistance_rates <-
  rent_assistance_table[R2 != "Proportion of rent paid by RA"] %>%
  .[, value := round(value, 2)] %>%
  dcast.data.table(... ~ R2, value.var = "value") %>%
  setnames(c("Maximum rent assistance payable",
             "Minimum rent paid for rent allowance to be payable"),
           c("max_rate",
             "min_rent")) %>%
  .[]



rent_assistance_table_by_date <-
  capita[sheet_name == "RentA"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  drop_empty_cols %>%
  drop_constant_cols %>%
  .[R6 == "No deps", nDependants := 0L] %>%
  .[R6 == "1-2 deps", nDependants := 1L] %>%
  .[R6 == "> 2 deps", nDependants := 3L] %>%
  .[, hasPartner := R5 %ein% "Married"] %>%
  .[,
    .(Date = as.Date(end_date),
      R2,
      hasPartner,
      nDependants,
      value)]

# assert constant
rent_assistance_table_by_date[R2 == "Proportion of rent paid by RA"] %>%
  .[, .(Date, value)] %$%
  stopifnot(all(value == 0.75))

rent_assistance_rates_by_date <-
  rent_assistance_table_by_date[R2 != "Proportion of rent paid by RA"] %>%
  .[, value := round(value, 2)] %>%
  dcast.data.table(... ~ R2, value.var = "value") %>%
  setnames(c("Maximum rent assistance payable",
             "Minimum rent paid for rent allowance to be payable"),
           c("max_rate",
             "min_rent")) %>%
  setkeyv(c("hasPartner",
            "nDependants",
            "Date")) %>%
  .[]

youthStudent_annual_rates <-
  capita[sheet_name == "YouthStudents_A"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0L] %>%
  .[COL <= "X"] %>%
  .[and(col %% 3 != 2, COL %notin% c("U", "V", "W", "X"))] %>%
  .[, .(fy_year = date2fy(end_date), col, COL, R6, value)] %>%
  .[, hasDependant := COL > "N"] %>%
  .[, hasPartner := COL %between% c("L", "Q")] %>%
  .[, LivesAtHome := COL %between% c("C", "G")] %>%
  .[, Age16or17 := COL %chin% c("C", "D")] %>%
  .[R6 %ein% c("MBR", "ES")] %>%
  .[, c("COL", "col") := NULL] %>%
  .[] %>%
  dcast.data.table(... ~ R6, value.var = "value") %>%
  .[]

youthStudent_income_tests <- 
  capita[sheet_name == "YouthStudents_A"] %>%
  .[COL %chin% c("U", "V", "W", "X"),
    .(start_date, end_date, col, COL, value)] %>%
  .[, variable_type := if_else(COL %chin% c("U", "V"),
                               "IncomeThreshold", 
                               "taper")] %>%
  .[, TaperNo := if_else(COL %ein% c("U", "W"), "1", "2")] %>%
  .[, fy_year := date2fy(end_date)] %>%
  .[, isStudent := TRUE] %>%
  dcast.data.table(isStudent + fy_year ~ variable_type + TaperNo, value.var = "value") %>%
  .[, IncomeThreshold_1 := as.integer(IncomeThreshold_1)] %>%
  .[, IncomeThreshold_2 := as.integer(IncomeThreshold_2)] %>%
  .[]

youth_annual_rates <- 
  capita[sheet_name == "YouthUnemployment_A"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[COL <= "X"] %>%
  .[and(col %% 3 != 2, COL %notin% c("U", "V", "W", "X"))] %>%
  .[, .(fy_year = date2fy(end_date), col, COL, R6, value)] %>%
  .[, hasDependant := COL > "N"] %>%
  .[, hasPartner := COL %between% c("L", "Q")] %>%
  .[, LivesAtHome := COL %between% c("C", "G")] %>%
  .[, Age16or17 := COL %chin% c("C", "D")] %>%
  .[R6 %ein% c("MBR", "ES")] %>%
  .[, c("COL", "col") := NULL] %>%
  dcast.data.table(... ~ R6, value.var = "value")

youth_income_tests <- 
  capita[sheet_name == "YouthUnemployment_A"] %>%
  .[COL %chin% c("U", "V", "W", "X"),
    .(fy_year = date2fy(end_date), COL, value)] %>%
  .[, variable_type := if_else(COL %chin% c("U", "V"),
                               "IncomeThreshold", 
                               "taper")] %>%
  .[, TaperNo := if_else(COL %ein% c("U", "W"), "1", "2")] %>%
  .[, isStudent := FALSE] %>% 
  dcast.data.table(isStudent + fy_year ~ variable_type + TaperNo, value.var = "value") %>%
  .[, IncomeThreshold_1 := as.integer(IncomeThreshold_1)] %>%
  .[, IncomeThreshold_2 := as.integer(IncomeThreshold_2)] %>%
  rbind(youthStudent_income_tests, use.names = TRUE, fill = TRUE) %>%
  setkey(isStudent, fy_year) %>%
  .[]

youth_unemployment_rates <-
  capita[sheet_name == "YouthUnemployment_A"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  .[col %between% match(c("C", "T"), LETTERS)] %>%
  drop_constant_cols() %>%
  .[, hasDependant := R3 %ein% c("Coupled With Children", "Sole parents")] %>%
  .[, hasPartner := R3 %ein% c("Coupled Without Children", "Coupled With Children")] %>%
  .[, LivesAtHome := R3 %ein% "Dependent At Home"] %>%
  .[]




# Age pension
# age_pension_income_tests <-
  capita[sheet_name %ein% "Pensions"] %>%
  .[COL %chin% c("Q", "S", "U"),
    .(start_date, end_date, col, COL, value)] %>%
  .[, variable_type := if_else(COL %chin% c("U"),
                               "taper", 
                               "IncomeThreshold")] %>%
  .[, has_partner := if_else(COL %in% c("S", "T"), "Couple", "Single")] %>%
  .[, .(fy_year = date2fy(end_date), end_date = as.Date(end_date), variable_type, has_partner, value)] %>%
    dcast(fy_year + end_date ~ variable_type + has_partner, value.var = "value") %>%
    melt(measure.vars = patterns("IncomeThreshold"),
         variable.factor = FALSE) %>%
    .[, hasPartner := endsWith(variable, "Couple")] %>%
    .[, .(fy_year, Date = end_date, hasPartner, taper = taper_Single, value)]






