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
        HasPartner = R3 %ein% "Married",
        HasDependant = R4 %enotin% "No Dependents",
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
         HasPartner = R3 == "Married",
         HomeOwner = R4 == "Home owner",
         asset_cutout = value)] %>%
  setkey(fy_year, HasPartner, HomeOwner) %>%
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
  .[, HasPartner := grepl("Couples", R5)] %>%
  .[, HasDependant := R5 == "Singles with Dependents"] %>%
  {
    dot <- .
    Couples_with_dependants <- dot[(HasPartner)]
    Couples_with_dependants[, R5 := "Couples with Dependants"]
    rbind(dot, Couples_with_dependants[, HasDependant := TRUE])
  } %>%
  {
    dot <- .
    Singles_without_dependants <- dot[R5 %ein% "Couples and Singles Without Deps"]
    Singles_without_dependants[, R5 := "Singles without Dependants"]
    Singles_without_dependants[, HasPartner := FALSE]
    rbind(dot, Singles_without_dependants[, HasDependant := FALSE])
  } %>%
  .[order(fy_year, value, HasPartner, HasDependant, TaperNumber),
    .(fy_year, HasPartner, HasDependant, TaperNumber, taper = value)]

unemployment_income_tests <-
  unemployment_income_thresholds[unemployment_income_tapers,
                                 on = c("fy_year", "TaperNumber")] %>%
  dcast.data.table(fy_year + HasPartner + HasDependant ~ TaperNumber,
                   value.var = c("IncomeThreshold", "taper"),
                   sep = "_")

rent_assistance_table <-
  capita[sheet_name == "RentA_A"] %>%
  .[capita_headers, on = c("sheet_name", "col"), nomatch = 0] %>%
  drop_empty_cols %>%
  drop_constant_cols %>%
  .[R6 == "No deps", nDependants := 0L] %>%
  .[R6 == "1-2 deps", nDependants := 1L] %>%
  .[R6 == "> 2 deps", nDependants := 3L] %>%
  .[, HasPartner := R5 %ein% "Married"] %>%
  .[,
    .(fy_year = date2fy(end_date),
      R2,
      HasPartner,
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

















