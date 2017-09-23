library(zoo)

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








