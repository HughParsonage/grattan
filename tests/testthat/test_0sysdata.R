context("Data up-to-date")

test_that("Data is up-to-date as documented", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  check_doc <- function(rd) {
    rd_lines <- readLines(rd, warn = FALSE)
    if (length(intrnl_data_line <- grep("internal data", rd_lines))) {
      text_blob <- lapply(intrnl_data_line, function(i) {
        paste0(trimws(rd_lines[pmax(seq(i - 1L, i + 1L, by = 1), 1)]), 
               collapse = " ")
      }) %>%
        paste0(collapse = " ")
      if (!grepl("The internal data was updated on ([0-9]{4}.[0-9]{2}.[0-9]{2})", 
                 text_blob)) {
        stop("In ", rd, "\nString referring to internal data not in expected format.")
      }
      
      Date_rd <- gsub("^.*The internal data was updated on ([0-9]{4}.[0-9]{2}.[0-9]{2}).*$", 
                      "\\1", 
                      text_blob)
      expect_equal(as.character(.date_data_updated), Date_rd, info = rd, label = rd)
      return(TRUE)
    }
    TRUE
  }
  
  skip_if_not(file.exists("test_0sysdata.R"))
  Rds <- dir(path = "../../man", full.names = TRUE, pattern = "\\.Rd$")
  expect_gt(length(Rds), 0)
  res <- vapply(Rds, check_doc, FALSE)
  expect_true(all(res))
  
  
  
})
