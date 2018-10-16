context("Data up-to-date")

test_that("Data is up-to-date as documented", {
  skip_if_not(is.na(Sys.getenv("R_GRATTAN_CHECK_UP_TO_DATE", unset = NA_character_)))
  skip_on_cran()
  # skip_on_travis()
  skip_on_appveyor()
  skip_if_not(identical(.Platform$r_arch, "x64"))
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
  skip_if(length(Rds) == 0)
  res <- vapply(Rds, check_doc, FALSE)
  expect_true(all(res))
  
  
  
})

test_that("pkgdown up-to-date", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("magrittr")
  skip_if_not(file.exists("test_0sysdata.R"))
  skip_if_not(dir.exists("../../docs"))  # if checking, this will be removed
  pkgdown_files <- dir("../../docs", recursive = TRUE, full.names = TRUE)
  
  expect_gt(length(pkgdown_files), 0)
  
  last_doc_file <-
    pkgdown_files %>%
    vapply(file.mtime, double(1)) %>%
    .[which.max(.)] %>%
    as.POSIXct.numeric(origin = structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
  
  expect_lte(as.integer(difftime(time1 = Sys.time(), last_doc_file, units = "hours")), 24)
})



