
tax_dir <- function(...) {
  file.path(Sys.getenv("R_TAXSTATS_RDA_DIR"), ...)
}

sf_env <- new.env()

GET <- function(x, value) {
  if (exists(x, envir = sf_env, inherits = FALSE)) {
    return(get(x, envir = sf_env))
  }
  assign(x, value, envir = sf_env)
  value
}

CLEAR_ENV <- function() {
  rm(list = ls(envir = sf_env), envir = sf_env)
}

skip_if_not <- function(...) {
  if (requireNamespace("testthat", quietly = TRUE)) {
    testthat::skip_if_not(...)
  }
}

skip_without_sample_files <- function(file = NULL) {
  if (requireNamespace("testthat", quietly = TRUE)) {
    if (dir.exists(tax_dir())) {
      if (is.character(file) && file.access(tax_dir(file), mode = 4L)) {
        testthat::skip("No file access")
      }
    } else {
      testthat::skip("no sample file")
    }
  }
}

.load_all_sample_files <- function() {
  skip_without_sample_files()
  files.rda <- dir(tax_dir(), pattern = "\\.rda$", full.names = TRUE)
  for (f in files.rda) {
    load(f, envir = sf_env)
  }
}

.sfa <- function() {
  .load_all_sample_files()
  do_setnames <- function(dt, old, new) {
    if (old %in% names(dt)) {
      setnames(dt, old, new)
    }
    dt
  }
  Year <- NULL
  rbindlist(lapply(ls(pattern = "sample_file_[0-9]{4}", envir = sf_env), function(obj) {
    out <- GET(obj)
    do_setnames(out, "Birth_year", "age_range")
    do_setnames(out, "Marital_status", "Partner_status")
    do_setnames(out, "HECS_accum_ind", "Help_debt")
  }), use.names = TRUE, fill = TRUE, idcol = "Year") %>%
    .[, "fy.year" := yr2fy(Year + 2003L)]
}

.sample_file_1415 <- function() {
  GET("sample_file_1415", {
    if (requireNamespace("testthat", quietly = TRUE)) {
      skip_if_not(file.exists("~/SampleFile1415/data-raw/sample_file_1415.tsv"))
    }
    fread(file = "~/SampleFile1415/data-raw/sample_file_1415.tsv")
  })
}

.sample_file_1516 <- function() {
  GET("sample_file_1516", {
    if (requireNamespace("testthat", quietly = TRUE)) {
      skip_if_not(file.exists("~/Data/ato/2015-16/Sample_file_1516/2016_sample_file.csv"))
    }
    fread(file = "~/Data/ato/2015-16/Sample_file_1516/2016_sample_file.csv")
  })
}
.sample_file_1617 <- function() {
  GET("sample_file_1617", {
    if (requireNamespace("testthat", quietly = TRUE)) {
      testthat::skip_if_not(file.exists("~/Data/ato/2016-17/Sample_file_1617/2017_sample_file.csv"))
      fread(file = "~/Data/ato/2016-17/Sample_file_1617/2017_sample_file.csv")
    }
  })
}

.sample_file_1314 <- function() {
  GET("sample_file_1314", {
    skip_without_sample_files("sample_file_1314.rda")
    load(tax_dir("sample_file_1314.rda"), envir = sf_env)
    GET("sample_file_1314")
  })
}

.sample_file_ <- function(x) {
  z <- paste0("sample_file_", x)
  GET(z, {
    skip_without_sample_files(paste0(z, ".rda"))
    load(tax_dir(paste0(z, ".rda")), envir = sf_env)
    GET(z)
  })
}

.funds_table1_201314 <- function() {
  z <- "funds_table1_201314"
  GET(z, {
    if (requireNamespace("testthat", quietly = TRUE)) {
      skip_if_not(file.exists("~/taxstats/data/funds_table1_201314.rda"))
      load("~/taxstats/data/funds_table1_201314.rda", envir = sf_env)
      GET(z)
    }
  })
}

.funds_table2_smsf_201314 <- function() {
  z <- "funds_table2_smsf_201314"
  GET(z, {
    if (requireNamespace("testthat", quietly = TRUE)) {
      skip_if_not(file.exists("~/taxstats/data/funds_table2_smsf_201314.rda"))
      load("~/taxstats/data/funds_table2_smsf_201314.rda", envir = sf_env)
      GET(z)
    }
  })
}

