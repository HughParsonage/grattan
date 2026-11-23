# Refresh only the five cached law tables. Run from the repository root with:
# Rscript data-raw/update-law-data.R
# The TSV files are authoritative; no sample files, network, or full data rebuild.

read_law_tables <- function(root = ".") {
  raw_file <- function(name) file.path(root, "data-raw", name)
  read_table <- function(name, classes) {
    data.table::fread(raw_file(name), na.strings = "NA", colClasses = classes)
  }

  tax_tbl <- read_table("tax-brackets-and-marginal-rates-by-fy.tsv",
                        c(fy_year = "character", lower_bracket = "integer",
                          marginal_rate = "numeric"))
  tax_table2 <- data.table::copy(tax_tbl)
  tax_table2[, tax_at := cumsum(data.table::shift(marginal_rate, fill = 0) *
                                (lower_bracket - data.table::shift(lower_bracket, fill = 0))),
             by = "fy_year"]
  tax_table2 <- tax_table2[, list(fy_year = fy_year, income = lower_bracket,
                                 lower_bracket = lower_bracket,
                                 marginal_rate = marginal_rate, tax_at = tax_at)]
  data.table::setkeyv(tax_table2, c("fy_year", "income"))

  lito_tbl <- read_table("lito-info.tsv",
                         c(fy_year = "character", max_lito = "numeric",
                           lito_taper = "numeric", min_bracket = "numeric",
                           second_bracket = "numeric", second_taper = "numeric"))
  data.table::setkeyv(lito_tbl, "fy_year")

  # Upper thresholds denote the first whole-dollar income at the full levy
  # rate, not the last reduced income in an inclusive statutory phase-in table.
  medicare_tbl <- read_table("medicare_tbl.tsv",
                             c(fy_year = "character", sato = "logical", pto = "logical",
                               sapto = "logical", lower_threshold = "numeric",
                               upper_threshold = "numeric", taper = "numeric",
                               rate = "numeric", lower_family_threshold = "numeric",
                               upper_family_threshold = "numeric",
                               lower_up_for_each_child = "numeric"))
  data.table::setkeyv(medicare_tbl, c("fy_year", "sapto"))

  sapto_tbl <- read_table("sapto_tbl.tsv",
                          c(fy_year = "character", family_status_index = "numeric",
                            family_status = "character", max_offset = "numeric",
                            lower_threshold = "numeric", upper_threshold = "numeric",
                            taper_rate = "numeric", source = "character",
                            sapto = "logical", sato = "logical", pto = "logical",
                            max_max_offset = "numeric"))
  data.table::setkeyv(sapto_tbl, c("fy_year", "family_status"))

  list(tax_tbl = tax_tbl, tax_table2 = tax_table2, lito_tbl = lito_tbl,
       medicare_tbl = medicare_tbl, sapto_tbl = sapto_tbl)
}

update_law_data <- function(root = ".") {
  cache_file <- file.path(root, "R", "sysdata.rda")
  before <- new.env(parent = baseenv())
  object_names <- load(cache_file, envir = before)
  law_tables <- read_law_tables(root)
  stopifnot(all(names(law_tables) %in% object_names))
  untouched <- setdiff(object_names, names(law_tables))

  # Preserve the existing column types and table keys. LITO additionally needs
  # two columns for the second taper enacted from 2020-21.
  for (name in names(law_tables)) {
    old <- before[[name]]
    new <- law_tables[[name]]
    stopifnot(all(names(old) %in% names(new)),
              identical(class(old), class(new)),
              identical(data.table::key(old), data.table::key(new)))
    for (column in names(old)) {
      stopifnot(identical(typeof(old[[column]]), typeof(new[[column]])))
    }
  }

  after <- new.env(parent = baseenv())
  for (name in object_names) {
    assign(name, before[[name]], envir = after)
  }
  list2env(law_tables, envir = after)
  temporary <- tempfile("sysdata-law-", tmpdir = dirname(cache_file))
  on.exit(unlink(temporary), add = TRUE)
  # Match the repository's bzip2-compressed, version-2 R data format.
  save(list = object_names, file = temporary, envir = after,
       compress = "bzip2", version = 2)

  verified <- new.env(parent = baseenv())
  saved_names <- load(temporary, envir = verified)
  stopifnot(setequal(saved_names, object_names))
  for (name in untouched) {
    stopifnot(identical(before[[name]], verified[[name]]))
  }
  for (name in names(law_tables)) {
    # Serialization deliberately clears data.table's process-local self pointer.
    stopifnot(isTRUE(all.equal(law_tables[[name]], verified[[name]])),
              identical(data.table::key(law_tables[[name]]),
                        data.table::key(verified[[name]])))
  }
  if (!file.rename(temporary, cache_file)) {
    stop("Could not replace ", cache_file)
  }
  message("Updated ", length(law_tables), " law tables; verified ", length(untouched),
          " other cached objects unchanged.")
  invisible(names(law_tables))
}

if (sys.nframe() == 0L) {
  update_law_data()
}
