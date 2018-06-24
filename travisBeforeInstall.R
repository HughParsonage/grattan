if (getRversion() >= "3.6") {
  # dev changes to ALTREP mean release is broken
  if (!requireNamespace("data.table", quietly = TRUE)) {
    install.packages("data.table")
  } 
  if (utils::packageVersion("data.table") < "1.11.0") {
    update.packages("data.table")
  }
  data.table::update.dev.pkg()
}
