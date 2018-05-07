
if (requireNamespace("taxstats", quietly = TRUE)){
  library(data.table)
  library(taxstats)
  sample_files_all <- taxstats::get_sample_files_all()
}
library(zoo)
