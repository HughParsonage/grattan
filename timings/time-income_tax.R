library(devtools)

Ref <- commandArgs(trailingOnly = TRUE)
if (!dir.exists(Ref)) {
  dir.create(Ref)
  
  devtools::install_github('hughparsonage/grattan', quiet = FALSE,
                           ref = Ref,
                           args = paste0('--library="', normalizePath(Ref, winslash = "/"), '"'))
} 

library(data.table)
library("grattan", lib.loc = Ref)
library(microbenchmark)
library(taxstats)
s1314 <- as.data.table(sample_file_1314)

mi <- as.data.frame(microbenchmark(Ref_DT = income_tax(s1314$Taxable_Income, "2013-14"), times = 50L))
mi[["commit"]] <- Ref
fwrite(as.data.table(mi), paste0("income-tax-201314_", Ref, ".tsv"), sep = "\t")