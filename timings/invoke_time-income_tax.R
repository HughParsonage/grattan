library(data.table)
# setwd("..")
# income_tax_commits <- system('git log --follow R/income_tax.R', intern = TRUE)
# setwd("timings")

commits <- 
  data.table(Date = anytime::anytime(sub("Date:\\s+", "", 
                                         income_tax_commits[startsWith(income_tax_commits, "Date:")])),
             commit = sub("commit ", "", income_tax_commits[startsWith(income_tax_commits, "commit")]))

for (i in 1:nrow(commits)) {
  c <- commits[i][["commit"]]
  cat(commits[i][["Date"]], "\n")
  system(sprintf('Rscript time-income_tax.R %s', c), intern = TRUE)
}

if (FALSE) {
  lapply(dir(pattern = "^income-tax.*csv$"),
         fread) %>%
    rbindlist(use.names = TRUE, fill = TRUE)
}
