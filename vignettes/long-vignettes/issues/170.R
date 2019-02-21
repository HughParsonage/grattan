library(data.table)
library(knitr)
library(readxl)
library(magrittr)
library(ggplot2)
library(taxstats)
library(taxstats1516)

# Table 2 individuals has age and taxable status
individuals_table1_1516.tsv <- "~/taxstats/data-raw/2015-16/individuals_table2_201516.tsv"

if (!file.exists(individuals_table1_1516.tsv)) {
  individuals_table1_1516.tsv <- tempfile(fileext = ".tsv")
  res <- 
    download.file("https://github.com/HughParsonage/taxstats/raw/master/data-raw/2015-16/individuals_table1_201516.tsv",
                  destfile = individuals_table1_1516.tsv,
                  method = "wb")
  if (res) {
    if (HTTP_ERROR <- httr::http_error("https://github.com/HughParsonage/taxstats/raw/master/data-raw/2015-16/individuals_table1_201516.tsv")) {
      stop(HTTP_ERROR)
    } else {
      stop("Unable to download file.")
    }
  }
}

individuals_table1_1516 <-
  fread(file = individuals_table1_1516.tsv, sep = "\t")


individuals_table1_1516 %>% 
  .[startsWith(Selected_items, "18")]

age2age_range <- function(age) {
  if_else(age >= 70, 
          0, 
          pmin(11 - {(age - 15) %/% 5}, 11))
}


stopifnot(age2age_range(65) == 1)
stopifnot(age2age_range(64) == 2)
stopifnot(age2age_range(60) == 2)
stopifnot(age2age_range(24) == 10)
stopifnot(age2age_range(20) == 10)
stopifnot(age2age_range(19) == 11)


aus_pop_by_fy_age_range <- 
  grattan:::aus_pop_fy_age(tbl = TRUE) %>%
  .[, .(Population = sum(Population),
        maxAge = max(Age)),
    keyby = .(fy_year, age_range = age2age_range(Age))]


sfa <- get_sample_files_all2()
sfa[, tax := income_tax(Taxable_Income, fy.year = .BY[[1L]], .dots.ATO = .SD),
    keyby = .(fy_year)]
nTaxpayers_by_fy_age_range <- 
  sfa[tax > 0][, .(nTaxpayers = sum(WEIGHT)), keyby = c("fy_year", "age_range")]


format_numbers <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  if (min(x) > -1 && max(x) < 1) {
    rx <- round(x * 100, 1)
    
    return(paste0(formatC(rx, width = 3, flag = "#", digits = 1, format = "f",
                          drop0trailing = FALSE),
                  "%"))
  }
  if (max(abs(x)) > 1e3) {
    return(prettyNum(round(x), big.mark = ","))
  }
  x
}

nTaxpayers_by_fy_age_range[aus_pop_by_fy_age_range,
                           on = c("fy_year", "age_range"),
                           nomatch = 0L] %>%
  .[, P := nTaxpayers / Population] %>%
  .[age_range_decoder, on = "age_range"] %>%
  setnames("age_range_description", "Age") %>%
  .[, age_range := NULL] %>%
  .[, maxAge := NULL] %>%
  setkey(fy_year, Age) %>%
  .[, nTaxpayers := prettyNum(nTaxpayers, big.mark = ",")] %>%
  .[, Population := prettyNum(Population, big.mark = ",")] %>%
  .[, lapply(.SD, format_numbers)] %>%
  set_cols_first(key(.)) %>%
  kable(align = "r")

nTaxpayers_by_fy_age_range[aus_pop_by_fy_age_range,
                           on = c("fy_year", "age_range"),
                           nomatch = 0L] %>%
  .[, P := nTaxpayers / Population] %>%
  .[age_range_decoder, on = "age_range"] %>%
  setnames("age_range_description", "Age") %>%
  .[, age_range := NULL] %>%
  .[, maxAge := NULL] %>%
  setkey(fy_year, Age) %>%
  set_cols_first(key(.)) %>%
  .[, lapply(.SD, format_numbers)] %>%
  .[fy_year %in% yr2fy(c(2004, 2008, 2013)),
    .(Age, fy_year, P)] %>%
  dcast(Age ~ fy_year, value.var = "P") %>%
  kable(align = "r")

nTaxpayers_by_fy_age_range[aus_pop_by_fy_age_range,
                           on = c("fy_year", "age_range"),
                           nomatch = 0L] %>%
  .[, P := nTaxpayers / Population] %>%
  .[, Year := fy2yr(fy_year)] %>%
  .[age_range_decoder, on = "age_range"] %>%
  ggplot(aes(x = Year, 
             y = P,
             group = age_range_description, 
             color = age_range_description)) + 
  geom_line(size = 1.4) + 
  theme_dark() + 
  scale_y_continuous(labels = format_numbers)



