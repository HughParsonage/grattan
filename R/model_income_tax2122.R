
# library(knitr)
# library(data.table)
# library(hutils)
# library(hutilscpp)
if (F) {
library(ggplot2)
library(magrittr)
library(hutilscpp)
library(data.table)
library(grattan)
library(hutils)

MultiOffset <- grattan:::MultiOffset


lmito_2122 <- function(x, first_offset = 255, thresholds = c(37e3, 48e3, 90e3, 126e3), taper = c(0, 0.075, 0, -0.03)) {
  # diff of thresholds
  dth <- diff(thresholds)
  
  fcase(x < thresholds[1], first_offset, 
        x < thresholds[2], first_offset + taper[2] * (x - thresholds[1]),
        x < thresholds[3], first_offset + taper[2] * dth[1] + taper[3] * (x - thresholds[2]),
        x < thresholds[4], first_offset + taper[2] * dth[1] + taper[3] * dth[2] + taper[4] * (x - thresholds[3]),
        x >= last(thresholds), 0)
}

emp_forecasts <- list(fy_year = as.character(fy::yr2fy(2019:2030)),
                      r = c(2.5, 0.2, -1.3, 2.5, 2.6, 2.4, rep(2.4, 6)) / 100) # employment for 2018-19 from MYEFO and forecast from Deloitte p.9

wage_forecasts <- list(fy_year = as.character(fy::yr2fy(2019:2030)),
                       r = c(2.3, 2.1, 0.9, 0.8, 1.0, 1.8, rep(1.8, 6)) / 100) # wage price index for 2018-19 from MYEFO and forecast from Deloitte p.93


futureIncomeTax <- function(x, stage = c("stage1", "stage2", "stage3")) {
  switch(match.arg(stage), 
         "stage1" = IncomeTax(x, 
                              thresholds = c(0, 18200, 37e3, 90e3, 180e3),
                              rates = c(0, 0.19, 0.325, 0.37, 0.45)),
         "stage2" = IncomeTax(x,
                              thresholds = c(0, 18200, 45e3, 120e3, 180e3),
                              rates = c(0, 0.19, 0.325, 0.37, 0.45)),
         "stage3" = IncomeTax(x, 
                              thresholds = c(0, 18200, 45e3, 200e3),
                              rates = c(0, 0.19, 0.30, 0.45)))
}

future_lmito <- function(x) {
  lmito_2122(x)
  # grattan:::MultiOffset(x, first_offset = 255, thresholds = c(37e3, 48e3, 90e3, 126e3), taper = c(0, 0, 0.075, 0, -0.03))
}

future_lito <- function(x, stage = c("stage1", "stage2", "stage3")) {
  switch(match.arg(stage),
         "stage1" = MultiOffset(x, first_offset = 445, thresholds = c(37000), taper = c(-0.015)),
         "stage2" = MultiOffset(x, first_offset = 700, thresholds = c(37500, 45e3), taper = c(-0.050, -0.015)),
         "stage3" = MultiOffset(x, first_offset = 700, thresholds = c(37500, 45e3), taper = c(-0.050, -0.015)))
}

future_taxable_incomes <- function(fy_year) {
  baseline_stage <- 
    switch(fy_year,
           "2019-20" = "stage1",
           "2020-21" = "stage1",
           "2021-22" = "stage1",
           "2022-23" = "stage2",
           "2023-24" = "stage2",
           "2024-25" = "stage3",
           "stage3")
    
  
  fread("../taxstats1718/2018_sample_file.csv")  %>% 
    project_to(fy_year, 
               wage.series = wage_forecasts,
               lf.series = emp_forecasts,
               fy.year.of.sample.file = "2017-18") %>%
    selector(Gender, Taxable_Income, WEIGHT) %>%
    .[, lmito := if (fy_year <= "2021-22") future_lmito(Taxable_Income) else 0] %>%
    .[, baseline_tax := 
        pmax0(futureIncomeTax(Taxable_Income, stage = baseline_stage) - 
                future_lito(Taxable_Income, stage = baseline_stage) - 
                lmito)] %>%
    .[]
}

future_taxable_incomes <- memoise::memoise(future_taxable_incomes)


bring_forward_stage2_to_202122 <- function(fy.year, stage = c("stage2", "stage3"), keep_lmito = NA) {
  ti <- future_taxable_incomes(fy.year)[["Taxable_Income"]]
  
  if (is.na(keep_lmito)) {
    keep_lmito <- fy.year %in% yr2fy(2019:2022)
  }
  if (keep_lmito) {
    LMITO <- future_lmito(ti)
  } else {
    LMITO <- 0
  }
  the_stage <- match.arg(stage)
  
  LITO <- future_lito(ti, stage = the_stage)
  OrdinaryTax <- futureIncomeTax(ti, stage = the_stage)
  future_taxable_incomes(fy.year)[, new_tax := pmax0(OrdinaryTax - LITO - LMITO)]
}


full_table <- 
  CJ(fy_year = yr2fy(2021:2030), 
     stage = c("stage2", "stage3"), 
     keep_lmito = c(NA, FALSE, TRUE)) %>%
  .[, bring_forward_stage2_to_202122(.BY[["fy_year"]], .BY[["stage"]], .BY[["keep_lmito"]]),
    by = .(fy_year, stage, keep_lmito)] %>%
  mutate_ntile("Taxable_Income", n = 5L, keyby = c("fy_year", "stage", "keep_lmito")) %>%
  .[, revenue := new_tax - baseline_tax] %>%
  .[]

library(grattantheme)
grattan_save_all <- function(filename, object) {
  grattantheme::grattan_save(filename = filename,
                             object = object,
                             type = "all",
                             save_pptx = TRUE,
                             save_data = TRUE)
}

dropbox_charts_dir <- 
  file.path(grattandata::get_dropbox_location(), 
            r'{\Income taxes\Data and analysis\HP-stage3-stage2-lmito\charts}')

full_table %>% 
  .[, .(unwt_tot_revenue = sum(revenue), 
        wt = first(WEIGHT)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile)] %>%
  .[, tot_revenue := unwt_tot_revenue * wt / 1e9] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  ggplot(aes(x = fy_year, y = tot_revenue, fill = Quintile)) + 
  geom_col() + 
  grattan_fill_manual(n = 5L) +
  facet_wrap(~stage + keep_lmito) +
  theme_grattan() + 
  scale_y_continuous_grattan(labels = grattanCharts::grattan_dollar)

full_table %>% 
  .[, .(avgTaxChange = mean(revenue), 
        wt = first(WEIGHT)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile)] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[] %>%
  ggplot(aes(x = fy_year, y = avgTaxChange, fill = Quintile)) + 
  geom_col(position = "dodge") + 
  grattan_fill_manual(n = 5L) +
  facet_wrap(~stage + keep_lmito) +
  theme_grattan() + 
  scale_y_continuous_grattan(labels = grattanCharts::grattan_dollar)

full_table %>% 
  .[, .(unwt_tot_revenue = sum(revenue), 
        wt = first(WEIGHT)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile)] %>%
  .[, tot_revenue := unwt_tot_revenue * wt / 1e9] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  fwrite(file.path(dropbox_charts_dir, "tot_revenue-by-fy-stage-lmito-quintile.csv"))


full_table %>% 
  .[, .(avgTaxChange = mean(revenue), 
        wt = first(WEIGHT)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile)] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  fwrite(file.path(dropbox_charts_dir, "avg_revenue-by-fy-stage-lmito-quintile.csv"))

full_table %>% 
  .[, .(unwt_tot_revenue = sum(revenue), 
        wt = first(WEIGHT)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile, Gender)] %>%
  .[, tot_revenue := unwt_tot_revenue * wt / 1e9] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  fwrite(file.path(dropbox_charts_dir, "tot_revenue-by-fy-stage-lmito-quintile-gender.csv"))


full_table %>% 
  .[, .(avgTaxChange = mean(revenue), 
        wt = first(WEIGHT)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile, Gender)] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  fwrite(file.path(dropbox_charts_dir, 
                   "avg_revenue-by-fy-stage-lmito-quintile-gender.csv"))


tot_revenue_vs_taxyear_by_quintile_stage <- 
  full_table %>% 
  .[, .(unwt_tot_revenue = sum(revenue), 
        wt = first(WEIGHT),
        minIncome = min(Taxable_Income)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile)] %>%
  .[, tot_revenue := round(unwt_tot_revenue * wt / 1e9, 1)] %>%
  .[, Quintile := factor(paste0("Q", Taxable_IncomeQuintile))] %>%
  .[, unwt_tot_revenue := NULL] %>%
  .[, Taxable_IncomeQuintile := NULL] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  .[fy_year <= "2023-24"] %>%
  .[implies(fy_year > "2021-22", stage == "stage3")] %>%
  .[keep_lmito != "Keep LMITO"] %>%
  .[order(stage, keep_lmito, fy_year)] %>%
  .[, fy_year := paste0("'", substr(fy_year, 6, 7))] %>%
  .[, stage := Switch(stage, "stage2" = "Stage 2", "stage3" = "Stage 3", DEFAULT = "")] %>%
  ggplot(aes(x = fy_year, y = tot_revenue, fill = Quintile)) + 
  geom_col() + 
  geom_hline(yintercept = 0, color = grattan_grey5) +
  theme_grattan() + 
  xlab("Tax year ending") +
  theme(panel.background = element_rect(fill = alpha(grattan_orange_alpha, 0.33))) +
  scale_y_continuous_grattan(labels = grattanCharts::grattan_dollar, expand_bottom = 0.015) + 
  grattan_fill_manual(n = 5L) +
  # https://github.com/teunbrand/ggh4x 
  ggh4x::facet_nested(keep_lmito ~ stage + Quintile, scales = "free_x", space = "free_x") + 
  ggtitle("Change in revenue (billions)", 
          subtitle = "Negative values indicate a tax decrease")


tempd <- provide.dir(tempfile(pattern = "inc-charts"))
tempf <- file.path(tempd, "tot_revenue_vs_taxyear_by_quintile_stage.pdf")
grattan_save_all(tot_revenue_vs_taxyear_by_quintile_stage, filename = tempf)
fs::dir_copy(tempd, 
             dropbox_charts_dir,
             overwrite = TRUE)

avg_revenue_vs_taxyear_by_quintile_stage <- 
  full_table %>% 
  .[, .(unwt_tot_revenue = mean(revenue), 
        wt = first(WEIGHT),
        minIncome = min(Taxable_Income)), 
    keyby = .(fy_year, stage, keep_lmito, Taxable_IncomeQuintile)] %>%
  .[, tot_revenue := round(unwt_tot_revenue, 1)] %>%
  .[, Quintile := factor(Taxable_IncomeQuintile)] %>%
  .[, unwt_tot_revenue := NULL] %>%
  .[, Taxable_IncomeQuintile := NULL] %>%
  .[, keep_lmito := as.character(fifelse(keep_lmito, "Keep LMITO", "Drop LMITO", "LMITO as legis."))] %>%
  .[] %>%
  .[fy_year <= "2023-24"] %>%
  .[implies(fy_year > "2021-22", stage == "stage3")] %>%
  .[keep_lmito != "Keep LMITO"] %>%
  .[order(stage, keep_lmito, fy_year)] %>%
  .[, fy_year := paste0("'", substr(fy_year, 6, 7))] %>%
  .[, stage := Switch(stage, "stage2" = "Stage 2", "stage3" = "Stage 3", DEFAULT = "")] %>%
  ggplot(aes(x = fy_year, y = tot_revenue, fill = Quintile)) + 
  geom_col() + 
  geom_hline(yintercept = 0, color = grattan_grey5) +
  theme_grattan() + 
  xlab("Tax year ending") + 
  theme(panel.background = element_rect(fill = alpha(grattan_orange_alpha, 0.33))) +
  scale_y_continuous_grattan(labels = grattanCharts::grattan_dollar, expand_bottom = 0.015) + 
  grattan_fill_manual(n = 5L) +
  # https://github.com/teunbrand/ggh4x 
  ggh4x::facet_nested(keep_lmito ~ stage + Quintile, scales = "free_x", space = "free_x")

tempd <- provide.dir(tempfile(pattern = "avg-charts"))
tempf <- file.path(tempd, "avg_revenue_vs_taxyear_by_quintile_stage.pdf")
grattan_save_all(avg_revenue_vs_taxyear_by_quintile_stage, filename = tempf)
fs::dir_copy(tempd, 
             dropbox_charts_dir,
             overwrite = TRUE)

fst::write_fst(full_table, file.path(dropbox_charts_dir, "..", "full_table.fst"), 
               compress = 100)
}
