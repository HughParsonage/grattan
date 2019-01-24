library(grattanCharts)
library(SampleFile1516)
library(data.table)
library(magrittr)
library(hutils)

p <- 
  s1516[, .(avgTaxableIncome = mean(Taxable_Income)), keyby = .(age_range, Gender)] %>%
  .[age_range_decoder, on = "age_range"] %>%
  .[, Sex := if_else(Gender == 1, "Female", "Male")] %>%
  .[, Age := 70 - 5 * age_range] %>%
  grplot(aes(x = Age,
             y = avgTaxableIncome,
             group = Sex, 
             color = Sex)) + 
  geom_line() + 
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = grattan_dollar)
save_pptx(p,"vignettes/long-vignettes/issues/164.pptx")

