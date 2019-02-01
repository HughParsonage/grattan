library(data.table)
library(SampleFile1516)
library(SampleFile1415)
library(grattan)
library(hutils)
library(magrittr)
library(ggplot2)
library(grattanCharts)
library(scales)

s1516 <- as.data.table(sample_file_1516)
s1415 <- as.data.table(sample_file_1415)
s1516[, Sex := c("Male", "Female")[Gender + 1L]]


s1516[, tax := income_tax(Taxable_Income, "2015-16", .dots.ATO = copy(s1516))]
s1516[, tax_no_frnk := tax + (tax <= Dividends_franking_cr_amt) * Dividends_franking_cr_amt]
s1516[, isAffected := tax_no_frnk > tax]
s1516[, delta := tax_no_frnk - tax]

s1415[, tax := income_tax(Taxable_Income, "2014-15", .dots.ATO = copy(s1415))]
s1415[, tax_no_frnk := tax + (tax <= Dividends_franking_cr_amt) * Dividends_franking_cr_amt]
s1415[, isAffected := tax_no_frnk > tax]
s1415[, delta := tax_no_frnk - tax]

# Percent women:
s1415[, .(isAffected = mean(isAffected)), keyby = "Gender"]
s1516[, .(isAffected = mean(isAffected)), keyby = "Gender"]

# Affected and women
s1516[(isAffected), mean(Gender)]
# 0.5582023

# Affected and over 60
s1415[(isAffected & Gender == 1), mean(age_range <= 2)]
# 0.6792155

# Average sizes by gender
s1516[, .(avgAffected = mean(Dividends_franking_cr_amt)), keyby = "Gender"]
s1516[Dividends_franking_cr_amt > 0, .(avgAffected = mean(Dividends_franking_cr_amt)), keyby = "Gender"]
s1516[(isAffected), .(avgAffected = mean(Dividends_franking_cr_amt)), keyby = "Gender"]

s1516[, .(avgDelta = mean(delta)), keyby = "Sex"]
s1516[(isAffected), .(avgDelta = mean(delta)), keyby = "Sex"]

