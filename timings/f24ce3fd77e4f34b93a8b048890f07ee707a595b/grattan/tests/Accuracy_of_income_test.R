# tests

library(taxstats)
library(grattan)
# True value of personal income tax receipts was $159.021 billion
# 5506.0 - Taxation Revenue, Australia, 2013-14



# basic taxable income to tax
test1 <- 
  sample_file_1213 %>%
  mutate(tax0 = income_tax(Taxable_Income),
         tax1 = grattan:::.income_tax(Taxable_Income, "2012-13", sapto = 0, age = 42))

sum(test1$tax0) * 50 / 1e9
# [1] 159.24
sum(test1$tax1) * 50 / 1e9
# [1] 159.24

age_decoder <- 
  read.table(text="Age ranges	Age ranges - description
             0	70 to 75
             1	65 to 69
             2	60 to 64
             3	55 to 59
             4	50 to 54
             5	45 to 49
             6	40 to 44
             7	35 to 39
             8	30 to 34
             9	25 to 29
             10	20 to 24
             11	16 to 20", header = TRUE, sep = "\t") %>%
  setnames(old = names(.), new = c("age_range", "age")) %>%
  mutate(age = sub("\\sto.*$", "", age))

test2 <- 
  test1 %>%
  setkey(age_range) %>%
  .[age_decoder] %>%
  mutate(SAPTO = sapto(rebate_income = rebate_income(Taxable_Income = Taxable_Income, 
                                                     Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt, 
                                                     All_deductible_super_contr = Non_emp_spr_amt, 
                                                     Net_fincl_invstmt_lss_amt = Net_fincl_invstmt_lss_amt,
                                                     Net_rent_amt = Net_rent_amt, 
                                                     Rep_frng_ben_amt = Rep_frng_ben_amt), 
                       fy.year = "2012-13", 
                       sapto.eligible = age >= 65)) %>%
  mutate(tax2 = grattan:::.income_tax(Taxable_Income, 
                                      fy.year = "2012-13", 
                                      sapto = SAPTO),
         rebate_income = rebate_income(Taxable_Income = Taxable_Income, 
                                       Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt, 
                                       All_deductible_super_contr = Non_emp_spr_amt, 
                                       Net_fincl_invstmt_lss_amt = Net_fincl_invstmt_lss_amt,
                                       Net_rent_amt = Net_rent_amt, 
                                       Rep_frng_ben_amt = Rep_frng_ben_amt))

sum(test2$tax2) * 50 / 1e9
# [1] 158.6408

test3 <- 
  test1 %>%
  setkey(age_range) %>%
  .[age_decoder] %>%
  mutate(SAPTO = sapto(rebate_income = rebate_income(Taxable_Income = Taxable_Income, 
                                                     Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt, 
                                                     All_deductible_super_contr = Non_emp_spr_amt, 
                                                     Net_fincl_invstmt_lss_amt = Net_fincl_invstmt_lss_amt,
                                                     Net_rent_amt = Net_rent_amt, 
                                                     Rep_frng_ben_amt = Rep_frng_ben_amt), 
                       fy.year = "2012-13", 
                       sapto.eligible = age >= 65,
                       family_status = ifelse(!Partner_status, "single", "married"))) %>%
  mutate(tax3 = grattan:::.income_tax(Taxable_Income, 
                                      fy.year = "2012-13", 
                                      sapto = SAPTO),
         rebate_income = rebate_income(Taxable_Income = Taxable_Income, 
                                       Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt, 
                                       All_deductible_super_contr = Non_emp_spr_amt, 
                                       Net_fincl_invstmt_lss_amt = Net_fincl_invstmt_lss_amt,
                                       Net_rent_amt = Net_rent_amt, 
                                       Rep_frng_ben_amt = Rep_frng_ben_amt))

sum(test3$tax3) * 50 / 1e9
