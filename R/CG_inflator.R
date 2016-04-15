#' CG_inflator
#' 
#' @param x To be inflated.
#' @param from_fy,to_fy Financial years designating the inflation period.
#' @return An estimate of \code{x} inflated in \code{to_fy}

CG_population_inflator <- function(x = 1, from_fy, to_fy){
  last_fy <- max(from_fy, to_fy)
  last_year <- fy2yr(last_fy)
  
  input <- 
    data.table::data.table(x = x, from_fy = from_fy, to_fy = to_fy)
  
  n_cg_history <- 
    data.table::as.data.table(taxstats::individuals_table1_201314) %>%
    dplyr::filter(Selected_items == "Net capital gain") %>%
    dplyr::filter(!is.na(Count))  %>%
    dplyr::select(fy_year, n_CG = Count)
  
  # Only attempt a forecast if required.
  if (last_year > 2014){
    population_forecast <- 
      forecast::forecast(n_cg_history$n_CG, h = last_year - 2014) 
    
    forecast_tbl <- 
      data.table::data.table(
        fy_year = yr2fy(2015:last_year),
        n_CG = as.numeric(population_forecast$mean)
      )
    out_tbl <- 
      rbind(forecast_tbl, 
            n_cg_history, 
            use.names = TRUE, 
            fill = TRUE)
  } else {
    out_tbl <- n_cg_history
  }
  
  input %>%
    merge(out_tbl, by.x = "from_fy", by.y = "fy_year", all.x = TRUE, sort = FALSE) %>%
    data.table::setnames("n_CG", "n_CG_from") %>% 
    merge(out_tbl, by.x = "to_fy", by.y = "fy_year", all.x = TRUE, sort = FALSE) %>% 
    data.table::setnames("n_CG", "n_CG_to") %$%
    {
      x * n_CG_to / n_CG_from
    }
}
  

CG_inflator <- function(x = 1, from_fy, to_fy){
  prohibit_vector_recycling(x, from_fy, to_fy)
  stopifnot(is.numeric(x), all(is.fy(from_fy)), all(is.fy(to_fy)))

  input <- 
    data.table::data.table(x = x, from_fy = from_fy, to_fy = to_fy)
  
  # discounts
  taxstats::sample_files_all %>%
    dplyr::filter(Net_CG_amt > 0) %>%
    dplyr::mutate(apparent_discount = round(1 - Net_CG_amt / Tot_CY_CG_amt, 1)) %>% 
    dplyr::count(apparent_discount) %>%
    dplyr::mutate(p = n / sum(n))
  
    revenue_foregone_from_sample_files <-
      sample_files_all %>%
      # for sapto purposes, only care whether or not >= 65
      # turns out sapto has little effect. Somewhat surprising given 
      # amount of CG in that age group: should verify.
      # merge(age_range_decoder, by = "age_range") %>%
      # mutate(age = ifelse(age_range_description %in% c("65 to 69", "70 and over"), 70, 42)) %>%
      mutate(age = 42) %>% 
      mutate(tax_no_discount = income_tax(Taxable_Income + ifelse(Tot_CY_CG_amt > 0, 
                                                                  # only change if the discount seems to 
                                                                  # be in effect
                                                                  ifelse(1 - Net_CG_amt / Tot_CY_CG_amt > 0.49,
                                                                         Net_CG_amt,
                                                                         0), 
                                                                  0), fy.year = fy.year, age = age),
             actual_tax = income_tax(Taxable_Income, fy.year = fy.year, age = age)) %>%
      group_by(fy.year) %>%
      summarise(revenue_foregone = sum((tax_no_discount - actual_tax) * WEIGHT))
    
  
  
  cg_table <- 
    taxstats::sample_files_all %>%
    dplyr::select(fy.year, Taxable_Income, Net_CG_amt) %>%
    dplyr::filter(Net_CG_amt > 0) %>%
    dplyr::mutate(marginal_rate_first = income_tax(Taxable_Income + 1, 
                                                   fy.year = fy.year) - income_tax(Taxable_Income, 
                                                                                   fy.year = fy.year)) %>%
    dplyr::mutate(marginal_rate_last = (income_tax(Taxable_Income + Net_CG_amt, fy.year = fy.year) - income_tax(Taxable_Income, fy.year = fy.year)) / Net_CG_amt) %>%
    dplyr::group_by(fy.year) %>%
    dplyr::summarise(mean_mr1 = mean(marginal_rate_first), 
                     mean_wmr1 = weighted.mean(marginal_rate_first, Net_CG_amt), 
                     mean_mrL = mean(marginal_rate_last), 
                     mean_wmrL = weighted.mean(marginal_rate_last, Net_CG_amt)) %>% 
    merge(cgt_expenditures, by.x = "fy.year", by.y = "FY", all = TRUE) %>% 
    # dplyr::select_(.dots = c(-URL, -Projected) %>%
    unselect_(.dots = c("URL", "Projected")) %>%
    dplyr::rename(revenue_foregone = CGT_discount_for_individuals_and_trusts_millions) %>%
    dplyr::mutate(revenue_foregone = revenue_foregone * 10^6,
                  zero_discount_Net_CG_total = revenue_foregone / mean(mean_wmrL, na.rm = TRUE)) %>%
    dplyr::select(fy.year, zero_discount_Net_CG_total)
  
  raw_out <- 
    input %>%
    merge(cg_table, by.y = "fy.year", by.x = "from_fy", all.x = TRUE) %>%
    data.table::setnames("zero_discount_Net_CG_total", "from_cg") %>% 
    merge(cg_table, by.y = "fy.year", by.x = "to_fy", all.x = TRUE) %>%
    data.table::setnames("zero_discount_Net_CG_total", "to_cg") %$%
    {
      x * to_cg / from_cg
    }
  
  # The tax expenditures reflect totals, not means, so we need to adjust for
  # totals.
  raw_out / CG_population_inflator(1, from_fy = from_fy, to_fy = to_fy)
}


#   
#   if (FALSE){
#   net_cg_history <- 
#     data.table::as.data.table(taxstats::individuals_table1_201314) %>%
#     dplyr::filter(Selected_items == "Net capital gain") %>%
#     # Ignore before 2000 (different policy)
#     dplyr::filter(fy_year > "2000-01") %>%
#     dplyr::mutate(Sum_r = Sum / data.table::shift(Sum, type = "lag"))
#   
#   tot_cg_history <- 
#     data.table::as.data.table(taxstats::individuals_table1_201314) %>%
#     dplyr::filter(Selected_items %in% c("Net capital gain", "Total current year capital gains")) %>%
#     # Ignore before 2000 (different policy)
#     dplyr::filter(fy_year > "2000-01") %>% 
#     dplyr::select(-Count) %>%
#     tidyr::spread(Selected_items, Sum) %>%
#     dplyr::mutate(discount = 1 - `Net capital gain` / `Total current year capital gains`)
#   
#   net_cg_history %>%
#     ungroup %>%
#     mutate(Mean = Sum / Count) %>%
#     mutate(Mean = Mean / last(Mean)) %$%
#     forecast::forecast(Mean)
#   
#   n_cg_history <- 
#     data.table::as.data.table(taxstats::individuals_table1_201314) %>%
#     dplyr::filter(Selected_items == "Net capital gain") %$%
#     plot(forecast(Count))
#   
#   
#   } else {
#     grattan:::cgt_expenditures %>%
#       dplyr::mutate(cg_inflator = 1.)
#     
#     
#   }
#   
#   x <- c(1, -1)
#   from_fy <- c("2013-14", "2012-13")
#   to_fy = c("2014-15", "2015-16")
#   
#   input <- 
#     data.table::data.table(cg = x, from_fy = from_fy, to_fy = to_fy)
#   
#   # The Net CG amt under a zero discount is the revenue foregone 
#   # divided by the average tax rate over the taxed portion of 
#   # the whole gain. (Since 50% discount.)
# 
#   

#     
#   trusts_cgt_history <- 
#     trusts_table1_201314 %>% 
#     filter(grepl("Net capital gain", Selected_items)) %>%
#     select(fy.year = fy_year, trusts_cg = Sum)
#   
#   individuals_cgt_history <- 
#     individuals_table1_201314 %>% 
#     filter(grepl("Net capital gain", Selected_items)) %>%
#     select(fy.year = fy_year, individuals_cg = Sum)
#   
#   comparison_of_methods <- 
#     revenue_foregone_from_sample_files %>%
#     merge(grattan:::cgt_expenditures, by.x = "fy.year", by.y = "FY", all.y = TRUE) %>%
#     select(-URL, -Projected) %>%
#     merge(trusts_cgt_history, by = "fy.year", all = TRUE) %>%
#     merge(individuals_cgt_history, by = "fy.year", all = TRUE) %>%
#     mutate(revenue_foregone_Treasury = CGT_discount_for_individuals_and_trusts_millions * 10^6) %>%
#     select(-CGT_discount_for_individuals_and_trusts_millions) %>%
#     mutate(difference = revenue_foregone - revenue_foregone_Treasury) %>%
#     mutate(more_trusts = ifelse(trusts_cg > individuals_cg, "trusts higher", "trusts lower"))
#   
#   comparison_of_methods %>%
#     ggplot(aes(x = trusts_cg, y = difference)) + 
#     geom_point() + 
#     geom_smooth() + 
#     coord_equal()
#   
#   comparison_of_methods %>%
#     ggplot(aes(x = fy.year, y = difference, fill = factor(more_trusts))) + 
#     geom_bar(stat = "identity") 
#   
#   comma_bn <- function(x) round(x/1e9, 1)
#   
#   comparison_of_methods %>% mutate_each(funs(comma_bn), -fy.year, -more_trusts) %>% .[]
#   
#   taxstats::sample_file_1314 %>%
#     project(h = 1L) %>%
#     # since 50% discount just add Net_CG_amt
#     dplyr::mutate(Taxable_Income_no_discount = Taxable_Income + Net_CG_amt)
# }