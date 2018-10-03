library(data.table)
library(magrittr)

# http://guides.dss.gov.au/guide-social-security-law/5/2/5/20
carers_tbl <- 
  fread("01/07/1999	75.60
01/01/2000	76.40
        01/07/2000	79.50
        01/01/2001	82.00
        01/01/2002	85.30
        01/01/2003	87.70
        01/01/2004	90.10
        01/01/2005	92.40
        01/01/2006	94.70
        01/01/2007	98.50
        01/01/2008	100.60
        01/01/2009	105.10
        01/01/2010	106.70
        01/01/2011	110.00
        01/01/2012	114.00
        01/01/2013	115.40
        01/01/2014	118.20
        01/01/2015	121.70
        01/01/2016	123.50
        01/01/2017	124.70
        01/01/2018	127.10", 
        sep = "\t",
        header = FALSE) %>%
  .[, .(Date = as.Date(V1, format = "%d/%m/%Y"), 
        CarersPayment = V2)] %>%
  setkey(Date) %>%
  .[]

saveRDS(carers_tbl, "inst/extdata/carers_tbl.rds", compress = FALSE)


