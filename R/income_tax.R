#' tax function
#' 
#' @param income the personal assessable income
#' @param age the individual's age
#' @param fy.year the financial year in which the income was earned
#' @param return.mode use numeric or integer
#' @export 
#' @author Various
#' @return the total personal income tax payable

income_tax <- function(income, fy.year = "2012-13", include.temp.budget.repair.levy = FALSE, return.mode = "numeric", age = 44, age_group, is.single = TRUE){
  # If not applicable:
  LITO <- 0
  SAPTO <- grattan:::.sapto(income, age, age_group, is.single, fy.year = fy.year)
  medicare.levy <- 0.015 * income
  flood.levy <- 0
  
  if (fy.year == "2017-18" | fy.year == "2015-16"){
    warning("Uhh, you're applying a (plausible) tax rate to 2017-18.")
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income < 180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    
    # Assumed the levy will go.
    if(isTRUE(include.temp.budget.repair.levy)){
      temp.budget.repair.levy <- ifelse(income < 180e3,
                                        0,
                                        0.02 * (income - 180e3))
    } else {
      temp.budget.repair.levy <- 0
    }
    
    medicare.levy <- ifelse(income < 20896,
                            0,
                            ifelse(income < 26121,
                                   0.10 * (income - 20896),
                                   0.02 * income))
    
    medicare.surcharge <- 0
    
    LITO <- ifelse(income < 37000, 445,
                   ifelse(income < 66667, 445 - ((income - 37000)*0.015),
                          0))
    
    out <- pmax(tax + temp.budget.repair.levy + medicare.levy + medicare.surcharge - LITO - SAPTO, 0)
  }
  
  if (fy.year == "2014-15"){
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income < 180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    
    if(isTRUE(include.temp.budget.repair.levy)){
      temp.budget.repair.levy <- ifelse(income < 180e3,
                                        0,
                                        0.02 * (income - 180e3))
    } else {
      temp.budget.repair.levy <- 0
    }
    
    medicare.levy <- ifelse(income < 20542, 
                            0,
                            ifelse(income <= 20542,
                                   0.1 * (income - 20542),
                                   0.02 * income))
    
    # We assume no-one pays the medicare surcharge
    medicare.surcharge <- 0 * income * ifelse(income <= 88000,
                                          0,
                                          ifelse(income <= 102000,
                                                 0.01,
                                                 ifelse(income <= 136000,
                                                        0.0125,
                                                        0.015)))
    LITO <- ifelse(income < 37000, 445,
                   ifelse(income < 66667, 445 - ((income - 37000)*0.015),
                          0))
    
    out <- pmax(tax + temp.budget.repair.levy + medicare.levy + medicare.surcharge - LITO - SAPTO, 0)
  }
  

  
  if (fy.year == "2013-14"){
    
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income<180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    #ATO
    medicare.levy <- ifelse(income < 20542,0, 
                            ifelse(income < 24167, (income - 20542)*.1,
                                   0.015*income))
    # https://www.ato.gov.au/Individuals/Medicare-levy/Medicare-levy-surcharge/
    # no medicare surcharge
    medicare.surcharge <- 0 * income * ifelse(income <= 88000,
                                          0,
                                          ifelse(income <= 102000,
                                                 0.01,
                                                 ifelse(income <= 136000,
                                                        0.0125,
                                                        0.015)))
    
    
    LITO <- ifelse(income < 37000, 445,
                   ifelse(income < 66667, 445 - ((income - 37000)*0.015),
                          0))
    
    out <- pmax(tax + medicare.levy + medicare.surcharge - LITO - SAPTO, 0)
    
  }
  
  if (fy.year == "2012-13"){
    ML.lower <- 20542
    ML.upper <- 24167
    medicare.base.rate <- 0.015
    
    LITO <- ifelse(income < 37000, 
                   445,
                   ifelse(income < 66667, 
                          445 - ((income - 37000)*0.015),
                          0))
    
    tax <- ifelse(income < 18200, 
                  0, 
                  ifelse(income < 37000, 
                         0.19*(income - 18200), 
                         ifelse(income < 80000, 
                                3572 + 0.325*(income - 37000),
                                ifelse(income < 180000, 
                                       17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    # medicare.levy 
    
    
    medicare.levy <- ifelse (income <= ML.lower,
                             0,
                             ifelse(income > ML.lower & income <= ML.upper,
                                    0.10 * (income - 20542),
                                    0.015 * income
                             )
    )
                             
    out <- pmax(tax + medicare.levy - LITO - SAPTO, 0)
  }
  
  
  if (fy.year %in% c("2011-12", "2010-11")){
    tax <- ifelse(income < 6000, 0, 
                  ifelse(income < 37000, (income - 6000) * 0.15, 
                         ifelse(income < 80000, 4650 + (income - 37000) * 0.30,
                                ifelse(income < 180000, 17550 + (income - 80000) * 0.37, 
                                       54550 + 0.45*(income - 180000)))))
    
    #http://www.lewistaxation.com.au/tax/historic-tax/medicare-levy-historical
    medicare.levy <- ifelse(income < 19405,0, 
                            ifelse(income < 22829, (income - 19405)*.1,
                                   0.015*income))
    #Plunky
    if (fy.year == "2011-12"){
    flood.levy <- ifelse(income < 50000, 0,
                         ifelse(income < 100000, (income - 50000) * 0.005, 
                                250 + (income - 100000)*0.01))
    } else {
    flood.levy <- 0
    }
    
    LITO <- ifelse(income < 30000, 1500,
                   ifelse(income < 65000, 1500 - ((income - 30000)*0.04),
                          0))
                          
    out <- pmax(tax + medicare.levy + flood.levy - LITO - SAPTO, 0)
    
  }
  
  if (fy.year == "2009-10"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 35e3,
                         0.15 * (income - 6000),
                         ifelse(income < 80e3,
                                4350 + 0.30*(income - 35e3),
                                ifelse(income < 180e3,
                                       17850 + 0.38*(income - 80e3),
                                       55850 + 0.45*(income - 180e3))
                         )
                  )
    )
    
    medicare.levy <- ifelse(income < 18840, 
                            0, 
                            ifelse(income < 22164,
                                   0.100*(income - 18840),
                                   0.015*income)
    )
    
    
    # http://www.lewistaxation.com.au/tax/historic-tax/low-income-tax-offset-historical
    LITO <- ifelse(income < 30e3,
                   1350,
                   ifelse(income < 63750,
                          1350 - 0.04*(income - 30e3),
                          0))
    
    out <- pmax(tax + medicare.levy  - LITO, 0)
    
  }
  
  if (fy.year == "2008-09"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 34000,
                         0.15 * (income - 6000),
                         ifelse(income < 80000,
                                4200 + 0.30*(income - 34000),
                                ifelse(income < 180e3,
                                       18000 + 0.40*(income - 80000),
                                       58000 + 0.45*(income - 180e3)))))
    
    LITO <- ifelse(income < 30e3,
                   1200,
                   ifelse(income < 60e3,
                          1200 - 0.04 * (income - 30e3),
                          0))
  }
  
  if (fy.year == "2007-08"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 30e3,
                         0.15 * (income - 6000),
                         ifelse(income < 75e3,
                                3600 + 0.30*(income - 30e3),
                                ifelse(income < 150e3,
                                       17100 + 0.40*(income - 75e3),
                                       47100 + 0.45*(income - 150e3)))))
    
    LITO <- ifelse(income < 30e3,
                   750,
                   ifelse(income < 48750,
                          750 - 0.04 * (income - 30e3),
                          0))
    
    
  }
  
  if (fy.year == "2006-07"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 25e3,
                         0.15 * (income - 6000),
                         ifelse(income < 75e3,
                                2850 + 0.30 * (income - 25e3),
                                ifelse(income < 150e3,
                                       17850 + 0.40 * (income - 75e3),
                                       47850 + 0.45 * (income - 150e3)))))
    
    LITO <- ifelse(income < 25e3,
                   600,
                   ifelse(income < 40e3,
                          600 - 0.04 * (income - 25e3),
                          0))
  }
  
  if (fy.year == "2005-06"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.15 * (income - 6000),
                         ifelse(income < 63e3,
                                2340 + 0.30*(income - 21600),
                                ifelse(income < 95e3,
                                       14760 + 0.42*(income - 63e3),
                                       28200 + 0.47*(income - 95e3)))))
  }
  
  if (fy.year == "2004-05"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.17 * (income - 6000),
                         ifelse(income < 58e3,
                                2652 + 0.30*(income - 21601),
                                ifelse(income < 70e3,
                                       13572 + 0.42*(income - 58e3),
                                       18612 + 0.47*(income - 70e3)))))
  }
  
  if (fy.year == "2004-05"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.17*(income - 6000),
                         ifelse(income < 52000,
                                2652 + 0.30*(income - 21601),
                                ifelse(income < 62500,
                                       11772 + 0.42*(income - 52000),
                                       16182 + 0.47*(income - 62500)))))
  }
  
  if (fy.year == "2003-04"){
    tax <- ifelse(income < 6000,
                  0,
                  ifelse(income < 21600,
                         0.17*(income - 6000),
                         ifelse(income < 52e3,
                                2652 + 0.30 * (income - 21600),
                                ifelse(income < 62500,
                                       11771 + 0.42*(income - 52000),
                                       16182 + 0.47*(income - 62500)))))
  }
  
  #
  #
  
  if (fy.year %in% grattan::yr2fy(2004:2010))
    out <- pmax(tax + medicare.levy + flood.levy - LITO, 0)
  
  
  if (return.mode == "integer")
    return(as.integer(floor(out)))
  else
    return(out)
}
