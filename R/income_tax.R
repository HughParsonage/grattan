#' tax function
#' 
#' @param income the personal assessable income
#' @param age the individual's age
#' @param fy.year the financial year in which the income was earned
#' @export 
#' @author Various
#' @return the total personal income tax payable

income_tax <- function(income, age = 44, fy.year = "2012-13"){
  if (fy.year == "2017-18"){
    warning("Uhh, you're applying a (plausible) tax rate to 2017-18.")
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income < 180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    
    # Assumed the levy will go.
    temp.budget.repair.levy <- 0
    
    # includes SAPTO now
    medicare.levy <- ifelse(age >= 65,
                            ifelse(income < 32279,
                                   0,
                                   ifelse(income < 37975,
                                          0.1 * (income - 32279),
                                          0.02 * income)),
                            ifelse(income < 20542, 
                                   0,
                                   ifelse(income <= 20542,
                                          0.1 * (income - 20542),
                                          0.02 * income))
    )
                            
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
    
    return(pmax(tax + temp.budget.repair.levy + medicare.levy + medicare.surcharge - LITO, 0))
  }
  
  if (fy.year == "2014-15"){
    tax <- ifelse(income < 18200, 0, 
                  ifelse(income < 37000, (income-18200)*0.19, 
                         ifelse(income < 80000, 3572 + (income - 37000)*0.325,
                                ifelse(income < 180000, 17547 + 0.37*(income - 80000), 
                                       54547 + 0.45*(income - 180000)))))
    
    temp.budget.repair.levy <- income * ifelse(income > 180000, 0.02, 0)
    
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
    
    return(pmax(tax + temp.budget.repair.levy + medicare.levy + medicare.surcharge - LITO, 0))
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
    
    return(pmax(tax + medicare.levy + medicare.surcharge - LITO, 0))
    
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
                                    income * (ifelse(income <= 84000,
                                                     0,
                                                     ifelse(income <= 97000,
                                                            0.01,
                                                            ifelse(income <= 130000,
                                                                   0.0125,
                                                                   0.0150))) + 
                                                medicare.base.rate)
                             )
    )
                             
    return(pmax(tax + medicare.levy - LITO, 0))
  }
  
  
  if (fy.year == "2011-12"){
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
    flood.levy <- ifelse(income < 50000, 0,
                         ifelse(income < 100000, (income - 50000) * 0.005, 
                                250 + (income - 100000)*0.01))
    
    LITO <- ifelse(income < 30000, 1500,
                   ifelse(income < 65000, 1500 - ((income - 30000)*0.04),
                          0))
    
    return(pmax(tax + medicare.levy + flood.levy - LITO, 0))
  }
}