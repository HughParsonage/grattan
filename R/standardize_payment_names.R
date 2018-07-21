

standardize_payment_names <- function(payment_name) {
  .pn <-
    payment_name %>%
    trimws %>%
    tolower %>%
    gsub("[^a-z]+", " ", x = .)
  
  Switch(.pn, 
         "abstudy" = "abstudy",
         
         "austudy" = "austudy",
         
         "dsp" = "disability pension",
         "disability support pension" = "disability pension",
         
         "ftba" = "ftba",
         "ftb a" = "ftba",
         "family tax benefit a" = "ftba",
         
         "ftb" = "ftbb",
         "ftb b" = "ftbb",
         "family tax benefit b" = "ftbb",
         
         "nsa" = "newstart allowance",
         "newstart" = "newstart allowance", 
         
         "parenting payment" = "parenting payment",
         
         "rent assistance" = "rent assistance",
         "commonwealth rent assistance" = "rent assistance",
         "cra" = "rent assistance",
         
         "ya" = "youth allowance",
         
         .pn)
}


