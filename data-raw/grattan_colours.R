Orange <- grDevices::rgb(243,144,29, maxColorValue=256)
OrangeBackground <- grDevices::rgb(254,240,222, maxColorValue = 256)
DarkOrange <- "#D4582A"  # Dark orange
theGrey <- grDevices::rgb(106,115,123, maxColorValue=256)

col.1 <- "#FFE07F" # Lightest yellow
col.2 <- "#FFC35A" # light yellow
col.3 <- "#F68B33" # Regular orange
col.4 <- "#D4582A" # Ochre 
col.5 <- "#A02226" # Maroon
col.6 <- "#621214" # brown

pal.1 <- col.3
pal.2 <- c(col.4, col.2)
pal.2dark <- c(col.5, col.3)
pal.3 <- c(col.5, col.4, col.3)
pal.4 <- c(col.5, col.4, col.3, col.2)
pal.5 <- c(col.5, col.4, col.3, col.2, col.1)
pal.6 <- c(col.6, col.5, col.4, col.3, col.2, col.1)
pal.7 <- c("black", col.6, col.5, col.4, col.3, col.2, col.1)
pal.8 <- c("black", theGrey, col.6, col.5, col.4, col.3, col.2, col.1)
pal.9 <- c("black", theGrey, col.6, col.5, col.4, col.3, col.2, col.1, "white")

grey.pal  <- c(
  grDevices::rgb(217,217,217, maxColorValue=256),
  grDevices::rgb(174,174,174, maxColorValue=256),
  grDevices::rgb(130,130,130, maxColorValue=256),
  grDevices::rgb(87, 87, 87, maxColorValue=256),
  grDevices::rgb(43, 43, 43, maxColorValue=256),
  "black"
)

devtools::use_data(Orange, 
                   OrangeBackground, 
                   DarkOrange, 
                   theGrey, 
                   col.6, col.5, col.4, col.3, col.2, col.1, 
                   pal.1, pal.2, pal.2dark, pal.3, pal.4, pal.5, pal.6, pal.7, pal.8, pal.9, 
                   grey.pal, 
                   
                   internal = FALSE)