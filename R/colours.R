#' The Grattan colours
#'
#' Simply hardcodes the allowed colours. 
#' The Grattan principal orange
#' @name colours
delayedAssign("Orange", rgb(243,144,29, maxColorValue=256))
delayedAssign("theGrey", rgb(106,115,123, maxColorValue=256))

delayedAssign("col.1", "#FFE07F") # Lightest yellow
delayedAssign("col.2", "#FFC35A") # light yellow
delayedAssign("col.3", "#F68B33") # Regular orange
delayedAssign("col.4", "#D4582A") # Ochre 
delayedAssign("col.5", "#A02226") # Maroon
delayedAssign("col.6", "#621214") # brown

delayedAssign("pal.1", col.3)
delayedAssign("pal.2", c(col.4, col.2))
delayedAssign("pal.2dark", c(col.5, col.3))
delayedAssign("pal.3", c(col.5, col.4, col.3))
delayedAssign("pal.4", c(col.5, col.4, col.3, col.2))
delayedAssign("pal.5", c(col.5, col.4, col.3, col.2, col.1))
delayedAssign("pal.6", c(col.6, col.5, col.4, col.3, col.2, col.1))