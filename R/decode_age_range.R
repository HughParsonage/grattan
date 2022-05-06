

decode_age_range <- function(x, nThread = getOption("grattan.nThread", 1L)) {
  .Call("Cdecode_age_range", x, nThread, PACKAGE = "grattanDev")
}

