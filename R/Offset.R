

Offset <- function(x, y, a, m) {
  .Call("COffset", x, y, a, m, PACKAGE = packageName())
}

