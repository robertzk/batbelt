#' Gets the last element in a vector.
#' @export
last <- function(x) tail(x, n = 1)

#' The counterpart to isTRUE.
#' @export
isFALSE <- function(x) !isTRUE(x)
