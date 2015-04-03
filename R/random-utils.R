#' Gets the last element in a vector.
#' @export
last <- function(x) tail(x, n = 1)

#' The counterpart to isTRUE.
#' @export
isFALSE <- function(x) identical(x, FALSE)

#' Capitalizes the first letter of a string.
#' @import magrittr
#' @export
capitalize <- function(x) {
  x %>% substr(., 1, 1) %>% toupper() %>% paste0(., substring(x, 2))
}
