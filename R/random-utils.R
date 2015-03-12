#' Gets the last element in a vector.
#' @export
last <- function(x) tail(x, n = 1)

#' The counterpart to isTRUE.
#' @export
isFALSE <- function(x) !isTRUE(x)

#' Capitalizes the first letter of a string.
#' @export
capitalize <- function(x) {
  x %>% substr(., 1, 1) %>% toupper() %>% paste0(., substring(x, 2))
}
