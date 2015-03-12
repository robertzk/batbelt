#' Converts a number to an ordinal (e.g., first, second, etc.)
#' @param number. The number to convert to ordinal.
#' @export
as.ordinal <- function(num) {
  if (num > 20) stop('Numbers higher than twenty have not been implemented yet.')
  ordinals <- c('first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth')
  if (num <= length(ordinals)) {
    return(ordinals[num])
  } else {
    paste0(num, 'th')
  }
}
