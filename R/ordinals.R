#' Converts a number to an ordinal (e.g., first, second, etc.)
#' @param num numeric. The number to convert to ordinal.
#' @export
as.ordinal <- function(num) {
  ordinals <- list('first', 'second', 'third', 'fourth', 'fifth',
    'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh',
    'twelfth', 'thirteenth', 'fourteenth', 'fifteenth',
    'sixteenth', 'seventeenth', 'eighteenth', 'nineteenth',
    'twentieth')
  ext <- c("th", "st", "nd", "rd", rep("th", 6))
  ordinals[num][[1]] %||%
  paste0(num, ext[[(num %% 10) + 1]])
}
