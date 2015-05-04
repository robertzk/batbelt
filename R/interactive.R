#' Stop if the user is not in interactive mode.
#' Prevents code from being run outside the R console.
#' @export
ensure_in_interactive <- function() {
  if (!interactive()) {
    stop("This code should only be run in interactive mode.")
  }
}

#' Stop if the user is in interactive mode.
#' Prevents code from being run inside the R console.
#' @export
ensure_not_in_interactive <- function() {
  if (interactive()) {
    stop("This code should not be run in interactive mode.")
  }
}
