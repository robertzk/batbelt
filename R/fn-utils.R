#' Delays the processing of a function by a certain amount of seconds.
#'
#' Useful for handling API calls and other processes where you don't want to overload things.
#' Taken from http://adv-r.had.co.nz/Function-operators.html by Hadley Wickham.
#' Intended to be used with magrittr <https://github.com/smbache/magrittr>
#'
#' @param f function. The function to run, with a delay.
#' @param delay numeric. The number of seconds to delay.
#' @return a modified version of f to include a delay prior to running.
#' @examples
#' 2 %>% (identity %>% delay_by(10))
#' @export
delay_by <- function(f, delay) {
  force(f); force(delay)
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}


#' Displays a progress bar for a given function.
#'
#' Taken from http://adv-r.had.co.nz/Function-operators.html by Hadley Wickham.
#' Intended to be used with magrittr <https://github.com/smbache/magrittr>
#'
#' @param f function. The function to run with the progress bar.
#' @param bar character. The name of the progress bar to use.  Default ('default') is to display a "." every 1 function run.
#' @param default.n integer. If using the default progress bar, display every n function runs instead.
#' @examples
#' seq(10) %>% (identity %>% progress_bar('default', 2))
#' @export
progress_bar <- function(f, bar = 'default', default.n = 1) {
  if (!bar %in% c('default')) {  #TODO: Implement more progress bars; see plyr
    warning('Progress bar', sQuote(bar), 'not found.  Defaulting.')
    bar <- 'default'
  }
  force(f); force(default.n)
  runs <- 1
  function(...) {
    if (runs %% default.n == 0) cat('.')
    runs <<- runs + 1
    f(...)
  }
}
