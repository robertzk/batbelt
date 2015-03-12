#' Opens a function in GitHub or dies trying.
#' @param fun function or character. The function to open in GitHub.
#' @param verbose logical. Whether or not to make messages.
#' @param dev logical. If true, CRAN will not be searched first.  If false, it will.
#' @export
open_fun <- function(fun, verbose = TRUE, dev = FALSE) {
  if (!is.character(fun)) fun <- deparse(substitute(fun))
  
  package <- paris::find_fun(fun)

  for(char in c('/', ':::', '::')) {
    if (grepl(char, fun)) fun <- strsplit(fun, char)[[1]][[2]]
  }

  if (package %in% c('base', 'utils', 'stats', 'graphics')) {
    stop('Cannot open in GitHub because ', sQuote(fun), ' is in R base libraries.')
  }

  authors <- c('cran', 'hadley', 'robertzk', 'peterhurford')
  if (isTRUE(dev)) { authors <- c(authors[-1], 'cran') }      # Put cran at end of search.

  attempts <- list()
  for (author in authors) {
    ll <- list(package, fun, author, cap_r = FALSE)
    attempts[[length(attempts) + 1]] <- ll
    ll$cap_r <- TRUE
    attempts[[length(attempts) + 1]] <- ll
  }

  for (i in seq_along(attempts)) {
    if (isTRUE(verbose)) cat(capitalize(as.ordinal(i)), 'attempt...\n')
    url <- do.call(get_github, attempts[[i]])
    try <- url %>% RCurl::getURL()
    if (!is.error(try)) {
      browseURL(url)
      break(2)
    }
  }

  if (is.error(try)) {
    if (isTRUE(verbose)) {
      cat('Could not find the function on GitHub.\n',
        'Opening repo search instead.\n')
    }
    amount_prior_attempts <- length(attempts)
    attempts <- list()
    for (author in authors) {
      ll <- list(package, fun, author)
      attempts[[length(attempts) + 1]] <- ll
    }
    for (i in seq_along(attempts)) {
      j <- amount_prior_attempts + i
      if (isTRUE(verbose)) cat(capitalize(as.ordinal(j)), 'attempt...\n')
      url <- do.call(github_fun_search, attempts[[i]])
      try <- url %>% RCurl::getURL()
      if (!is.error(try)) {
        browseURL(url)
        break(2)
      }
    }
  }
}

is.error <- function(curl_output) {
  identical(curl_output, "{\"error\":\"Not Found\"}")
}
