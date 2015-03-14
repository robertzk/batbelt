#' Opens a function in GitHub or dies trying.
#' @param fun function or character. The function to open in GitHub.
#' @param verbose logical. Whether or not to make messages.
#' @param dev logical. If true, CRAN will not be searched first.  If false, it will.
#' @param author character. A vector of GitHub usernames to search through.  Will be modified to include cran.
#' @export
open_fun <- function(fun, verbose = TRUE, dev = FALSE, author = NULL) {
  if (is.null(author)) {
    if (is.null(getOption('batbelt.openfun.authors'))) {
      author <- c('hadley', 'robertzk', 'peterhurford')
    } else {
      author <- getOption('batbelt.openfun.authors')
    }
  }

  if (!is.character(fun)) fun <- deparse(substitute(fun))
  
  package <- paris::find_fun(fun)

  for(char in c('/', ':::', '::')) {
    if (grepl(char, fun)) fun <- strsplit(fun, char)[[1]][[2]]
  }

  if (package %in% c('base', 'utils', 'stats', 'graphics')) {
    stop('Cannot open in GitHub because ', sQuote(fun), ' is in R base libraries.')
  }

  if (!('cran' %in% author)) { author <- c('cran', author) } # Always include cran.
  if (isTRUE(dev)) { author <- c(author[-1], 'cran') }        # Put cran at end of search in dev mode.

  attempts <- list()
  for (subauthor in author) {
    ll <- list(package, fun, subauthor, cap_r = FALSE)
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
    for (subauthor in author) {
      ll <- list(package, fun, subauthor)
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
  if (is.error(try)) stop('Failed to find the specified repo.')
}

is.error <- function(curl_output) {
  identical(curl_output, '') || identical(curl_output, "{\"error\":\"Not Found\"}")
}
