#' Opens a GitHub page.
#' @export
open_github <- function(...) browseURL(get_github(...))

#' Finds the URL for a GitHub page.
#' @param package character. The name of the R package (e.g., 'batbelt').
#'    Can also be 'author/package' (e.g., 'peterhurford/batbelt').
#' @param fun character. The name of the function to open.  Leave NULL to just get the URL for the package overall.
#' @param author character. The GitHub username of the package.  Defaults to finding packages via cran.
#' @param branch character. The branch to look for.
#' @param cap_r logical. TRUE if the file ends with a capital R, FALSE otherwise.
#' @export
get_github <- function(package, fun = NULL, author = 'cran', branch = 'master', cap_r = TRUE) {
  for(char in c('/', ':::', '::')) {
    if (grepl(char, package)) {
      author <- strsplit(package, char)[[1]][[1]]
      package <- strsplit(package, char)[[1]][[2]]
    }
  }
  url <- paste('https://github.com', author, package, sep = '/')
  if (!is.null(fun)) {
    url <- paste(
      url,
      if (identical(branch, 'master')) 'blob' else 'tree',
      branch,
      'R',
      paste0(
        fun,
        '.',
        if (isTRUE(cap_r)) 'R' else 'r'
      ),
      sep = '/'
    )
  }
  url
}


#' Searches for a function in GitHub using GitHub's search.
#' @param package character. The name of the R package (e.g., 'batbelt').
#'    Can also be 'author/package' (e.g., 'peterhurford/batbelt').
#' @param fun character. The name of the function to open.  Leave NULL to just get the URL for the package overall.
#' @param author character. The GitHub username of the package.  Defaults to finding packages via cran.
#' @export
github_fun_search <- function(package, fun, author = 'cran') {
  url <- paste(
    get_github(package, author = author),
    'search?&q=%22',
    fun,
    '+%3C-+function%22',
    sep = '/'
  )
}
