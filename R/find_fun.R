#' Finds the name of the package that a function is from.
#' 
#' Uses Hadley's "where" from pryr, but gets just the name of the package and
#' can handle passing in a function instead of just a character string.
#' Also can handle a string that includes the function name (e.g., 'batbelt::find_fun' or 'batbelt/find_fun')
#'
#' @param fun function or character. The function to find the name of the package for.
#' @export
find_fun <- function(fun) {
  if (is.character(fun)) eval(parse(text = fun), env = globalenv())  # Make sure function exists.
  else fun <- deparse(substitute(fun))

  for(char in c('/', ':::', '::')) {
    if (grepl(char, fun)) return(strsplit(fun, char)[[1]][[1]])
  }

  fun_where <- fun %>% pryr::where()
  if (identical(fun_where, pryr::where('mean'))) { return('base') }
  if (identical(fun_where, pryr::where('find_fun'))) { return('batbelt') }
  fun_where %>% attr(., 'name') %>% strsplit(., ':') %>% .[[1]] %>% .[[2]]
}
