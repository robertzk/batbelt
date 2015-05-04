#' Takes an expression and turns it into a function by inferring parameters.
#' @param expr expression. The expression to turn into a function.
#' @export
function_from_expr <- function(expr) {
  inputs <- unlist(lapply(find_variables(substitute(expr)), as.character))
  fun <- eval(parse(text = paste0(
   'function(',
    paste0(find_variables(substitute(fun)), collapse = ', '),
    ') { ',
    deparse(substitute(fun)),
    ' }'
  )))
}
