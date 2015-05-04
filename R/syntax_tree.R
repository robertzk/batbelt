#' Returns the syntax tree for an expression.
#'
#' @param expr. The expression to evaluate.
syntax_tree <- function(expr) {
  lapply(expr, function(x) {
    if (is.call(x)) syntax_tree(x) else x
  })
}
