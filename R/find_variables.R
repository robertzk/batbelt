#' Finds variables within an expression.
#'
#' @param expr. The expression to evaluate.
find_variables <- function(expr) {
  o <- list()
  i <- 1
  for (s_expr in unlist(syntax_tree(expr))) {
    if (!exists(deparse(s_expr)) && !is.numeric(s_expr)) {
      o[[i]] <- s_expr
      i <- i + 1
    }
  }
  o
}
