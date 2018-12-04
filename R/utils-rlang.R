
# Reference: <https://github.com/tidyverse/tidyr/blob/master/R/utils.R>
# Until https://github.com/r-lib/rlang/issues/675 is fixed
ensym2 <- function(arg) {
  arg <- rlang::ensym(arg)

  expr <- rlang::eval_bare(rlang::expr(rlang::enquo(!!arg)), rlang::caller_env())
  expr <- rlang::quo_get_expr(expr)

  if (rlang::is_string(expr)) {
    rlang::sym(expr)
  } else if (rlang::is_symbol(expr)) {
    expr
  } else {
    rlang::abort("Must supply a symbol or a string as argument")
  }
}
