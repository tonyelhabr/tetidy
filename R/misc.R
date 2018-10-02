
# NOTE: This is copied from the teproj package.
#' `do.call()` with package namespace
#'
#' @description Allows a package to explicitly included in `do.call()`
#' @details None.
#' @param what character. package name and function specified in explicit
#' namespace notation (i.e. `package::function`).
#' @param args list. Arguments passed to function parsed from `what`.
#' @param ... dots. Arguments passed to `do.call()`.
#' @source <https://stackoverflow.com/questions/10022436/do-call-in-combination-with>.
#' @export
do_call_with <- function(what, args, ...) {
  if (is.character(what)) {
    fn <- strsplit(what, "::")[[1]]
    what <- if (length(fn) == 1) {
      get(fn[[1]], envir = parent.frame(), mode = "function")
    } else {
      get(fn[[2]], envir = asNamespace(fn[[1]]), mode = "function")
    }
  }

  do.call(what, as.list(args), ...)
}
