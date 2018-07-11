
# NOTE: This is copied from the teproj package.
#' \code{do.call()} with package namespace
#'
#' @description Allows a package to explicitly included in \code{do.call()}
#' @details None.
#' @param what character. package name and function specified in explicit
#' namespace notation (i.e. \code{package::function}).
#' @param args list. Arguments passed to function parsed from \code{what}.
#' @param ... dots. Arguments passed to \code{do.call()}.
#' @source \url{https://stackoverflow.com/questions/10022436/do-call-in-combination-with}.
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
