
#' \code{dplyr::distinct()} + \code{dplyr::arrange()} + \code{dplyr::pull()}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @return vector.
#' @rdname pull_distinctly
#' @export
pull_distinctly_at <-
  function(data = NULL, col = NULL) {

    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1)

    # stopifnot(rlang::is_quosure(col), length(col) == 1, any(names(data) == col))

    col <- rlang::sym(col)
    # stopifnot(rlang::is_quosure(col), length(col) == 1, any(names(data) == col))

    ret <- dplyr::distinct(data, !!col)
    ret <- dplyr::arrange(ret, !!col)
    ret <- dplyr::pull(ret, !!col)
    ret
  }

#' @rdname pull_distinctly
#' @export
pull_distinctly <-
  function(data = NULL, col = NULL) {
    pull_distinctly_at(data = data, col = rlang::quo_text(rlang::enquo(col)))
  }

#' \code{dplyr::arrange()} + \code{dplyr::distinct()}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @param ... dots. Bare names of columns in \code{data} on which to perform operations.
#' @rdname arrange_distinctly
#' @return data.frame
#' @export
arrange_distinctly_at <-
  function(data = NULL, ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    # dots <- alist(...)
    # stopifnot(length(dots) >= 1)

    cols <- rlang::syms(...)
    # if (length(cols) == 0)  {
    #   cols <- tidyselect::everything(data)
    # }
    # stopifnot(rlang::is_quosures(cols), length(cols) >= 1)

    ret <- data
    ret <- dplyr::distinct(ret, !!!cols)
    ret <- dplyr::arrange(ret, !!!cols)
    ret
  }

#' @rdname arrange_distinctly
#' @export
arrange_distinctly <-
  function(data = NULL, ...) {

    stopifnot(!is.null(data), is.data.frame(data))
    # dots <- alist(...)
    # stopifnot(length(dots) >= 1)
    cols <- rlang::enquos(...)
    # if (length(cols) == 0)  {
    #   cols <- tidyselect::everything(data)
    # }
    stopifnot(rlang::is_quosures(cols), length(cols) >= 1)
    ret <- data
    ret <- dplyr::distinct(ret, !!!cols)
    ret <- dplyr::arrange(ret, !!!cols)
    ret
  }

# TODO: Figure out a way to capture bare dots, then convert them to separate
# characters to pass to the SE version.
# #' @rdname arrange_distinctly
# #' @export
# arrange_distinctly <-
#   function(data = NULL, ...) {
#     dots <- rlang::quo_text(rlang::enquos(...))
#     dots2a <- rlang::quo_text(rlang::enquos(...))
#     dots2b <- rlang::quo_name(rlang::enquos(...))
#     dots3 <- rlang::expr_name(rlang::exprs(...))
#     browser()
#     arrange_distinctly_at(data = data, dots2b)
#   }

#' \code{dplyr::row_number(dplyr::desc(.))}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details None.
#' @param x symbol. Name of column on which to perform operations. Should be numeric.
#' @export
rank_unique <-
  function(x = NULL) {
    stopifnot(is.numeric(x))
    dplyr::row_number(dplyr::desc(x))
  }

#' \code{dplyr::mutate(... dplyr::row_number(dplyr::desc(.)))}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @param col_out charactor for SE version; symbol for NSE version. Name of column in returned data.frame
#' corresponding to rank.
#' @param pretty logical. Whether to re-arrange columns such that \code{col_out} is the first column.
#' @param ... dots. For NSE version, passed to SE version.
#' @return data.frame.
#' @rdname rank_arrange
#' @export
rank_arrange_at <-
  function(data = NULL,
           col = NULL,
           col_out = "rnk",
           pretty = TRUE) {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1)
    stopifnot(is.character(col_out), length(col_out) == 1)
    stopifnot(is.logical(pretty))
    col <- rlang::sym(col)
    col_out <- rlang::sym(col_out)
    # ret <- dplyr::mutate(data, !!col_out := dplyr::row_number(dplyr::desc(!!col)))
    ret <- dplyr::mutate(data, !!col_out := rank_unique(!!col))
    ret <- dplyr::arrange(ret, !!col_out)

    if(pretty) {
      ret <- dplyr::select(ret, !!col_out, dplyr::everything())
    }
    ret
  }

#' @rdname rank_arrange
#' @export
rank_arrange <-
  function(data = NULL, col = NULL, ...) {
    rank_arrange_at(data = data,
                    col = rlang::quo_text(rlang::enquo(col)),
                    ...)
  }

#' \code{dplyr::count()} + \code{dplyr::arrange()}
#'
#' @description Shorthand for \code{dplyr} functions called consecutively.
#' @details Note that arrangeing is performed on the column used to compute
#' the value for \code{dplyr::count()}, which is differrent than simply
#' calling \code{dplyr::count(..., sort = TRUE)} (which sorts by the output column \code{n}).
#' @inheritParams arrange_distinctly
#' @return data.frame.
#' @rdname count_arrange
#' @export
count_arrange_at <-
  function(data = NULL, ...) {

    stopifnot(is.data.frame(data))
    cols <- rlang::syms(...)

    ret <- dplyr::count(data, !!!cols)
    ret <- dplyr::arrange(ret, !!!cols)
    ret
  }

#' @rdname rank_arrange
#' @export
count_arrange <-
  function(data = NULL, ...) {

    stopifnot(is.data.frame(data))
    cols <- rlang::enquos(...)

    ret <- dplyr::count(data, !!!cols)
    ret <- dplyr::arrange(ret, !!!cols)
    ret
  }
