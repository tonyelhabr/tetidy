


#' `dplyr::distinct()` + `dplyr::arrange()` + `dplyr::pull()`
#'
#' @description Shorthand for `dplyr` functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @return vector.
#' @rdname pull_distinctly
#' @export
pull_distinctly_at <-
  function(.data, var) {

    stopifnot(is.data.frame(.data))
    stopifnot(is.character(var), length(var) == 1)

    # stopifnot(rlang::is_quosure(var), length(var) == 1, any(names(.data) == var))

    var <- rlang::sym(var)
    # stopifnot(rlang::is_quosure(var), length(var) == 1, any(names(.data) == var))

    res <- dplyr::distinct(.data, !!col)
    res <- dplyr::arrange(res, !!col)
    res <- dplyr::pull(res, !!col)
    res
  }

#' @rdname pull_distinctly
#' @export
pull_distinctly <-
  function(.data, var) {
    pull_distinctly_at(.data = .data, var = rlang::quo_text(rlang::enquo(var)))
  }

#' `dplyr::arrange()` + `dplyr::distinct()`
#'
#' @description Shorthand for `dplyr` functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @param ... dots. Bare names of columns in `.data` on which to perform operations.
#' @rdname arrange_distinctly
#' @return A [tibble][tibble::tibble-package].
#' @export
arrange_distinctly_at <-
  function(.data, ...) {

    stopifnot(!is.null(.data), is.data.frame(.data))
    # dots <- alist(...)
    # stopifnot(length(dots) >= 1)

    cols <- rlang::syms(...)
    # if (length(cols) == 0)  {
    #   cols <- tidyselect::everything(.data)
    # }
    # stopifnot(rlang::is_quosures(cols), length(cols) >= 1)

    res <- .data
    res <- dplyr::distinct(res, !!!cols)
    res <- dplyr::arrange(res, !!!cols)
    res
  }

#' @rdname arrange_distinctly
#' @export
arrange_distinctly <-
  function(.data, ...) {

    stopifnot(!is.null(.data), is.data.frame(.data))
    # dots <- alist(...)
    # stopifnot(length(dots) >= 1)
    cols <- rlang::enquos(...)
    # if (length(cols) == 0)  {
    #   cols <- tidyselect::everything(.data)
    # }
    stopifnot(rlang::is_quosures(cols), length(cols) >= 1)
    res <- .data
    res <- dplyr::distinct(res, !!!cols)
    res <- dplyr::arrange(res, !!!cols)
    res
  }

# TODO: Figure out a way to capture bare dots, then convert them to separate
# characters to pass to the SE version.
# #' @rdname arrange_distinctly
# #' @export
# arrange_distinctly <-
#   function(.data, ...) {
#     dots <- rlang::quo_text(rlang::enquos(...))
#     dots2a <- rlang::quo_text(rlang::enquos(...))
#     dots2b <- rlang::quo_name(rlang::enquos(...))
#     dots3 <- rlang::expr_name(rlang::exprs(...))
#     browser()
#     arrange_distinctly_at(.data = .data, dots2b)
#   }

#' `dplyr::row_number(dplyr::desc(.))`
#'
#' @description Shorthand for `dplyr` functions called consecutively.
#' @details None.
#' @param x symbol. Name of column on which to perform operations. Should be numeric.
#' @export
rank_unique <-
  function(x) {
    stopifnot(is.numeric(x))
    dplyr::row_number(dplyr::desc(x))
  }

#' `dplyr::mutate(... dplyr::row_number(dplyr::desc(.)))`
#'
#' @description Shorthand for `dplyr` functions called consecutively.
#' @details None.
#' @inheritParams summarise_stats
#' @param var_out charactor for SE version; symbol for NSE version. Name of column in returned data.frame
#' corresponding to rank.
#' @param .pretty logical. Whether to re-arrange columns such that `var_out` is the first column.
#' @param ... dots. For NSE version, passed to SE version.
#' @return A [tibble][tibble::tibble-package].
#' @rdname rank_arrange
#' @export
rank_arrange_at <-
  function(.data,
           var,
           var_out = "rnk",
           pretty = TRUE) {

    stopifnot(is.data.frame(.data))
    stopifnot(is.character(var), length(var) == 1)
    stopifnot(is.character(var_out), length(var_out) == 1)
    stopifnot(is.logical(pretty))
    var <- rlang::sym(var)
    var_out <- rlang::sym(var_out)
    # res <- dplyr::mutate(.data, !!var_out := dplyr::row_number(dplyr::desc(!!col)))
    res <- dplyr::mutate(.data, !!var_out := rank_unique(!!var))
    res <- dplyr::arrange(res, !!var_out)

    if(pretty) {
      res <- dplyr::select(res, !!var_out, dplyr::everything())
    }
    res
  }

#' @rdname rank_arrange
#' @export
rank_arrange <-
  function(.data, var, ...) {
    rank_arrange_at(.data = .data,
                    var = rlang::quo_text(rlang::enquo(var)),
                    ...)
  }

#' `dplyr::count()` + `dplyr::arrange()`
#'
#' @description Shorthand for `dplyr` functions called consecutively.
#' @details Note that arrangeing is performed on the column used to compute
#' the value for `dplyr::count()`, which is differrent than simply
#' calling `dplyr::count(..., sort = TRUE)` (which sorts by the output column `n`).
#' @inheritParams arrange_distinctly
#' @return A [tibble][tibble::tibble-package].
#' @rdname count_arrange
#' @export
count_arrange_at <-
  function(.data, ...) {

    stopifnot(is.data.frame(.data))
    cols <- rlang::syms(...)

    res <- dplyr::count(.data, !!!cols)
    res <- dplyr::arrange(res, !!!cols)
    res
  }

#' @rdname rank_arrange
#' @export
count_arrange <-
  function(.data, ...) {

    stopifnot(is.data.frame(.data))
    cols <- rlang::enquos(...)

    res <- dplyr::count(.data, !!!cols)
    res <- dplyr::arrange(res, !!!cols)
    res
  }
