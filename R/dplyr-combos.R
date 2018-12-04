
#' `dplyr::pull()` + `unique()` + `sort()`
#'
#' Shorthand for `{dplyr}` functions called consecutively.
#'
#' @details This may be slower than `dplyr:;distinct()` +
#'   `dplyr::arrange()` + `dplyr::pull()` (which was the original implementation).
#' @inheritParams summarise_stats
#' @inheritParams dplyr::pull
#' @inheritParams base::unique
#' @inheritParams base::sort
#' @return A vector (of the same type as `var`).
#' @rdname pull_distinctly
#' @export
#' @family dplyr-combos
pull_distinctly <-
  function(.data, var = -1, ..., decreasing = FALSE) {
    data <- .validate_coerce_data(data)

    var <- tidyselect::vars_pull(names(.data), !!rlang::enquo(var))
    sort(unique(.data[[var]]), decreasing = decreasing, ...)
  }


#' `dplyr::distinct()` + `dplyr::arrange()`
#'
#' Shorthand for `{dplyr}` functions called consecutively.
#'
#' @section Borrowing from the `{tidyverse}`:
#'   The logic for handling `...` is borrowed from a reivew of the internals for
#'   `tidyr::unite()` and `tidyr::gather()`.
#'
#' @inheritParams summarise_stats
#' @inheritParams dplyr::distinct
#' @inheritParams dplyr::arrange
#' @rdname arrange_distinctly
#' @return A [tibble][tibble::tibble-package].
#' @export
#' @family dplyr-combos
arrange_distinctly <-
  function(data, ..., .keep_all = FALSE, .by_group = FALSE) {
    data <- .validate_coerce_data(data)

    cols <- rlang::enquos(...)
    # .validate_cols(data, cols)

    res <- dplyr::distinct(data, !!!cols, .keep_all = .keep_all)
    res <- dplyr::arrange(res, !!!cols, .by_group = .by_group)
    res
  }

#' `dplyr::count()` + `dplyr::arrange()`
#'
#' @description Shorthand for `{dplyr}` functions called consecutively.
#' @details Note that arranging is performed on the column used to compute
#'   the value for `dplyr::count()`, which is different than simply
#'   calling `dplyr::count(x, ..., sort = TRUE)` (which sorts by the output column `n`).
#'   For this reason, no `sort` option is provided (to prevent the user from
#'   mistakenly using this function in such a manner).
#'
#' @inheritSection arrange_distinctly Borrowing from the `{tidyverse}`
#' @inheritParams arrange_distinctly
#' @return A [tibble][tibble::tibble-package].
#' @rdname count_arrange
#' @export
#' @family dplyr-combos
count_arrange <-
  function(data, ...) {

    data <- .validate_coerce_data(data)

    # Reference: [tidyr::unite](https://github.com/tidyverse/tidyr/blob/master/R/unite.R)
    if (rlang::dots_n(...) == 0) {
      return(data)
    } else {
      cols <- tidyselect::vars_select(colnames(data), ...)
    }

    res <- dplyr::count(data, !!!cols)
    res <- dplyr::arrange(res, !!!cols)
    res
  }
