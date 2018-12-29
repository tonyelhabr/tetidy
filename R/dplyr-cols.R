
# Source: personal `{sportspredict}` project.
# reorder_one_of_at <-
#   function(data, cols_order) {
#     .validate_data(data)
#     .validate_cols(data, cols_order)
#
#     cols <- names(data)
#     cols_in <- intersect(cols_order, cols)
#     cols_nin <- setdiff(cols, cols_order)
#     # length(cols); length(cols_in); length(cols_nin); length(cols_order)
#     # setdiff(cols, c(cols_in, cols_nin))
#     cols_fct <- factor(cols, levels = c(cols_in, cols_nin))
#     dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
#   }
#
# select_one_of_at <-
#   function(data, cols, cols_order = cols) {
#     .validate_data(data)
#     .validate_cols(data, cols)
#
#     if(cols_order != cols) {
#       .validate_cols(data, cols_order)
#     }
#     cols_fct <- factor(cols, levels = cols_order)
#     dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
#   }

#' `dplyr::select(data, tidyselect::one_of(cols), tidyselect::everything())`
#'
#' Shorthand for `{dplyr}` functions called consecutively.
#' @details There is probably a better way to implement this function internally.
#'   Should review `tidyselect::one_of()` and `tidyr::separate()`'s `into` argument.
#' @inheritParams summarise_stats
#' @param cols A character vector corresponding to the columns in `data` to be
#'   selected by `dplyr::one_of()`.
#' @param drop If `FALSE`, does NOT drop all columns not specified by `cols`.
#'   (i.e. This is like calling `tidyselect::everything()`).
#' @return A [tibble][tibble::tibble-package].
#' @rdname select_one_of
#' @export
#' @family dplyr-cols
select_one_of <-
  function(data, cols, drop = FALSE) {

    data <- .validate_coerce_data(data)

    .validate_cols(data, cols)
    .validate_lgl(drop)

    .cols <- names(data)

    if(!drop) {
      cols_in <- intersect(cols, .cols)
      cols_nin <- setdiff(.cols, cols)
      # length(cols); length(cols_in); length(cols_nin); length(cols_order)
      # setdiff(cols, c(cols_in, cols_nin))
      cols_fct <- factor(.cols, levels = c(cols_in, cols_nin))
    } else {
      cols_fct <- factor(cols, levels = cols)
    }
    dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
  }

#' `dplyr::row_number(dplyr::desc(.))`
#'
#' Shorthand for `{dplyr}` functions called consecutively.
#'
#' @param x The name of column on which to perform operations. Should be numeric.
#' @return A vector (of the same type as `x`).
#' @export
rank_unique <-
  function(x) {
    # stopifnot(is.numeric(x))
    dplyr::row_number(dplyr::desc(x))
  }

#' `dplyr::mutate(... dplyr::row_number(dplyr::desc(.)))`
#'
#' Shorthand for `{dplyr}` functions called consecutively.
#'
#' @section Borrowing from the `{tidyverse}`:
#'   Documentation for `col` and `col_out` is modified
#'   `tidyr::gather()`'s documentation of `key` and `value`.
#'
#' @inheritParams summarise_stats
#' @param col_out Name of the new column to create, as a string or symbol.
#' @param arrange If `TRUE`, will arrange `data` according to `col_out`
#' (calling `dplyr::arrange()`). Does not currently support grouped arranging
#' (i.e. via `.by_group`).
#' @param pretty If `TRUE`, will re-order columns such that `col_out` is the first column.
#' @return A [tibble][tibble::tibble-package].
#' @rdname add_rnk_col
#' @export
#' @family dplyr-cols
add_rnk_col <-
  function(data, col, col_out = "rnk", arrange = TRUE, pretty = TRUE) {

    data <- .validate_coerce_data(data)

    col <- ensym2(col)
    .validate_col(data, rlang::as_string(col))

    col_out <- ensym2(col_out)
    .validate_col_out(data, rlang::as_string(col_out))

    .validate_lgl(arrange)
    .validate_lgl(pretty)

    res <- dplyr::mutate(data, !!col_out := rank_unique(!!col))

    if(arrange) {
      res <- dplyr::arrange(res, !!col_out)
    }

    if(pretty) {
      res <- dplyr::select(res, !!col_out, dplyr::everything())
    }
    res
  }
