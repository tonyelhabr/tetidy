

#' Calculate how "well" two `data.frame`s joined
#'
#' @description Calculates the join count and percentage (by number of rows missing)
#' of a d`ata.frame` that is the results of a join.
#' @details This function is mostly useful after calling `dplyr::full_join()`
#' (or a similar function), although it can also be useful for `dplyr::left_join()`
#' #' or `dplyr::right_join()`.
#'
#' The logic for
#' handling `x` and `y` as strings/`quo`s was borrowed from a previous
#' version of `tidyr::gather()`.
#' @param data data.frame. This should already by the result of a join.
#' @param x,y Either characters or bare names of columns in `data` on which
#' to compute metrics.
#' @return A [tibble][tibble::tibble-package]. One line summary of join metrics,
#' including the following columns:
#' `x`, `n_x`, `n_x_unjoined`, `x_in_y_pct` and their `y` analogues,
#' as well as `n_joined`.
#' @export
#' @seealso <https://github.com/tidyverse/tidyr/blob/master/R/gather.R>
#' provides some insight for how to make the function accept either characters
#' or bare names for the column names.
summarise_join_stats <-
  function(data, x, y) {
    stopifnot(is.data.frame(data))

    x_nm <- rlang::quo_name(rlang::enexpr(x))
    y_nm <- rlang::quo_name(rlang::enexpr(y))

    in_x <- in_y <-  n_x <- n_y <- n_joined <- NULL

    idx <- match(c(x_nm, y_nm), names(data))
    res <- dplyr::select(data, idx)
    res <- dplyr::rename(res, in_x = !!(names(res)[1]), in_y = !!(names(res)[2]))
    res <-
      dplyr::summarise(
        res,
        n_x = sum(!is.na(in_x)),
        n_y = sum(!is.na(in_y)),
        n_joined = sum(!is.na(in_x) & !is.na(in_y)),
        n_x_unjoined = sum(is.na(in_x) & !is.na(in_y)),
        n_y_unjoined = sum(is.na(in_y) & !is.na(in_x))
      )
    res <-
      dplyr::mutate(
        res,
        x_in_y_pct = 100 * n_joined / n_x,
        y_in_x_pct = 100 * n_joined / n_y
      )
    res <- dplyr::bind_cols(tibble::tibble(x = x_nm, y = y_nm), res)
    res
  }


#' Join two data.frames using fuzzy string matching.
#'
#' @description Join two `data.frame`s using fuzzy string matching.
#' @details This function is effectively a customized version of the `stringdist_join`
#' functions in the `{fuzzyjoin}` package.
#' Additionally, this function is primarily intended to be uzed before `tetidy::summarise_join_stats()`
#' with `mode = "full"`.
#' @inheritParams fuzzyjoin::stringdist_join
#' @inheritParams dplyr::inner_join
#' # @param by character. Names of columns in `x` and `y` to join upon.
#' # @param copy logical. Whether or not to keep common columns
#' # @param suffix character (vector). For duplicate variables in `x` and `y`, this vector of
#' # length two is used for differentiating the two.
#' # (in the same manner for `suffix` in `dplyr`'s join functions).
#' @return data.frame
#' @export
#' @seealso [fuzzyjoin::stringdist_join()] [dplyr::inner_join()]
join_fuzzily <-
  function(x,
           y,
           mode = "inner",
           max_dist = 0,
           ...,
           by = intersect(names(x), names(y)),
           copy = FALSE,
           suffix_x = "_x",
           suffix_y = "_y",
           suffix = c(suffix_x, suffix_y)) {

    pkg <- "fuzzyjoin"
    if(!requireNamespace(pkg)) {
      msg <- sprintf("Please install the `{%s}` package.", pkg)
      stop(msg)
    }
    f <- sprintf("fuzzyjoin::stringdist_%s_join", mode)
    suffix_x <- suffix[1]
    suffix_y <- suffix[2]
    res <- do_call_with(f, list(x = x, y = y, by = by, max_dist = max_dist, ...))

    . <- NULL
    res <-
      dplyr::rename_at(res, dplyr::vars(dplyr::ends_with(".x")), funs(gsub("\\.x", suffix_x, .)))
    res <-
      dplyr::rename_at(res, dplyr::vars(dplyr::ends_with(".y")), funs(gsub("\\.y", suffix_y, .)))

    if(copy) {
      by_y <- rlang::quo_name(paste0(by, suffix_y))
      res <-
        dplyr::left_join(res, dplyr::mutate(y, !!!rlang::sym(by_y) := !!!rlang::sym(by)), by = by)
    }
    res
  }
