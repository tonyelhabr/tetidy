

#' Calculate how "well" two data.frames joined
#'
#' @description Calculates the join count and percentage (by number of rows missing)
#' of a data.frame that is the results of a join.
#' @details This function is mostly useful after calling \code{dplyr::full_join()}
#' (or a similar function), although it can also be useful for \code{dplyr::left_join()}
#' or \code{dplyr::right_join()}.
#' @param data data.frame. This should already by the result of a join.
#' @param x,y Either characters or bare names of columns in \code{data} on which
#' to compute metrics.
#' @return data.frame. One line summary of join metrics,
#' including the following columns:
#' \code{x}, \code{n_x}, \code{n_x_unjoined}, \code{x_in_y_pct} and their \code{y} analogues,
#' as well as \code{n_joined}.
#' @export
#' @seealso \url{https://github.com/tidyverse/tidyr/blob/master/R/gather.R}
#' provides some insight for how to make the function accept either characters
#' or bare names for the column names.
summarise_join_stats <-
  function(data = NULL,
           x = NULL,
           y = NULL) {
    stopifnot(is.data.frame(data))
    # stopifnot(!is.null(x), !is.null(y))

    x_chr <- rlang::quo_name(rlang::enexpr(x))
    y_chr <- rlang::quo_name(rlang::enexpr(y))

    in_x <- in_y <-  n_x <- n_y <- n_joined <- NULL

    idx <- match(c(x_chr, y_chr), names(data))
    ret <- dplyr::select(data, idx)
    ret <- dplyr::rename(ret, in_x = !!(names(ret)[1]), in_y = !!(names(ret)[2]))
    ret <-
      dplyr::summarise(
        ret,
        n_x = sum(!is.na(in_x)),
        n_y = sum(!is.na(in_y)),
        n_joined = sum(!is.na(in_x) & !is.na(in_y)),
        n_x_unjoined = sum(is.na(in_x) & !is.na(in_y)),
        n_y_unjoined = sum(is.na(in_y) & !is.na(in_x))
      )
    ret <-
      dplyr::mutate(
        ret,
        x_in_y_pct = 100 * n_joined / n_x,
        y_in_x_pct = 100 * n_joined / n_y
      )
    ret <- dplyr::bind_cols(tibble::tibble(x = x_chr, y = y_chr), ret)
    ret
  }


#' Join two data.frames using fuzzy string matching.
#'
#' @description Join two data.frames using fuzzy string matching.
#' @details This function is effectively a customized version of the \code{stringdist_join}
#' functions in the \code{fuzzyjoin} package.
#' Additionally, this function is primarily intended to be uzed before \code{tetidy::summarise_join_stats}
#' with \code{mode = "full"}.
#' @inheritParams fuzzyjoin::stringdist_join
#' @param cols_join character. Names of columns in \code{x} and \code{y} to join upon.
#' @param copy logical. Whether or not to keep common columns
#' @param suffix_x,suffix_y For duplicate variables in \code{x} and \code{y}, these
#' are used for differentiating the two. Alternatively, \code{suffix} can be specified directly
#' (in the same manner for \code{suffix} in \code{dplyr}'s join functions).
#' @param suffix dplyr::join
#' @return data.frame
#' @export
join_fuzzily <-
  function(x = NULL,
           y = NULL,
           mode = "inner",
           max_dist = 0,
           ...,
           cols_join = intersect(names(x), names(y)),
           copy = FALSE,
           suffix_x = "_x",
           suffix_y = "_y",
           suffix = c(suffix_x, suffix_y)) {

    f <- sprintf("fuzzyjoin::stringdist_%s_join", mode)
    suffix_x <- suffix[1]
    suffix_y <- suffix[2]
    ret <-
      do_call_with(f, list(x = x, y = y, by = cols_join, max_dist = max_dist, ...))

    . <- NULL
    ret <-
      dplyr::rename_at(ret, dplyr::vars(dplyr::ends_with(".x")), funs(gsub("\\.x", suffix_x, .)))
    ret <-
      dplyr::rename_at(ret, dplyr::vars(dplyr::ends_with(".y")), funs(gsub("\\.y", suffix_y, .)))

    if(copy) {
      cols_join_y <- rlang::quo_name(paste0(cols_join, suffix_y))
      ret <-
        dplyr::left_join(ret, dplyr::mutate(y, !!!rlang::sym(cols_join_y) := !!!rlang::sym(cols_join)), by = cols_join)
    }
    ret
  }
