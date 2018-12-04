

#' Summary statistics for a data.frame
#'
#' @description A customized summary of a data.frame.
#' @details The purpose of this function is equivalent to that of similar functions in
#' other packages, such as `skimr::skim()`.
#' This function outputs the following `n`, `mean`, `median`, `sd`,
#' `min`, `max`, `zn1`, `zp1`, `q25`, `q75`, `q05`, `q95`.
#'
#' The `_at` versions of this function are SE evaluation. (i.e. They take characters
#' as column names.) ~~The `_by` version(s) of this function allows
#' for groups to be specificed as an input, although this function will detect groups
#' and respect their integrity  (meaning that the `_by` version(s) are simply
#' provided as an alternative means).~~
#'
#' @param data data.frame.
#' @param col charactor for SE version; symbol for NSE version.
#' Name of column in `data` on which to perform operations.
#' @param ... dots. Arguments passed to stats functions used internally.
#' @param na.rm logical. Argument passed to stats function used internally.
#' @param tidy logical. Whether to put output in long (i.e. .tidy) format.
#' @return A [tibble][tibble::tibble-package].
#' @rdname summarise_stats
#' @seealso <https://github.com/ropenscilabs/skimr/blob/master/R/skim.R>.
#' @importFrom stats median sd quantile
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
summarise_stats_impl <-
  function(data,
           col,
           ...,
           na.rm = TRUE,
           tidy = FALSE) {

    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1)
    stopifnot(length(intersect(names(data), col)) == 1)
    stopifnot(is.logical(na.rm))
    stopifnot(is.logical(tidy))
    is_grouped <- ifelse(is.null(dplyr::groups(data)), FALSE, TRUE)
    if (is_grouped) {
      cols_grp_chr <- as.character(dplyr::groups(data))
      cols_grp <- rlang::syms(cols_grp_chr)
      data <- dplyr::group_by(data, !!!cols_grp)
    } else {
      cols_grp <- NULL
    }
    col <- rlang::sym(col)
    n <- . <- NULL

    data <- dplyr::mutate(data, n = sum(!is.na(!!col)))

    res <-
      dplyr::summarise_at(
        data,
        dplyr::vars(!!col),
        dplyr::funs(
          n = dplyr::first(n),
          mean = mean(., na.rm = na.rm, ...),
          median = stats::median(., ...),
          sd = stats::sd(., na.rm = na.rm, ...),
          min = min(., na.rm = na.rm, ...),
          max = max(., na.rm = na.rm, ...),
          zn1 = mean(., na.rm = na.rm, ...) - stats::sd(., ...),
          zp1 = mean(., na.rm = na.rm, ...) + stats::sd(., na.rm = na.rm, ...),
          q25 = stats::quantile(., 0.25, na.rm = na.rm, ...),
          q75 = stats::quantile(., 0.75, na.rm = na.rm, ...),
          q05 = stats::quantile(., 0.05, na.rm = na.rm, ...),
          q95 = stats::quantile(., 0.95, na.rm = na.rm, ...)
        )
      )

    res <- dplyr::ungroup(res)

    if (tidy) {
      stat <- NULL
      value <- NULL
      # res <- tidyr::gather(res, stat, value)
      if (!is.null(cols_grp)) {
        cols_gath_chr <- setdiff(names(res), cols_grp)
        res <-
          suppressWarnings(tidyr::gather(res, stat, value, !!!rlang::syms(cols_gath_chr), convert = TRUE))
      } else {
        res <- suppressWarnings(tidyr::gather(res, stat, value, convert = TRUE))
      }
      res <- tibble::as_tibble(res)
    }
    res

  }

#' @rdname summarise_stats
#' @export
summarise_stats_at <-
  function(.data, .col, ...) {
    summarise_stats_impl(data = .data, col = .col, ...)
  }

#' @rdname summarise_stats
#' @export
summarise_stats <-
  function(.data, .col, ...) {
    summarise_stats_at(.data = .data, .col = rlang::quo_text(rlang::enquo(.col)), ...)
  }
