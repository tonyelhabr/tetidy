

#' Summary statistics for a `data.frame`
#'
#' @description A customized summary of a data.frame.
#' @details The purpose of this function is equivalent to that of similar functions in
#'   other packages, such as `skimr::skim()`.
#'   This function outputs the following `n`, `mean`, `median`, `sd`,
#'   `min`, `max`, `zn1`, `zp1`, `q25`, `q75`, `q05`, `q95`.
#'
#'   The `_at` versions of this function are SE evaluation. (i.e. They take characters
#'   as column names.) ~~The `_by` version(s) of this function allows
#'   for groups to be specificed as an input, although this function will detect groups
#'   and respect their integrity  (meaning that the `_by` version(s) are simply
#'   provided as an alternative means).~~
#'
#' @param data A `data.frame`.
#' @param col A character for the SE version; a symbol for NSE version.
#'   The name of column in `data` on which to perform operations.
#' @param na.rm A logical value passed to `{base}`\`{stats}` functions used internally.
#' @param ... Additional arguments passed to internal functions.
#' @param tidy A logical value indicating whether to put output in long (i.e. "tidy") format.
#' @param key,value Parameters corresponding with those of the same name for `tidyr::gather()`.
#'   For this function, these should be characters.
#' @return A [tibble][tibble::tibble-package].
#' @rdname summarise_stats
#' @seealso [skimr::skim()], [tidyr::gather()]
summarise_stats <-
  function(data,
           col,
           na.rm = TRUE,
           ...,
           tidy = FALSE,
           key = "stat",
           value = "value") {

    data <- .validate_coerce_data(data)

    col_chr <- rlang::as_string(ensym2(col))
    .validate_col(data, col)

    .validate_lgl(na.rm)
    .validate_lgl(tidy)

    is_grouped <- ifelse(is.null(dplyr::groups(data)), FALSE, TRUE)
    if (is_grouped) {
      cols_grp_chr <- as.character(dplyr::groups(data))
      cols_grp <- rlang::syms(cols_grp_chr)
      data <- dplyr::group_by(data, !!!cols_grp)
    } else {
      cols_grp <- NULL
    }
    n <- . <- NULL

    data <- dplyr::mutate(data, n = sum(!is.na(!!col)))

    res <-
      dplyr::summarise_at(
        data,
        dplyr::vars(!!col),
        dplyr::funs(
          n = dplyr::first(n),
          mean = base::mean(., na.rm = na.rm, ...),
          median = stats::median(., ...),
          sd = stats::sd(., na.rm = na.rm, ...),
          min = base::min(., na.rm = na.rm, ...),
          max = base::max(., na.rm = na.rm, ...),
          zn1 = base::mean(., na.rm = na.rm, ...) - stats::sd(., ...),
          zp1 = base::mean(., na.rm = na.rm, ...) + stats::sd(., na.rm = na.rm, ...),
          q25 = stats::quantile(., 0.25, na.rm = na.rm, ...),
          q75 = stats::quantile(., 0.75, na.rm = na.rm, ...),
          q05 = stats::quantile(., 0.05, na.rm = na.rm, ...),
          q95 = stats::quantile(., 0.95, na.rm = na.rm, ...)
        )
      )

    res <- dplyr::ungroup(res)

    if (tidy) {
      if (!is.null(cols_grp)) {
        cols_gath <- setdiff(names(res), cols_grp)
        cols_gath <- rlang::syms(cols_gath_chr)
        res <-
          suppressWarnings(tidyr::gather(res, key = key, value = value, !!!cols_gath, convert = TRUE))
      } else {
        res <- suppressWarnings(tidyr::gather(res, stat, value, convert = TRUE))
      }
      res <- tibble::as_tibble(res)
    }
    res

  }
