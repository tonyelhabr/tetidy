

#' Summary statistics for a data.frame
#'
#' @description A customized summary of a data.frame.
#' @details The purpose of this function is equivalent to that of similar functions in
#' other packages, such as \code{skimr::skim()}.
#' This function outputs the following \code{n}, \code{mean}, \code{median}, \code{sd},
#' \code{min}, \code{max}, \code{zn1}, \code{zp1}, \code{q25}, \code{q75}, \code{q05}, \code{q95}.
#'
#' The \code{_at} versions of this function are SE evaluation. (i.e. They take characters
#' as column names.) The \code{_by} version(s) of this function allows
#' for groups to be specificed as an input, although this function will detect groups
#' and respect their integrity  (meaning that the \code{_by} version(s) are simply
#' provided as an alternative means).
#'
#' @param data data.frame.
#' @param col charactor for SE version; symbol for NSE version. Name of column in \code{data} on which to perform operations.
#' @param ... dots. Arguments passed to stats functions used internally.
#' @param na.rm logical. Argument passed to stats function used internally.
#' @param tidy logical. Whether to put output in long (i.e. tidy) format.
#' @return data.frame.
#' @export
#' @rdname summarise_stats
#' @seealso \url{https://github.com/ropenscilabs/skimr/blob/master/R/skim.R}.
#' @importFrom stats median sd quantile
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
summarise_stats_at <-
  function(data = NULL,
           col = NULL,
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
      # browser()
      cols_grp_chr <- as.character(dplyr::groups(data))
      cols_grp <- rlang::syms(cols_grp_chr)
      data <- dplyr::group_by(data, !!!cols_grp)
    } else {
      cols_grp <- NULL
    }
    col <- rlang::sym(col)
    n <- . <- NULL

    data <- dplyr::mutate(data, n = sum(!is.na(!!col)))

    ret <-
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

    ret <- dplyr::ungroup(ret)

    if (tidy) {
      stat <- NULL
      value <- NULL
      # ret <- tidyr::gather(ret, stat, value)
      if (!is.null(cols_grp)) {
        cols_gath_chr <- setdiff(names(ret), cols_grp)
        ret <-
          suppressWarnings(tidyr::gather(ret, stat, value, !!!rlang::syms(cols_gath_chr), convert = TRUE))
      } else {
        ret <- suppressWarnings(tidyr::gather(ret, stat, value, convert = TRUE))
      }
      ret <- tibble::as_tibble(ret)
    }
    ret

  }

#' @rdname summarise_stats
#' @export
summarise_stats <-
  function(data = NULL,
           col,
           ...,
           na.rm = TRUE,
           tidy = FALSE) {
    summarise_stats_at(data = data,
                       col = rlang::quo_text(rlang::enquo(col)),
                       ...,
                       na.rm = na.rm,
                       tidy = tidy)
  }

#' @rdname summarise_stats
#' @param cols_grp charactor for SE version; symbol for NSE version.
#' @export
#' @importFrom rlang !!! syms
#' @importFrom dplyr group_by ungroup arrange
summarise_stats_by_at <-
  function(data = NULL,
           col = NULL,
           cols_grp = NULL,
           ...) {
    stopifnot(is.character(col))
    stopifnot(is.character(cols_grp))
    stopifnot(length(intersect(names(data), cols_grp)) == length(cols_grp))
    cols_grp <- rlang::syms(cols_grp)
    ret <- data
    ret <- dplyr::group_by(ret, !!!cols_grp)
    ret <- summarise_stats_at(ret, col, ...)
    ret <- dplyr::ungroup(ret)
    ret
  }
