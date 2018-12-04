
# Source: personal `{sportspredict}` project.

get_cols_sep <-
  function(data, col, sep) {
    # Don't validate arguments since this should only be called from `separate_cols_max()`.
    # col <- rlang::as_string(ensym2(col))
    x <- dplyr::pull(data, !!col)
    x <- stringr::str_split(x, sep)
    x <- purrr::map_int(x, length)

    max(x)
  }

separate_cols_max <-
  function(data, col = "name", sep = "\\.", n_cols_sep = NULL, ..., fill = "right") {

    # Reference: <https://github.com/tidyverse/tidyr/blob/master/R/gather.R>
    data <- .validate_coerce_data(data)

    col <- rlang::as_string(ensym2(col))
    .validate_col(data, col)
    .validate_chr(sep)

    if(is.null(n_cols_sep)) {
      n_cols_sep <-
        get_cols_sep(data = data, col = col, sep = sep)

    }
    stopifnot(is.integer(n_cols_sep))

    nms_sep <-
      paste0(col, seq(1L, n_cols_sep, by = 1L))

    tidyr::separate(data, !!col, into = nms_sep, sep = sep, fill = fill, ...)
  }

convert_list_to_tbl <-
  function(x, ...) {
    .validate_list(x)
    tibble::enframe(x = unlist(x), ...)
  }

unlist_tidily <-
  function(x, ..., name = "name", value = "value") {
    data <- convert_list_to_tbl(x = x, name = name, value = value)
    separate_cols_max(data, col = name, ...)
  }
