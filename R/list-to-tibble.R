
# Source: personal `{sportspredict}` project.

.get_cols_max_at <-
  function(data, col, sep) {
    # Don't validate arguments since this should only be called from `.separate_cols_max_at()`.
    col <- rlang::sym(col)
    x <- dplyr::pull(data, !!col)
    x <- stringr::str_split(x, sep) %>%
    x <- purrr::map_int(x, length) %>%
    max(x)
  }

separate_cols_max_impl <-
  function(data, col = "name", sep, n_cols_max = NULL, ..., .fill = "right") {
    stopifnot(is.data.frame(data))
    stopifnot(is.character(col), length(col) == 1L, any(col == names(data)))
    stopifnot(is.character(sep))
    col <- rlang::sym(col)
    if(is.null(n_cols_max)) {
      n_cols_max <-
        .get_cols_max_at(data = data, col = col, sep = sep)

    }
    stopifnot(is.integer(n_cols_max))
    nms_sep <-
      paste0(.col, seq(1L, n_cols_max, by = 1L))
    tidyr::separate(data, !!col, into = nms_sep, sep = sep, fill = .fill, ...)
  }

separate_cols_max_at <-
  function(.data, ...) {
    separate_cols_max_impl(data = .data, ...)
  }

separate_cols_max <-
  function(.data, ...) {
    separate_cols_max_impl(data = .data, ...)
  }

.convert_list_to_tbl <-
  function(.x, ...) {
    stopifnot(is.list(.x))
    tibble::enframe(x = unlist(.x), ...)
  }

unnest_list_tidily <-
  function(.x, ..., .name = NULL, .value = NULL) {
    if(is.null(.name)) {
      .name <- "name"
    }
    if(is.null(.value)) {
      .value <- "value"
    }
    tbl <- .convert_list_to_tbl(x = .x, name = .name, value = .value)
    separate_cols_max_at(tbl, col = .name, ...)
  }
