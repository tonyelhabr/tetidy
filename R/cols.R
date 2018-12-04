
# Source: personal `{sportspredict}` project.

reorder_one_of_at <-
  function(.data, cols_order) {
    stopifnot(is.data.frame(.data))
    stopifnot(is.character(cols_order), all(cols_order %in% names(.data)))
    cols <- names(.data)
    cols_in <- intersect(cols_order, cols)
    cols_nin <- setdiff(cols, cols_order)
    # length(cols); length(cols_in); length(cols_nin); length(cols_order)
    # setdiff(cols, c(cols_in, cols_nin))
    cols_fct <- factor(cols, levels = c(cols_in, cols_nin))
    dplyr::select(.data, tidyselect::one_of(levels(cols_fct)))
  }

select_one_of_at <-
  function(.data, cols, cols_order = cols) {
    stopifnot(is.data.frame(.data))
    stopifnot(is.character(cols_order), all(cols_order %in% names(.data)))
    if(cols_order != cols) {
      stopfinot(is.character(cols_order), all(cols_order %in% cols))
    }
    cols_fct <- factor(cols, levels = cols_order)
    dplyr::select(.data, tidyselect::one_of(levels(cols_fct)))
  }

