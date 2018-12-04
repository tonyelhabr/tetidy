
# Source: personal `{sportspredict}` project.

reorder_one_of_at <-
  function(data, cols_order) {
    .validate_data(data)
    .validate_cols(data, cols_order)

    cols <- names(data)
    cols_in <- intersect(cols_order, cols)
    cols_nin <- setdiff(cols, cols_order)
    # length(cols); length(cols_in); length(cols_nin); length(cols_order)
    # setdiff(cols, c(cols_in, cols_nin))
    cols_fct <- factor(cols, levels = c(cols_in, cols_nin))
    dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
  }

select_one_of_at <-
  function(data, cols, cols_order = cols) {
    .validate_data(data)
    .validate_cols(data, cols)

    if(cols_order != cols) {
      .validate_cols(data, cols_order)
    }
    cols_fct <- factor(cols, levels = cols_order)
    dplyr::select(data, tidyselect::one_of(levels(cols_fct)))
  }

