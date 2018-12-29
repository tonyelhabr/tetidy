
# Should definitely iimplement type-checking more like `{dplyr}`'s, as demonstrated
# mostly in:
# - https://github.com/tidyverse/dplyr/blob/master/R/utils-replace-with.R
# - https://github.com/tidyverse/dplyr/blob/master/R/error.R

# Examples:
# For `tibble`s, see
# https://github.com/tidyverse/dplyr/blob/master/R/sample.R.
# sample_n.default <- function(tbl, size, replace = FALSE, weight = NULL,
#                              .env = parent.frame()) {
#   bad_args("tbl", "must be a data frame, not {friendly_type_of(tbl)}")
# }

# For type, see
#  https://github.com/tidyverse/dplyr/blob/master/R/bind.r
# if (!(is_string(.id))) {
#   bad_args(".id", "must be a scalar string, ",
#            "not {friendly_type_of(.id)} of length {length(.id)}"
#   )
# }

# For types (and lengths), see
# https://github.com/tidyverse/dplyr/blob/master/R/join.r
# check_suffix <- function(x) {
#   if (!is.character(x) || length(x) != 2) {
#     bad_args("suffix", "must be a character vector of length 2, ",
#              "not {friendly_type_of(x)} of length {length(x)}"
#     )
#   }
#
#   if (any(is.na(x))) {
#     bad_args("suffix", "can't be NA")
#   }
#
#   if (all(x == "")) {
#     bad_args("suffix", "can't be empty string for both `x` and `y` suffixes")
#   }
#
#   list(x = x[[1]], y = x[[2]])
# }

.validate_data <- function(data) {
  stopifnot(is.data.frame(data))
}

.coerce_data <- function(data) {
  tibble::as_tibble(data)
}

.validate_coerce_data <- function(data) {
  .validate_data(data)
  .coerce_data(data)
}

.validate_col <- function(data, col, nm = deparse(substitute(col))) {
  stopifnot(is.character(col))
  stopifnot(length(col) == 1)
  stopifnot(length(intersect(names(data), col)) == 1)
}

.validate_cols <- function(data, cols) {
  stopifnot(is.character(cols))
  stopifnot(all(cols %in% names(data)))
}

.validate_col_out <- function(data, col, nm = deparse(substitute(col))) {
  stopifnot(is.character(col))
  stopifnot(length(col) == 1)
  stopifnot(length(intersect(names(data), col)) == 0)
}

.validate_lgl <- function(x, nm = deparse(substitute(x))) {
  # stopifnot(is.logical(x))
  if(!rlang::is_logical(x)) {
    rlang::abort(sprintf("`%s` should be logical.", nm))
  }
  invisible(x)
}

.validate_chr <- function(x) {
  stopifnot(is.character(x))
}

.validate_list <- function(x) {
  stopifnot(is.list(x))
}

