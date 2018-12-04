
context("list-to-tibble")
require("datasets")
# testthat::test_file("tests/testthat/test-[file].R")

test_that("separate_cols_max() works as expected", {
  config <- config::get(file = "tests/testthat/config.yaml")

  # config <-
  #   list(
  #
  #     yaml =
  #       list(
  #         first = "third",
  #         second = -1L,
  #         third = c("apple", "banana"),
  #         fourth = 1L:2L,
  #         fifth = list(a = 1L, b = 2L),
  #         sixth = list(x = 3L, y = 4L:5L, z = list(za = 6.1, zb = 6.2))
  #       ),
  #     argparser =
  #       list(
  #         dummy = "nothing"
  #       )
  #   )
  actual_df1 <- convert_list_to_tbl(config)
  actual_df_sep1 <- separate_cols_max(actual_df1, name)
  actual_df_sep2 <- unlist_tidily(config)
  expect_equal(actual_df_sep1, actual_df_sep2)

  # argv_defaults <-
  #   actual_df_sep2 %>%
  #   dplyr::filter(name1 == "argparser", name2 == "default") %>%
  #   dplyr::select(name3, value) %>%
  #   tibble::deframe()
  # argv_defaults
  # argv_defaults[["required"]]
  # argv_defaults["type"]

  .get_default_arg <- function(lst, var) {
    # if(var %in% names(lst)) {
    if(length(lst[[var]]) == 1L) {
      return(lst[[var]])
    } else {
      return(NA)
    }
  }
  default_required <- .get_default_arg(config$argparser, "required")
  .get_default_arg(config$argparser, "dummy")
  .traverse_arg <- function(lst, arg, ..., required = default_rquired, ) {
    if(is.list(lst[[arg]])) {
      sublst <- lst[[arg]]
    } else {
      val <- lst[[arg]]
      required <- .get_default_arg(
    }

  }
  is.list(config$yaml$n_core)
  is.list(config$yaml$dir_data)
  p <- argparser::arg_parser(
    description = "Description",
    name = "Name"
  )

  for(x in names(config)) {
    # message(x)
    # message(class(x))
    message(config[[x]])
    p <-
      argparser::add_argument(
        p,
        arg = paste0("--", x),
        help = "", # deparse(substitute(x))
        default = config[x]
      )
  }
  argparser::parse_args(p)
}
