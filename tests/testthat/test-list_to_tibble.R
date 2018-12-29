
context("list-to-tibble")
require("datasets")
# testthat::test_file("tests/testthat/test-list_to_tibble.R")

testthat::test_that("`separate_cols_max()` (and others) works as expected with default values", {
  x <-
    list(
      a = TRUE,
      b = "data",
      c = 42,
      d = 3.14
    )
  actual_df1 <- convert_list_to_tbl(x)
  actual_df_sep1 <- separate_cols_max(actual_df1, name)
  actual_df_sep2 <- unlist_tidily(x)
  testthat::expect_equal(actual_df_sep1, actual_df_sep2)
})
