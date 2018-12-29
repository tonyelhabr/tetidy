
context("list-to-tibble")
require("datasets")
# testthat::test_file("tests/testthat/test-[file].R")

test_that("separate_cols_max() works as expected", {
  config <- config::get(file = "tests/testthat/config.yml")
  actual_df1 <- convert_list_to_tbl(config)
  actual_df_sep1 <- separate_cols_max(actual_df1, name)
  actual_df_sep2 <- unlist_tidily(config)
  expect_equal(actual_df_sep1, actual_df_sep2)
})
