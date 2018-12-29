
context("summarise")
require("datasets")
require("dplyr")
# testthat::test_file("tests/testthat/test-summarise.R")

testthat::test_that("`summarise_stats()` works as expected with default values", {

  # SE version.
  actual_df <- summarise_stats(mtcars, "mpg")
  actual <- actual_df %>% dplyr::pull(mean)
  expect <- mean(mtcars$mpg)
  testthat::expect_equal(actual, expect)

  # NSE version.
  actual_df <- summarise_stats(mtcars, mpg)
  actual <- actual_df %>% dplyr::pull(mean)
  testthat::expect_equal(actual, expect)

})

testthat::test_that("`summarise_stats()` works with non-default values", {
  actual_df <- summarise_stats(mtcars, mpg, tidy = TRUE, key = "stat", value = "value")
  testthat::expect_equal(ncol(actual_df), 2)
  testthat::expect_equal(names(actual_df)[1], "stat")
})
