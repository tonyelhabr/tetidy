
context("summarise")
require("datasets")

test_that("summarise_stats() works with string `col`", {
  actual_df <- summarise_stats(mtcars, "mpg")
  actual <- c(as.matrix(actual_df["mean"]))
  expect <- mean(mtcars$mpg)
  expect_equal(actual, expect)
})

test_that("summarise_stats() works with symbol `col`", {
  actual_df <- summarise_stats(mtcars, mpg)
  actual <- c(as.matrix(actual_df["mean"]))
  expect_equal(actual, expect)
})

test_that("summarise_stats() works with `tidy = TRUE`", {
  actual_df <- summarise_stats(mtcars, mpg, tidy = TRUE)
  expect_equal(ncol(actual_df), 2)
  expect_equal(names(actual_df)[1], "stat")
})
