
context("summarise")
require("datasets")

test_that("summarise", {

  actual_df <- summarise_stats_at(mtcars, "mpg")
  actual <- c(as.matrix(actual_df["mean"]))
  expect <- mean(mtcars$mpg)
  expect_equal(actual, expect)

  actual_df <- summarise_stats(mtcars, mpg)
  actual <- c(as.matrix(actual_df["mean"]))
  expect_equal(actual, expect)


  actual_df <- summarise_stats(mtcars, mpg, tidy = TRUE)
  expect_equal(ncol(actual_df), 2)

  expect_equal(names(actual_df)[1], "stat")

  actual_df <- summarise_stats_by_at(mtcars, col = "mpg", cols_grp = c("cyl"), tidy = TRUE)
  expect_equal(ncol(actual_df), 3)

  actual_df <- summarise_stats_by_at(mtcars, col = "mpg", cols_grp = c("cyl", "gear"), tidy = TRUE)
  expect_equal(ncol(actual_df), 4)

})
