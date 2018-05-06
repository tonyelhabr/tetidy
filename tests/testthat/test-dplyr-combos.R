
context("dplyr-combos")
require("datasets")
# testthat::test_file("tests/testthat/test-dplyr-combos.R")

test_that("pull_distinctly", {

  actual <- pull_distinctly_at(mtcars, "carb")
  expect <- sort(unique(mtcars$carb))
  expect_equal(actual, expect)

  actual <- pull_distinctly(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("arrange_distinctly", {

  actual <- arrange_distinctly_at(mtcars, "carb")
  expect <- data.frame(carb = sort(unique(mtcars$carb)))
  expect_equal(actual, expect)

  actual <- arrange_distinctly(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("rank_arrange", {

  actual <- rank_arrange_at(mtcars, "mpg")
  expect <- with(mtcars, mtcars[order(-mpg),])
  expect$rnk <- seq(1:nrow(mtcars))
  expect <- expect[,c("rnk", names(mtcars))]
  rownames(expect) <- NULL
  expect_equal(actual, expect)

  actual <- rank_arrange(mtcars, mpg)
  expect_equal(actual, expect)

  actual <- rank_arrange(mtcars, mpg, pretty = FALSE)
  expect <- expect[, c(names(mtcars), c("rnk"))]
  expect_equal(actual, expect)

  actual <- rank_arrange(mtcars, mpg, col_out = "rank", pretty = FALSE)
  names(expect) <- c(names(mtcars), c("rank"))
  expect_equal(actual, expect)

})

test_that("count_arrange", {

  actual <- count_arrange_at(mtcars, "carb")
  expect_df <- with(mtcars, aggregate(mtcars, by = list(carb), FUN = length))
  expect_df <- expect_df[, c("Group.1", "carb")]
  expect_df <- with(expect_df, expect_df[order(Group.1),])
  expect_df$n <- expect_df$carb
  expect_df$carb <- expect_df$Group.1
  expect <- expect_df[, c("carb", "n")]
  expect_equal(actual, expect)

  actual <- count_arrange(mtcars, carb)
  expect_equal(actual, expect)

})

