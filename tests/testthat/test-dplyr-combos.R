
context("dplyr-combos")
require("datasets")
# testthat::test_file("tests/testthat/test-[file].R")

test_that("pull_distinctly() works as expected", {

  actual <- pull_distinctly(mtcars, "carb")
  expect <- sort(unique(mtcars$carb))
  expect_equal(actual, expect)

  actual <- pull_distinctly(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("arrange_distinctly() works as expected", {

  actual <- arrange_distinctly(mtcars, "carb")
  expect <- tibble::as_tibble(carb = sort(unique(mtcars$carb)))
  expect_equal(actual, expect)

  actual <- arrange_distinctly(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("count_arrange() works as expected", {

  actual <- count_arrange(mtcars, "carb")
  expect <- tibble::as_tibble(mtcars)
  expect_ <- with(expect, aggregate(expect, by = list(carb), FUN = length))
  expect <- expect_[, c("Group.1", "carb")]
  expect <- with(expect, expect[order(Group.1),])
  expect$n <- expectf$carb
  expect$carb <- expect$Group.1
  expect <- expect[, c("carb", "n")]
  expect_equal(actual, expect)

  actual <- count_arrange(mtcars, carb)
  expect_equal(actual, expect)

})

