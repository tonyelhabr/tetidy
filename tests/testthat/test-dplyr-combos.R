
context("dplyr-combos")
require("datasets")
require("dplyr")
require("tibble")
# testthat::test_file("tests/testthat/test-dplyr-combos.R")

test_that("pull_distinctly() works as expected", {

  actual <- pull_distinctly(mtcars, "carb")
  expect <-
    mtcars %>%
    pull(carb) %>%
    unique() %>%
    sort()
  expect_equal(actual, expect)

  actual <- pull_distinctly(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("arrange_distinctly() works as expected with one column", {

  # debug(arrange_distinctly)
  # undebug(arrange_distinctly)
  actual <- arrange_distinctly(mtcars, "carb")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::distinct(carb) %>%
    dplyr::arrange(carb)
  expect_equal(actual, expect)

  actual <- arrange_distinctly(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("arrange_distinctly() works as expected with more than one column", {

  actual <- arrange_distinctly(mtcars, "carb", "cyl")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::distinct(carb, cyl) %>%
    dplyr::arrange(carb, cyl)
  expect_equal(actual, expect)

  actual <- arrange_distinctly(mtcars, carb, cyl)
  expect_equal(actual, expect)

})


test_that("count_arrange() works as expected with one column", {

  actual <- count_arrange(mtcars, "carb")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::count(carb) %>%
    dplyr::arrange(carb)
  expect_equal(actual, expect)

  actual <- count_arrange(mtcars, carb)
  expect_equal(actual, expect)

})

test_that("count_arrange() works as expected with more than one column", {

  actual <- count_arrange(mtcars, "carb", "cyl")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::count(carb, cyl) %>%
    dplyr::arrange(carb, cyl)
  expect_equal(actual, expect)

  actual <- count_arrange(mtcars, carb, cyl)
  expect_equal(actual, expect)

})
