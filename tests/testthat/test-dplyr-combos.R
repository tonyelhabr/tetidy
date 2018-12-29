
context("dplyr-combos")
require("datasets")
require("dplyr")
require("tibble")
# testthat::test_file("tests/testthat/test-dplyr-combos.R")

testthat::test_that("`pull_distinctly()` works as expected", {

  # SE version.
  actual <- pull_distinctly(mtcars, "carb")
  expect <-
    mtcars %>%
    dplyr::pull(carb) %>%
    unique() %>%
    sort()
  testthat::expect_equal(actual, expect)

  # NSE version.
  actual <- pull_distinctly(mtcars, carb)
  testthat::expect_equal(actual, expect)

})

testthat::test_that("`arrange_distinctly()` works as expected with one column", {

  # SE version.
  actual <- arrange_distinctly(mtcars, "carb")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::distinct(carb) %>%
    dplyr::arrange(carb)
  testthat::expect_equal(actual, expect)

  # NSE version.
  actual <- arrange_distinctly(mtcars, carb)
  testthat::expect_equal(actual, expect)

})

testthat::test_that("`arrange_distinctly()` works as expected with more than one column", {

  # SE version.
  actual <- arrange_distinctly(mtcars, "carb", "cyl")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::distinct(carb, cyl) %>%
    dplyr::arrange(carb, cyl)
  testthat::expect_equal(actual, expect)

  # NSE version.
  actual <- arrange_distinctly(mtcars, carb, cyl)
  testthat::expect_equal(actual, expect)

})


testthat::test_that("`count_arrange()` works as expected with one column", {

  # SE version.
  actual <- count_arrange(mtcars, "carb")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::count(carb) %>%
    dplyr::arrange(carb)
  testthat::expect_equal(actual, expect)

  # NSE version.
  actual <- count_arrange(mtcars, carb)
  testthat::expect_equal(actual, expect)

})

testthat::test_that("`count_arrange()` works as expected with more than one column", {

  # SE version.
  actual <- count_arrange(mtcars, "carb", "cyl")
  expect <-
    mtcars %>%
    tibble::as_tibble() %>%
    dplyr::count(carb, cyl) %>%
    dplyr::arrange(carb, cyl)
  testthat::expect_equal(actual, expect)

  # NSE version.
  actual <- count_arrange(mtcars, carb, cyl)
  testthat::expect_equal(actual, expect)

})
